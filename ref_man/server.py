from typing import Callable, List, Dict, Union, Optional
import os
import sys
import json
import time
import shutil
import logging
import requests
from queue import Queue
from threading import Thread, Event
import flask
from flask import Flask, request, Response
from werkzeug import serving
from subprocess import Popen, PIPE, TimeoutExpired

import re
import operator
from bs4 import BeautifulSoup

from .const import default_headers, __version__
from .arxiv import arxiv_get, arxiv_fetch, arxiv_helper
from .dblp import dblp_helper
from .semantic_scholar import SemanticSearch, load_ss_cache, semantic_scholar_paper_details


def get_stream_logger(name="default",
                      handler_log_level=logging.DEBUG,
                      log_level=logging.DEBUG,
                      datefmt=None, fmt=None):
    if datefmt is None:
        datefmt = '%Y/%m/%d %I:%M:%S %p'
    if fmt is None:
        '%(asctime)s %(message)s'
    logger = logging.getLogger(name)
    formatter = logging.Formatter(datefmt=datefmt, fmt=fmt)
    stream_handler = logging.StreamHandler(sys.stdout)
    stream_handler.setLevel(handler_log_level)
    stream_handler.setFormatter(formatter)
    logger.addHandler(stream_handler)
    logger.setLevel(log_level)
    return logger


app = Flask(__name__)


def fetch_url_info(url, headers, q=None):
    response = requests.get(url, headers=headers)
    if response.status_code == 200:
        soup = BeautifulSoup(response.content)
        title = soup.find("title").text
        if re.match("https{0,1}://arxiv.org.*", url):
            title = soup.find(None, attrs={"class": "title"}).text.split(":")[1]
            authors = soup.find(None, attrs={"class": "authors"}).text.split(":")[1]
            abstract = soup.find(None, attrs={"class": "abstract"}).text.split(":")[1]
            date = soup.find("div", attrs={"class": "dateline"}).text.lower()
            pdf_url = url.replace("/abs/", "/pdf/")
            if "last revised" in date:
                date = date.split("last revised")[1].split("(")[0]
            elif "submitted" in date:
                date = date.split("submitted")[1].split("(")[0]
            else:
                date = None
        else:
            authors = None
            abstract = None
            date = None
            pdf_url = None
        retval = {"title": title and title.strip(),
                  "authors": authors and authors.strip(),
                  "date": date and date.strip(),
                  "abstract": abstract and abstract.strip(),
                  "pdf_url": pdf_url and pdf_url.strip()}
    else:
        retval = {"error": "error", "code": response.status_code}
    if q is not None:
        q.put((url, retval))
    else:
        return retval


def parallel_fetch(urls, fetch_func, batch_size):
    def helper(q):
        responses = {}
        while not q.empty():
            url, retval = q.get()
            responses[url] = retval
        return responses
    j = 0
    content = {}
    while True:
        _urls = urls[(batch_size * j): (batch_size * (j + 1))].copy()
        if not _urls:
            break
        q = Queue()
        threads = []
        for url in _urls:
            threads.append(Thread(target=fetch_func, args=[url, q]))
            threads[-1].start()
        for t in threads:
            t.join()
        content.update(helper(q))
        j += 1


class Get:
    def __init__(self):
        self._finished = {}
        self._progress = {}

    def __call__(self, url, **kwargs) -> Union[bytes, bytearray]:
        self._finished[url] = Event()
        self._progress[url] = 0
        content = bytearray()
        response = requests.get(url, stream=True, **kwargs)
        total = response.headers.get('content-length')
        if total is None:           # no content length header
            return response.content
        else:
            dl = 0
            total_length = int(total)
            for data in response.iter_content(chunk_size=4096):
                content.extend(data)
                dl += 4096
                self._progress[url] = (100 * (dl / total_length))
        self._finished[url].set()
        return content

    def finished(self, url: str) -> Optional[bool]:
        finished = self._finished.get(url)
        if finished:
            return finished.is_set()
        else:
            return None

    def progress(self, url: str) -> Optional[float]:
        return self._progress.get(url)


def post_json_wrapper(request: flask.Request, fetch_func: Callable, helper: Callable,
                      batch_size: int, host: str, logger: logging.Logger):
    """Helper function to parallelize the requests and gather them.

    Args:
        request: An instance :class:`~Flask.Request`
        fetch_func: :func:`fetch_func` fetches the request from the server
        helper: :func:`helper` validates and collates the results
        batch_size: Number of simultaneous fetch requests
        verbosity: verbosity level

    """
    if not isinstance(request.json, str):
        data = request.json
    else:
        try:
            data = json.loads(request.json)
        except Exception:
            return json.dumps("BAD REQUEST")
    logger.info(f"Fetching {len(data)} queries from {host}")
    verbosity = True
    j = 0
    content: Dict[str, str] = {}
    while True:
        _data = data[(batch_size * j): (batch_size * (j + 1))].copy()
        for k, v in content.items():
            if v == ["ERROR"]:
                _data.append(k)
        if not _data:
            break
        q: Queue = Queue()
        threads = []
        for d in _data:
            # FIXME: This should also send the logger instance
            threads.append(Thread(target=fetch_func, args=[d, q],
                                  kwargs={"verbosity": verbosity}))
            threads[-1].start()
        for t in threads:
            t.join()
        content.update(helper(q))
        j += 1
    return json.dumps(content)


def check_proxy(proxies, flag):
    check_count = 0
    while flag.is_set():
        try:
            response = requests.get("http://google.com", proxies=proxies,
                                    timeout=1)
            if response.status_code != 200:
                flag.clear()
            else:
                check_count = 0
        except requests.exceptions.Timeout:
            check_count += 1
            print(f"Proxy failed {check_count} times")
        if check_count > 2:
            flag.clear()
        time.sleep(10)
    print("Proxy failed. Exiting from check.")


def update_links_cache_helper(local_dir, remote_dir, cache_file, ev,
                              success_ev, success_with_errors_ev, logger):
    def try_get_link(remote_path):
        try:
            p = Popen(f"rclone -v link {remote_path}", shell=True, stdout=PIPE, stderr=PIPE)
            out, err = p.communicate(timeout=10)
            if err and "error 403" in err.decode("utf-8").lower():
                status = False
                link = "not_present"
            else:
                link = out.decode("utf-8").replace("\n", "")
                status = True
        except TimeoutExpired:
            logger.warning(f"Timeout while getting link for file {f}")
            link = "timeout"
            status = False
        return status, link

    def copy_file(local_path):
        try:
            p = Popen(f"rclone --no-update-modtime -v copy {local_path} {remote_dir}", shell=True,
                      stdout=PIPE, stderr=PIPE)
            out, err = p.communicate(timeout=10)
            err = err.decode("utf-8").lower()
            if err and ("copied" in err or "transferred" in err):
                logger.debug(f"Copied file {local_path} to remote")
                status = True
            else:
                status = False
        except TimeoutExpired:
            logger.warning(f"Timeout while copying for file {local_path}")
            status = False
        return status

    if not ev.is_set():
        ev.set()
    if success_ev.is_set():
        success_ev.clear()
    if success_with_errors_ev.is_set():
        success_with_errors_ev.clear()
    logger.info(f"Updating local cache {cache_file}")
    init_cache_size = None
    try:
        local_files = [os.path.join(local_dir, f) for f in os.listdir(local_dir)
                       if not f.startswith(".")]
        warnings = []
        with open(cache_file) as f:
            cache = [x for x in f.read().split("\n") if len(x)]
            cached_files = [x.rsplit(";")[0] for x in cache]
            init_cache_size = len(cache)
        files = set(local_files) - set(cached_files)
        for f in files:
            if not ev.is_set():
                break
            try:
                start = time.time()
                remote_path = os.path.join(remote_dir, os.path.basename(f))
                if " " in remote_path:
                    remote_path = f'"{remote_path}"'
                status, link = try_get_link(remote_path)
                if not status:
                    if link == "not_present":
                        logger.warning(f"File {f} does not exist on remote. Copying")
                        status = copy_file(f)
                        if status:
                            status, link = try_get_link(remote_path)
                duration = time.time() - start
                if not status:
                    warnings.append(f"{f}")
                    logger.warning(f"Error occurred for file {f} {link}")
                else:
                    logger.debug(f"got link {link} for file {f} in {duration} seconds")
                    cache.append(f"{f};{link}")
            except Exception as e:
                logger.warning(f"Error occured for file {f} {e}")
        logger.info(f"Writing {len(cache) - init_cache_size} links to {cache_file}")
        shutil.copyfile(cache_file, cache_file + ".bak")
        with open(cache_file, "w") as cf:
            cf.write("\n".join(cache))
        ev.clear()
        if warnings:
            success_with_errors_ev.set()
        else:
            success_ev.set()
    except Exception as e:
        ev.clear()
        logger.error(f"Error {e} while updating cache")
        logger.error(f"Overwritten {cache_file}. Original file backed up to {cache_file}.bak")


class Server:
    """*ref-man* server for network requests.

    We use a separate python process for efficient (and sometimes parallel)
    fetching of network requests.

    FIXME: Should be keyword args instead
    Args:
        args: :class:`types.SimpleNamespace` obtained from :class:`argparse.ArgumentParser`

    host: host on which to bind
    port: port on which to bind
    batch_size: Number of parallel requests to send in case parallel requests is
                implemented for that method
    data_dir: Directory where the Semantic Scholar Cache is stored.
              See :func:`load_ss_cache`
    proxy_port: Port for the proxy server. Used by `fetch_proxy`, usually for PDFs.
    proxy_everything: Whether to fetch all requests via proxy.
    proxy_everything_port: Port for the proxy server on which everything is proxied.
                           Used by supported methods.
    chrome_debugger_path: Path for the chrome debugger script.
                          Used to validate the Semantic Scholar search api, as
                          the params can change sometimes. If it's not given then
                          default params are used and the user must update the params
                          in case of an error.
    verbosity: Verbosity control
    threaded: Start the flask server in threaded mode. Defaults to `True`.

    """
    def __init__(self, args):
        self.host = "127.0.0.1"
        self.port = args.port
        self.batch_size = args.batch_size
        self.data_dir = args.data_dir
        self.proxy_port = args.proxy_port
        self.proxy_everything = args.proxy_everything
        self.proxy_everything_port = args.proxy_everything_port
        self.chrome_debugger_path = args.chrome_debugger_path
        self.verbosity = args.verbosity
        self.threaded = args.threaded
        self.update_cache_thread = None
        # We set "error" to warning
        verbosity_levels = {"info": logging.INFO,
                            "error": logging.WARNING,
                            "debug": logging.DEBUG}
        if self.verbosity not in verbosity_levels:
            self.verbosity = "info"
            self.logger = get_stream_logger(log_level=verbosity_levels[self.verbosity])
            self.logger.warning(f"{args.verbosity} was not in known levels." +
                                f"Set to {self.verbosity}")
        else:
            self.logger = get_stream_logger(log_level=verbosity_levels[self.verbosity])
            self.logger.debug(f"Log level is set to {args.verbosity}.")
        # NOTE: This soup stuff should be separate buffer
        cur_dir = os.path.dirname(os.path.abspath(__file__))
        self.cvpr_files = [os.path.join(cur_dir, f) for f in os.listdir(cur_dir)
                           if f.lower().startswith("cvpr")]
        self.soups = {}
        for f in self.cvpr_files:
            with open(f) as _f:
                self.soups[f] = BeautifulSoup(_f.read(), features="lxml")
        self.logger.debug(f"Loaded conference files {self.soups.keys()}")

        self.ss_cache = load_ss_cache(self.data_dir)
        self.update_cache_run = False
        self.updating_cache_event = Event()
        self.update_success_event = Event()
        self.update_success_with_errors_event = Event()

        # TODO: Maybe start up the proxy from here
        # TODO: Maybe ssh_socks proxy server should also be entirely in python
        #       paramiko maybe? Or some tunnel library
        # if self.proxy_port:
        #     self.logger.info(f"Will redirect fetch_proxy to on {self.proxy_port}")
        #     proxies = {"http": f"http://127.0.0.1:{self.proxy_port}",
        #                "https": f"http://127.0.0.1:{self.proxy_port}"}
        #     # flag = Event()
        #     # flag.set()
        #     # check_proxy_thread = Thread(target=check_proxy, args=[proxies, flag])
        #     # check_proxy_thread.start()
        # else:
        #     proxies = None
        # if self.proxy_everything_port:
        #     self.logger.info(f"Will proxy everything on {self.proxy_everything_port}")
        #     everything_proxies = {"http": f"http://127.0.0.1:{self.proxy_everything_port}",
        #                           "https": f"http://127.0.0.1:{self.proxy_everything_port}"}
        #     flag = Event()
        #     flag.set()
        #     check_proxy_thread = Thread(target=check_proxy, args=[proxies, flag])
        #     check_proxy_thread.start()
        # else:
        #     everything_proxies = None

        self.check_proxies()
        self.semantic_search = SemanticSearch(self.chrome_debugger_path)
        self.init_routes()

    def check_proxies(self) -> str:
        msgs = []
        if self.proxy_everything_port:
            self.everything_proxies = None
            everything_proxies = {"http": f"http://127.0.0.1:{self.proxy_everything_port}",
                                  "https": f"http://127.0.0.1:{self.proxy_everything_port}"}
            try:
                response = requests.get("http://google.com", proxies=everything_proxies,
                                        timeout=1)
                if response.status_code == 200:
                    msg = "Proxy everything seems to work"
                    self.logger.info(msg)
                else:
                    msg = "Proxy everything seems reachable but wrong" +\
                        f" status_code {response.status_code}"
                    self.logger.info(msg)
                self.logger.warning("Warning: proxy_everything is only implemented for DBLP.")
                msg += "Warning: proxy_everything is only implemented for DBLP."
                self.everything_proxies = everything_proxies
            except requests.exceptions.Timeout:
                msg = "Proxy for everything else not reachable"
                self.logger.error(msg)
            msgs.append(msg)
        if self.proxy_port is not None:
            self.proxies = None
            proxies = {"http": f"http://127.0.0.1:{self.proxy_port}",
                       "https": f"http://127.0.0.1:{self.proxy_port}"}
            try:
                response = requests.get("http://google.com", proxies=proxies,
                                        timeout=1)
                if response.status_code == 200:
                    msg = f"Proxy {self.proxy_port} seems to work"
                    self.logger.info(msg)
                    self.proxies = proxies
                else:
                    msg = f"Proxy seems reachable but wrong status_code {response.status_code}"
                    self.logger.info(msg)
            except requests.exceptions.Timeout:
                msg = "Timeout: Proxy not reachable. Will not proxy"
                self.logger.error(msg)
            except requests.exceptions.ProxyError:
                msg = "ProxyError: Proxy not reachable. Will not proxy"
                self.logger.error(msg)
            msgs.append(msg)
        return "\n".join(msgs)

    def init_routes(self):
        @app.route("/arxiv", methods=["GET", "POST"])
        def arxiv():
            if request.method == "GET":
                if "id" in request.args:
                    id = request.args["id"]
                else:
                    return json.dumps("NO ID GIVEN")
                return arxiv_get(id)
            else:
                result = post_json_wrapper(request, arxiv_fetch, arxiv_helper,
                                           self.batch_size, "Arxiv", self.logger)
                return json.dumps(result)

        @app.route("/semantic_scholar", methods=["GET", "POST"])
        def ss():
            if request.method == "GET":
                if "id" in request.args:
                    id = request.args["id"]
                else:
                    return json.dumps("NO ID GIVEN")
                if "id_type" in request.args:
                    id_type = request.args["id_type"]
                else:
                    return json.dumps("NO ID_TYPE GIVEN")
                if "force" in request.args:
                    force = True
                else:
                    force = False
                data = semantic_scholar_paper_details(id_type, id, self.data_dir,
                                                      self.ss_cache, force)
                return data
            else:
                return json.dumps("METHOD NOT IMPLEMENTED")

        @app.route("/semantic_scholar_search", methods=["GET", "POST"])
        def ss_search():
            if "q" in request.args and request.args["q"]:
                query = request.args["q"]
            else:
                return json.dumps("NO QUERY GIVEN or EMPTY QUERY")
            if request.method == "GET":
                return self.semantic_search.semantic_scholar_search(query)
            else:
                if request.json:
                    kwargs = request.json
                else:
                    kwargs = {}
                return self.semantic_search.semantic_scholar_search(query, **kwargs)

        @app.route("/url_info", methods=["GET"])
        def url_info():
            """Fetch info about a given url or urls based on certain rules."""
            if "url" in request.args and request.args["url"]:
                url = request.args["url"]
                urls = None
            elif "urls" in request.args and request.args["urls"]:
                urls = request.args["urls"].split(",")
                url = None
            else:
                return json.dumps("NO URL or URLs GIVEN")
            if urls is not None:
                return parallel_fetch(urls, fetch_url_info, self.batch_size)
            elif url is not None:
                return json.dumps(fetch_url_info(url))
            else:
                return json.dumps("NO URL or URLs GIVEN")

        @app.route("/fetch_proxy")
        def fetch_proxy():
            """Fetch URL with :attr:`self.proxies` if :attr:`self.proxies` is not `None`.
            """
            if "url" in request.args and request.args["url"]:
                url = request.args["url"]
            else:
                return json.dumps("NO URL GIVEN or BAD URL")
            # DEBUG code
            # if url == "https://arxiv.org/pdf/2006.01912":
            #     with os.path.expanduser("~/pdf_file.pdf", "rb") as f:
            #         pdf_data = f.read()
            #     response = make_response(pdf_data)
            #     response.headers["Content-Type"] = "application/pdf"
            #     return response
            self.logger.debug(f"Fetching {url} with proxies {self.proxies}")
            if self.proxies:
                try:
                    response = requests.get(url, headers=default_headers, proxies=self.proxies)
                except requests.exceptions.Timeout:
                    self.logger.error("Proxy not reachable. Fetching without proxy")
                    self.proxies = None
                    response = requests.get(url, headers=default_headers)
                except requests.exceptions.ProxyError:
                    self.logger.error("Proxy not reachable. Fetching without proxy")
                    self.proxies = None
                    response = requests.get(url, headers=default_headers)
            else:
                self.logger.warn("Proxy dead. Fetching without proxy")
                response = requests.get(url, headers=default_headers)
            if url.startswith("http:") and response.url.startswith("https:"):
                return Response(response.content)
            elif response.url != url:
                if response.headers["Content-Type"] in\
                   {"application/pdf", "application/octet-stream"}:
                    return Response(response.content)
                elif response.headers["Content-Type"].startswith("text"):
                    return json.dumps({"redirect": response.url,
                                       "content": response.content.decode("utf-8")})
                else:
                    return json.dumps({"redirect": response.url,
                                       "content": "Error, unknown content from redirect"})
            else:
                return Response(response.content)

        @app.route("/progress")
        def progress():
            if "url" not in request.args:
                return "No url given to check"
            else:
                url = request.args["url"]
                progress = self.get.progress(url)
                if progress:
                    return progress
                else:
                    return "No such url: {url}"

        @app.route("/update_links_cache")
        def update_links_cache():
            if not self.update_cache_run:
                self.update_cache_run = True
            if self.updating_cache_event.is_set():
                return "Still updating cache from previous call"
            local_dir = None
            remote_dir = None
            cache_file = None
            if "local_dir" in request.args:
                local_dir = request.args["local_dir"]
            if "remote_dir" in request.args:
                remote_dir = request.args["remote_dir"]
            if "cache_file" in request.args:
                cache_file = request.args["cache_file"]
            if local_dir and remote_dir and cache_file:
                local_files = [os.path.join(local_dir, f) for f in os.listdir(local_dir)
                               if not f.startswith(".")]
                with open(cache_file) as f:
                    cache = [x for x in f.read().split("\n") if len(x)]
                    cached_files = [x.rsplit(";")[0] for x in cache]
                files = set(local_files) - set(cached_files)
                if files:
                    self.updating_cache_event.set()
                    self.update_cache_thread = Thread(target=update_links_cache_helper,
                                                      args=[local_dir, remote_dir, cache_file,
                                                            self.updating_cache_event,
                                                            self.update_success_event,
                                                            self.update_success_with_errors_event,
                                                            self.logger])
                    self.update_cache_thread.start()
                    return f"Updating cache for {len(files)} files"
                else:
                    return "Nothing to update"
            else:
                return f"Insufficient arguments {local_dir}, {remote_dir}, {cache_file}"

        @app.route("/force_stop_update_cache")
        def foce_stop_update_cache():
            if not self.update_cache_run:
                return "Update cache was never called"
            else:
                self.updating_cache_event.clear()
                return "Sent signal to stop updating cache"

        @app.route("/cache_updated")
        def cache_updated():
            if not self.update_cache_run:
                return "Update cache was never called"
            elif self.updating_cache_event.is_set():
                return "Still updating cache"
            elif self.update_success_event.is_set():
                return "Updated cache for all files"
            elif self.update_success_with_errors_event.is_set():
                return "Updated cache with errors."

        @app.route("/check_proxies")
        def check_proxies():
            return self.check_proxies()

        @app.route("/get_cvpr_url", methods=["GET"])
        def get_cvpr_url():
            if "title" not in request.args:
                return "Error. Title not in request"
            else:
                try:
                    if "year" in request.args:
                        year = int(request.args["year"])
                    else:
                        year = None
                except Exception:
                    year = None
                title = request.args["title"]
            if year:
                soups = self.soups[f"cvpr_{year}"].find_all("a")
            else:
                soups = []
                for v in self.soups.values():
                    soups.extend(v.find_all("a"))
            regexp = ".*" + ".*".join([*filter(None, title.split(" "))][:3])
            matches = [(x, re.match(regexp.lower(), x["href"].lower()))
                       for x in soups
                       if "href" in x.attrs and x["href"].lower().endswith(".pdf")
                       and re.match(regexp.lower(), x["href"].lower())]
            if not matches:
                return f"{title}"
            elif len(matches) == 1:
                href = matches[0][0]["href"]
            else:
                matches.sort(lambda x: operator.abs(operator.sub(*x[1].span())))
                href = matches[-1][0]["href"]
            href = os.path.join("https://openaccess.thecvf.com/", href)
            return f"{title};{href}"

        @app.route("/echo", methods=["GET"])
        def echo():
            if request.args:
                return "\n".join(k + " : " + v for k, v in request.args.items())
            else:
                return "Echo!"

        @app.route("/version", methods=["GET"])
        def version():
            return f"ref-man python server {__version__}"

        # TODO: rest of helpers should also support proxy
        # CHECK: Why are the interfaces to _dblp_helper and arxiv_helper different?
        #        Ideally there should be a single specification
        _proxy = self.everything_proxies if self.proxy_everything else None
        dblp_fetch, _dblp_helper = dblp_helper(_proxy, True)

        @app.route("/dblp", methods=["POST"])
        def dblp():
            """Fetch from DBLP"""
            result = post_json_wrapper(request, dblp_fetch, _dblp_helper,
                                       self.batch_size, "DBLP", self.logger)
            return result

        @app.route("/shutdown")
        def shutdown():
            self.updating_cache_event.clear()
            if self.update_cache_thread is not None:
                self.update_cache_thread.join()
            func = request.environ.get('werkzeug.server.shutdown')
            func()
            return "Shutting down"

    def run(self):
        "Run the server"
        serving.run_simple(self.host, self.port, app, threaded=self.threaded)
