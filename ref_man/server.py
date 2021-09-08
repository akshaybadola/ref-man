from typing import Callable, List, Dict, Optional
import os
from pathlib import Path
import json
import time
import logging
import requests
from queue import Queue
from threading import Thread, Event
import flask
from flask import Flask, request, Response
from werkzeug import serving

import re
import operator
from bs4 import BeautifulSoup

from common_pyutil.log import get_stream_logger

from .const import default_headers, __version__
from .arxiv import arxiv_get, arxiv_fetch, arxiv_helper
from .dblp import dblp_helper
from .semantic_scholar import SemanticSearch, load_ss_cache, semantic_scholar_paper_details
from .cache import CacheHelper


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


def parallel_fetch(urls: List[str], fetch_func: Callable[[str, Queue], None],
                   batch_size: int):
    def helper(q: Queue):
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
        q: Queue = Queue()
        threads = []
        for url in _urls:
            threads.append(Thread(target=fetch_func, args=[url, q]))
            threads[-1].start()
        for t in threads:
            t.join()
        content.update(helper(q))
        j += 1


def post_json_wrapper(request: flask.Request, fetch_func: Callable[[str, Queue], None],
                      helper: Callable, batch_size: int, host: str,
                      logger: logging.Logger):
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
    verbose = True
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
                                  kwargs={"verbose": verbose}))
            threads[-1].start()
        for t in threads:
            t.join()
        content.update(helper(q))
        j += 1
    return json.dumps(content)


def check_proxy(proxies: Dict[str, str], flag: Event):
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


class Server:
    """*ref-man* server for network requests.

    We use a separate python process for efficient (and sometimes parallel)
    fetching of network requests.

    Args:
        host: host on which to bind
        port: port on which to bind
        proxy_port: Port for the proxy server. Used by :code:`fetch_proxy`, usually for PDFs.
        proxy_everything: Whether to fetch all requests via proxy.
        proxy_everything_port: Port for the proxy server on which everything is proxied.
                               Used by supported methods.
        data_dir: Directory where the Semantic Scholar Cache is stored.
                  See :func:`load_ss_cache`
        local_pdfs_dir: Local directory where the pdf files are stored.
        remote_pdfs_dir: Remote directory where the pdf files are stored.
        remote_links_cache: File mapping local pdfs to remote links.
        batch_size: Number of parallel requests to send in case parallel requests is
                    implemented for that method.
        chrome_debugger_path: Path for the chrome debugger script.
                              Used to validate the Semantic Scholar search api, as
                              the params can change sometimes. If it's not given then
                              default params are used and the user must update the params
                              in case of an error.
        verbosity: Verbosity control
        threaded: Start the flask server in threaded mode. Defaults to :code:`True`.

    :code:`remote_pdfs_dir` has to be an :code:`rclone` path and the pdf files from
    :code:`local_pdfs_dir` is synced to that with :code:`rclone`.

    """
    def __init__(self, host: str, port: int, proxy_port: int, proxy_everything: bool,
                 proxy_everything_port: int, data_dir: Path, local_pdfs_dir: Path,
                 remote_pdfs_dir: str, remote_links_cache: Path, batch_size: int,
                 chrome_debugger_path: Path, verbosity: str, threaded: bool):
        self.host = "127.0.0.1"
        self.port = port
        self.batch_size = batch_size
        self.data_dir = data_dir
        self.proxy_port = proxy_port
        self.proxy_everything = proxy_everything
        self.proxy_everything_port = proxy_everything_port
        self.chrome_debugger_path = chrome_debugger_path
        self.config_dir = Path.home().joinpath(".config", "ref-man")
        self.verbosity = verbosity
        self.threaded = threaded
        # We set "error" to warning
        verbosity_levels = {"info", "error", "debug"}
        if self.verbosity not in verbosity_levels:
            self.verbosity = "info"
            self.logger = get_stream_logger("ref_man_logger", log_level=self.verbosity)
            self.logger.warning(f"{self.verbosity} was not in known levels." +
                                f"Set to {self.verbosity}")
        else:
            self.logger = get_stream_logger("ref_man_logger", log_level=self.verbosity)
            self.logger.debug(f"Log level is set to {self.verbosity}.")
        # NOTE: This soup stuff should be separate buffer
        if not self.config_dir.exists():
            os.makedirs(self.config_dir)
        self.cvf_files = [os.path.join(self.config_dir, f)
                          for f in os.listdir(self.config_dir)
                          if re.match(r'^(cvpr|iccv)', f.lower())]
        self.soups = {}
        self.logger.debug(f"Loading CVF soups.")
        for cvf in self.cvf_files:
            match = re.match(r'^(cvpr|iccv)(.*?)([0-9]+)',
                             Path(cvf).name, flags=re.IGNORECASE)
            if match:
                venue, _, year = map(str.lower, match.groups())
                with open(cvf) as f:
                    self.soups[(venue, year)] = BeautifulSoup(f.read(), features="lxml")
            else:
                self.logger.error(f"Could not load file {cvf}")
        self.logger.debug(f"Loaded conference files {self.soups.keys()}")

        self.ss_cache = load_ss_cache(self.data_dir)
        self.update_cache_run = None
        if local_pdfs_dir and remote_pdfs_dir and remote_links_cache:
            self.cache_helper: Optional[CacheHelper] =\
                CacheHelper(local_pdfs_dir, remote_pdfs_dir,
                            remote_links_cache, self.logger)
        else:
            self.cache_helper = None
            self.logger.warn("All arguments required for pdf cache not given.\n" +
                             "Will not maintain remote pdf links cache.")

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

    def logi(self, msg: str) -> str:
        self.logger.info(msg)
        return msg

    def logd(self, msg: str) -> str:
        self.logger.debug(msg)
        return msg

    def logw(self, msg: str) -> str:
        self.logger.warn(msg)
        return msg

    def loge(self, msg: str) -> str:
        self.logger.error(msg)
        return msg

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
            self.proxies: Optional[Dict[str, str]] = None
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
            if url.startswith("http:") or response.url.startswith("https:"):
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
                return self.loge("No url given to check")
            else:
                url = request.args["url"]
                progress = self.get.progress(url)
                if progress:
                    return progress
                else:
                    return self.loge("No such url: {url}")

        @app.route("/update_links_cache")
        def update_links_cache():
            if not self.cache_helper:
                return self.loge("Cache helper is not available.")
            if not self.update_cache_run:
                self.update_cache_run = True
            if self.cache_helper.updating:
                return "Still updating cache from previous call"
            files = self.cache_helper.cache_needs_updating
            if files:
                self.cache_helper.update_cache()
                return self.logi(f"Updating cache for {len(files)} files")
            else:
                return self.logi("Nothing to update")

        @app.route("/force_stop_update_cache")
        def foce_stop_update_cache():
            if not self.cache_helper:
                return self.loge("Cache helper is not available.")
            if not self.update_cache_run:
                return self.logi("Update cache was never called")
            else:
                self.cache_helper.stop_update()
                return self.logi("Sent signal to stop updating cache")

        @app.route("/cache_updated")
        def cache_updated():
            if not self.cache_helper:
                return self.loge("Cache helper is not available.")
            if not self.update_cache_run:
                return self.logi("Update cache was never called.")
            elif self.cache_helper.updating:
                return self.logi("Still updating cache")
            elif self.cache_helper.finished:
                return self.logi("Updated cache for all files")
            elif self.cache_helper.finished_with_errors:
                return self.logi("Updated cache with errors.")
            else:
                return self.logi("Nothing was updated in last call to update cache")

        @app.route("/check_proxies")
        def check_proxies():
            return self.check_proxies()

        @app.route("/get_cvf_url", methods=["GET"])
        def get_cvf_url():
            """Get CVPR or ICCV Pdf url.
            """
            if "title" not in request.args:
                return self.loge("Error. Title not in request")
            else:
                title = request.args["title"]
            if "venue" not in request.args:
                return self.loge("Error. Venue not in request")
            else:
                venue = request.args["venue"].lower()
            try:
                if "year" in request.args:
                    year = request.args["year"]
                else:
                    year = None
            except Exception:
                year = None
            if year:
                soup_keys = [(v, y) for v, y in self.soups.keys() if v == venue and y == year]
            else:
                soup_keys = [(v, y) for v, y in self.soups.keys() if v == venue]
            soups = []
            for k in soup_keys:
                soups.extend(self.soups[k].find_all("a"))
            if soups:
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
            else:
                return f"{title} not found for {venue} in {year}"

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
            if self.cache_helper:
                self.logd("Shutting down cache helper.")
                self.cache_helper.shutdown()
            func = request.environ.get('werkzeug.server.shutdown')
            func()
            return self.logi("Shutting down")

    def run(self):
        "Run the server"
        serving.run_simple(self.host, self.port, app, threaded=self.threaded)
