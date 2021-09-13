from typing import List, Dict, Callable, Optional
from threading import Thread, Event
from queue import Queue
import json
import re
import requests
import time
import logging

from bs4 import BeautifulSoup
import flask

from .const import default_headers
from .q_helper import ContentType


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


def fetch_url_info(url: str) -> Dict[str, str]:
    response = requests.get(url, headers=default_headers)
    if response.status_code == 200:
        soup = BeautifulSoup(response.content)
        title = soup.find("title").text
        if re.match("https{0,1}://arxiv.org.*", url):
            title = soup.find(None, attrs={"class": "title"}).text.split(":")[1]
            authors = soup.find(None, attrs={"class": "authors"}).text.split(":")[1]
            abstract = soup.find(None, attrs={"class": "abstract"}).text.split(":")[1]
            date = soup.find("div", attrs={"class": "dateline"}).text.lower()
            pdf_url: Optional[str] = url.replace("/abs/", "/pdf/")
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
    return retval


def fetch_url_info_parallel(url: str, q: Queue) -> None:
    q.put((url, fetch_url_info(url)))


def post_json_wrapper(request: flask.Request, fetch_func: Callable[[str, Queue], None],
                      helper: Callable[[Queue], ContentType], batch_size: int, host: str,
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
