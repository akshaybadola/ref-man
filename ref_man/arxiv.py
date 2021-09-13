from typing import Dict, List, Optional
import json
import requests
from queue import Queue
from functools import partial
from bs4 import BeautifulSoup

from .q_helper import q_helper


# TODO: There should be a cache of entries
def dict_to_bibtex(bib_dict: Dict[str, str], json_out: bool = False) -> Optional[str]:
    temp = bib_dict.copy()
    if "author" in temp:
        k = "author"
    elif "authors" in temp:
        k = "authors"
    else:
        return None
    if isinstance(temp[k], str):
        temp[k].split(" ")[-1].lower() + temp["year"] +\
            temp["title"].split(" ")[0].lower()
    else:
        key = temp[k][0].split(" ")[-1].lower() + temp["year"] +\
            temp["title"].split(" ")[0].lower()
    bib = "@" + temp.pop("type") + "{" + key + "\n"
    for k, v in temp.items():
        if k in {"author", "authors"}:
            if isinstance(v, list):
                authors = [", ".join([_.split(" ")[-1], " ".join(_.split(" ")[:-1])])
                           for _ in v]
                bib += "  author" + "={" + " and ".join(authors) + "},\n"
            elif isinstance(v, str):
                bib += "  author" + "={" + v + "},\n"
        else:
            bib += "  " + k + "={" + v + "},\n"
    bib = bib[:-2]
    bib += "\n}"
    if json_out:
        return json.dumps(bib)
    else:
        return bib


def arxiv_get(arxiv_id: str) -> str:
    """Fetch details of article with arxiv_id from arxiv api.

    Args:
        arxiv_id: The Arxiv ID of the article

    """
    response = requests.get("http://export.arxiv.org/api/query?id_list={arxiv_id}")
    soup = BeautifulSoup(response.content, features="lxml")
    entry = soup.find("entry")
    abstract = entry.find("summary").text
    title = entry.find("title").text
    authors = [a.text for a in entry.find_all("author")]
    date = entry.find("published").text
    bib_dict = {"abstract": abstract.replace("\n", " ").strip(), "title": title,
                "authors": [a.replace("\n", " ").strip() for a in authors], "year": date[:4],
                "url": f"https://arxiv.org/abs/{arxiv_id}", "type": "article"}
    if bib_dict:
        return dict_to_bibtex(bib_dict, True) or ""
    else:
        return json.dumps("ERROR RETRIEVING")


def _arxiv_success(query: str, response: requests.Response,
                   content: Dict[str, str]) -> None:
    soup = BeautifulSoup(response.content, features="lxml")
    entry = soup.find("entry")
    abstract = entry.find("summary").text
    title = entry.find("title").text
    authors = [a.text for a in entry.find_all("author")]
    date = entry.find("published").text
    bib_dict = {"abstract": abstract.replace("\n", " ").strip(), "title": title,
                "authors": [a.replace("\n", " ").strip() for a in authors],
                "year": date[:4],
                "url": f"https://arxiv.org/abs/{query}", "type": "misc"}
    content[query] = dict_to_bibtex(bib_dict) or ""


# FIXME: content has mixed type Dict[str, List[str]] and Dict[str, str]
def _arxiv_no_result(query: str, response: requests.Response,
                     content: Dict[str, List[str]]) -> None:
    content[query] = ["NO_RESULT"]


def _arxiv_error(query: str, response: requests.Response,
                 content: Dict[str, List[str]]) -> None:
    content[query] = ["ERROR"]


def arxiv_fetch(arxiv_id: str, q: Queue, ret_type: str = "json",
                verbose: bool = False):
    if verbose:
        print(f"Fetching for arxiv_id {arxiv_id}\n")
    if ret_type == "json":
        response = requests.get("http://export.arxiv.org/api/query?id_list={arxiv_id}")
        q.put((arxiv_id, response))
    else:
        q.put((arxiv_id, "INVALID"))


arxiv_helper = partial(q_helper, _arxiv_success, _arxiv_no_result, _arxiv_error)
