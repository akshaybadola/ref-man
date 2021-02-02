import json
import requests

from .q_helper import QHelper


class _DBLPHelper:
    """Private class which handles the various results of the query. Should make an
    ABC here maybe. It's just to isolate a couple of variables and functions.

    """

    verbose = False
    proxies = None

    @classmethod
    def dblp_fetch(cls, query, q, ret_type="json", verbose=None):
        """Fetch `query` from the dblp server and store the response in
        :class:queue.Queue `q`.

        `ret_type` is the format in to query the server. Valid values can be `json`
        and `xml`. `xml` isn't implemented right now.

        """
        if cls.verbose:
            print(f"Fetching from DBLP, query: {query}\n")
        if ret_type == "json":
            response = requests.get(f"https://dblp.uni-trier.de/search/publ/api" +
                                    f"?q={query}&format=json",
                                    proxies=cls.proxies)
            q.put((query, response))
        else:
            q.put((query, "INVALID"))

    @classmethod
    def _dblp_success(cls, query, response, content):
        """Handle HTTP status 202 (success) for `query` from DBLP server.

        `response` is the response and `content` is the dictionary where all the
        results are finally stored.

        """
        result = json.loads(response.content)["result"]
        if result and "hits" in result and "hit" in result["hits"]:
            content[query] = []
            for hit in result["hits"]["hit"]:
                info = hit["info"]
                authors = info["authors"]["author"]
                if isinstance(authors, list):
                    info["authors"] = [x["text"] for x in info["authors"]["author"]]
                else:
                    info["authors"] = [authors["text"]]
                content[query].append(info)
        else:
            # if cls.verbose:
            #     print("Do we get here at success and no result?")
            content[query] = ["NO_RESULT"]

    @classmethod
    def _dblp_no_result(cls, query, response, content):
        """Handle HTTP status 422 (no result) for `query` from DBLP server.

        `response` is the response and `content` is the dictionary where all the
        results are finally stored.

        """
        content[query] = ["NO_RESULT"]

    @classmethod
    def _dblp_error(cls, query, response, content):
        """Handle any other HTTP status (aside from 200 and 422) for `query` from DBLP
        server.

        `response` is the response and `content` is the dictionary where all the
        results are finally stored.

        """
        if cls.verbose:
            content[query] = [f"ERROR, {response.content}"]
        else:
            content[query] = [f"ERROR"]


def dblp_helper(proxies=None, verbose=False):
    _DBLPHelper.proxies = proxies
    _DBLPHelper.verbose = verbose
    return _DBLPHelper.dblp_fetch, QHelper(_DBLPHelper._dblp_success,
                                           _DBLPHelper._dblp_no_result,
                                           _DBLPHelper._dblp_error, verbose)
