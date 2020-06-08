import os
import json
import argparse
import requests
from functools import partial
from queue import Queue
from threading import Thread
from flask import Flask, request
from werkzeug import serving

from q_helper import q_helper
from arxiv import arxiv_get, arxiv_fetch, arxiv_helper


def save_data(data, data_dir):
    with open(os.path.join(data_dir, data["paperId"]), "w") as f:
        json.dump(data, f)
    with open(os.path.join(data_dir, "metadata"), "a") as f:
        f.write(",".join([data["arxivId"], str(data["corpusId"]), data["paperId"]]) + "\n")
    # TODO: update arxivId, corpusId indices


def semantic_scholar_paper_details(id_type, id, data_dir):
    urls = {"s2": f"https://api.semanticscholar.org/v1/paper/{id}",
            "doi": f"https://api.semanticscholar.org/v1/paper/{id}",
            "arxiv": f"https://api.semanticscholar.org/v1/paper/arXiv:{id}",
            "acl": f"https://api.semanticscholar.org/v1/paper/ACL:{id}",
            "corpus": f"https://api.semanticscholar.org/v1/paper/CorpusID:{id}"}
    if id_type not in urls:
        return json.dumps("INVALID ID TYPE")
    else:
        response = requests.get(urls[id_type])
        save_data(response.content, data_dir)
        return response.content  # already JSON


def dblp_fetch(query, q, ret_type="json", verbose=False):
    if verbose:
        print(f"Fetching for dblp query {query}\n")
    if ret_type == "json":
        response = requests.request("GET", f"https://dblp.uni-trier.de/search/publ/api" +
                                    f"?q={query}&format=json")
        q.put((query, response))
    else:
        q.put((query, "INVALID"))


def _dblp_success(query, response, content):
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
        content[query] = ["NO_RESULT"]


def _dblp_no_result(query, response, content):
    content[query] = ["NO_RESULT"]


def _dblp_error(query, response, content):
    content[query] = ["ERROR"]


app = Flask(__name__)


def _dblp_helper_old(q):
    content = {}
    while not q.empty():
        query, response = q.get()
        if response.status_code == 200:
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
                content[query] = ["NO_RESULT"]
        elif response.status_code == 422:
            content[query] = ["NO_RESULT"]
        else:
            content[query] = ["ERROR"]
    return content


def post_json_wrapper(request, fetch_func, helper, batch_size, verbose):
    if not isinstance(request.json, str):
        data = request.json
    else:
        try:
            data = json.loads(request.json)
        except Exception:
            return json.dumps("BAD REQUEST")
    j = 0
    content = {}
    while True:
        _data = data[(batch_size * j): (batch_size * (j + 1))].copy()
        for k, v in content.items():
            if v == ["ERROR"]:
                _data.append(k)
        if not _data:
            break
        q = Queue()
        threads = []
        for d in _data:
            threads.append(Thread(target=fetch_func, args=[d, q],
                                  kwargs={"verbose": verbose}))
            threads[-1].start()
        for t in threads:
            t.join()
        content.update(helper(q))
        j += 1
    return json.dumps(content)


def main(args):
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
                                       args.batch_size, args.verbose)
            return json.dumps(result)

    @app.route("/semantic_scholar", methods=["GET", "POST"])
    def semantic_scholar():
        if request.method == "GET":
            if "id" in request.args:
                id = request.args["id"]
            else:
                return json.dumps("NO ID GIVEN")
            if "id_type" in request.args:
                id_type = request.args["id_type"]
            else:
                return json.dumps("NO ID_TYPE GIVEN")
            return semantic_scholar_paper_details(id_type, id, args.data_dir)
        else:
            return json.dumps("METHOD NOT IMPLEMENTED")

    @app.route("/version", methods=["GET"])
    def version():
        return "ref-man python server 0.2.0"

    @app.route("/dblp_old", methods=["POST"])
    def dblp_old():
        if not isinstance(request.json, str):
            data = request.json
        else:
            try:
                data = json.loads(request.json)
            except Exception:
                return json.dumps("BAD REQUEST")
        j = 0
        content = {}
        while True:
            _data = data[(args.batch_size * j): (args.batch_size * (j + 1))].copy()
            for k, v in content.items():
                if v == ["ERROR"]:
                    _data.append(k)
            if not _data:
                break
            q = Queue()
            threads = []
            for d in _data:
                threads.append(Thread(target=dblp_fetch, args=[d, q],
                                      kwargs={"verbose": args.verbose}))
                threads[-1].start()
            for t in threads:
                t.join()
            content.update(_dblp_helper_old(q))
            j += 1
        return json.dumps(content)

    _dblp_helper = partial(q_helper, _dblp_success, _dblp_no_result, _dblp_error)
    @app.route("/dblp", methods=["POST"])
    def dblp():
        result = post_json_wrapper(request, dblp_fetch, _dblp_helper,
                                   args.batch_size, args.verbose)
        return result

    serving.run_simple("127.0.0.1", args.port, app, threaded=args.threaded)


if __name__ == '__main__':
    parser = argparse.ArgumentParser()
    parser.add_argument("--no-threaded", dest="threaded", action="store_false")
    parser.add_argument("--port", "-p", type=int, default=9999)
    parser.add_argument("--data-dir", "-d", type=str, default=os.path.expanduser("~"))
    parser.add_argument("--batch-size", "-b", type=int, default=16)
    parser.add_argument("--verbose", "-v", action="store_true")
    args = parser.parse_args()
    main(args)
