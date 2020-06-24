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


def save_data(data, data_dir, ss_cache, acl_id):
    with open(os.path.join(data_dir, data["paperId"]), "w") as f:
        json.dump(data, f)
    c = [acl_id if acl_id else "",
         data["doi"] if data["doi"] else "",
         data["arxivId"] if data["arxivId"] else "",
         str(data["corpusId"]), data["paperId"]]
    ss_cache["acl"][c[0]] = c[-1]
    ss_cache["arxiv"][c[1]] = c[-1]
    ss_cache["corpus"][c[2]] = c[-1]
    ss_cache["doi"][c[3]] = c[-1]
    with open(os.path.join(data_dir, "metadata"), "a") as f:
        f.write(",".join(c) + "\n")
    print("Updated metadata")


def semantic_scholar_paper_details(id_type, id, data_dir, ss_cache):
    urls = {"ss": f"https://api.semanticscholar.org/v1/paper/{id}",
            "doi": f"https://api.semanticscholar.org/v1/paper/{id}",
            "arxiv": f"https://api.semanticscholar.org/v1/paper/arXiv:{id}",
            "acl": f"https://api.semanticscholar.org/v1/paper/ACL:{id}",
            "corpus": f"https://api.semanticscholar.org/v1/paper/CorpusID:{id}"}
    if id_type not in urls:
        return json.dumps("INVALID ID TYPE")
    else:
        if id_type in {"doi", "acl", "arxiv", "corpus"} and\
           id in ss_cache[id_type] and ss_cache[id_type][id]:
            print(f"Fetching from cache for {id_type}, {id}")
            with open(os.path.join(data_dir, ss_cache[id_type][id])) as f:
                return json.load(f)
        else:
            acl_id = ""
            if id_type == "acl":
                acl_id = id
            print(f"Data not in cache for {id_type}, {id}. Fetching")
            url = urls[id_type] + "?include_unknown_references=true"
            response = requests.get(url)
            save_data(json.loads(response.content), data_dir, ss_cache, acl_id)
            return response.content  # already JSON


def semantic_scholar_search(query, title_only=False, authors=[], cs_only=True,
                            pub_types=[], has_github=False, year_filter=None):
    """
    pub_types can be ["Conference", "JournalArticle"]
    year_filter has to be a :class:`dict` of type {"max": 1995, "min": 1990}
    """
    if year_filter and not ("min" in year_filter and "max" in year_filter and
                            year_filter["max"] > year_filter["min"]):
        print("Invalid Year Filter. Disabling.")
        year_filter = None
    params = {'authors': authors,
              'coAuthors': [],
              'externalContentTypes': [],
              'page': 1,
              'pageSize': 10,
              'performTitleMatch': title_only,
              'publicationTypes': pub_types,
              'queryString': query,
              'requireViewablePdf': False,
              'sort': 'relevance',
              'useRankerService': True,
              'venues': [],
              'yearFilter': year_filter}
    if cs_only:
        params['fieldOfStudy'] = 'computer-science'
    if has_github:
        params['externalContentTypes'] = ["githubReference"]
    headers = {'User-agent': 'Mozilla/5.0', 'Origin': 'https://www.semanticscholar.org'}
    print(f"Sending request to semanticscholar search with query: {query} and params {params}")
    response = requests.post("https://www.semanticscholar.org/api/1/search",
                             headers=headers, json=params)
    if response.status_code == 200:
        results = json.loads(response.content)["results"]
        print(f"Got num results {len(results)} for query: {query}")
        return response.content  # already json
    else:
        return json.dumps(f"ERROR for {query}, {response.content}")


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
    with open(os.path.join(args.data_dir, "metadata")) as f:
        _cache = [*filter(None, f.read().split("\n"))]
    ss_cache = {"acl": {}, "doi": {}, "arxiv": {}, "corpus": {}}
    for _ in _cache:
        c = _.split(",")
        ss_cache["acl"][c[0]] = c[-1]
        ss_cache["arxiv"][c[1]] = c[-1]
        ss_cache["corpus"][c[2]] = c[-1]
        ss_cache["doi"][c[3]] = c[-1]
    print(f"Loaded cache {ss_cache}")

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
            return semantic_scholar_paper_details(id_type, id, args.data_dir, ss_cache)
        else:
            return json.dumps("METHOD NOT IMPLEMENTED")

    @app.route("/semantic_scholar_search", methods=["GET", "POST"])
    def ss_search():
        if request.method == "GET":
            if "q" in request.args and request.args["q"]:
                query = request.args["q"]
            else:
                return json.dumps("NO QUERY GIVEN or EMPTY QUERY")
            return semantic_scholar_search(query)
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
