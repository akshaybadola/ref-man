import os
import json
import requests
import argparse
from queue import Queue
from threading import Thread
from flask import Flask, request, Response
from werkzeug import serving

from arxiv import arxiv_get, arxiv_fetch, arxiv_helper
from dblp import dblp_fetch, dblp_helper
from semantic_scholar import load_ss_cache, semantic_scholar_search, semantic_scholar_paper_details


app = Flask(__name__)


def post_json_wrapper(request, fetch_func, helper, batch_size, verbose):
    """flask `request`, `fetch_func` fetches from the server, helper is a
    q_helper of """
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
    ss_cache = load_ss_cache(args.data_dir)
    proxies = {'http': 'http://127.0.0.1:10102',
               'https': 'http://127.0.0.1:10102'}

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
            if "force" in request.args:
                force = True
            else:
                force = False
            return semantic_scholar_paper_details(id_type, id, args.data_dir, ss_cache, force)
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
            args = dict((k, False if v.lower() == "false" else v)
                        for k, v in request.json.items())
            query = args.pop("q")
            return semantic_scholar_search(query, **args)

    @app.route("/fetch_proxy")
    def fetch_proxy():
        if "url" in request.args and request.args["url"]:
            url = request.args["url"]
        else:
            return json.dumps("NO URL GIVEN or BAD URL")
        if args.verbose:
            print(f"Fetching {url} with proxies {proxies}")
        response = requests.get(url, proxies=proxies)
        if response.url != url:
            return json.dumps({"redirect": response.url,
                               "content": response.content.decode('utf-8')})
        else:
            return Response(response.content)

    @app.route("/version", methods=["GET"])
    def version():
        return "ref-man python server 0.2.0"

    # CHECK: Why are the interfaces to _dblp_helper and arxiv_helper different?
    #        Ideally there should be a specification
    # _dblp_helper = partial(q_helper, _dblp_success, _dblp_no_result,
    #                        _dblp_error)
    # _dblp_helper = QHelper(_dblp_success, _dblp_no_result, _dblp_error)
    _dblp_helper = dblp_helper(True)
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
