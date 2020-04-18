import json
import argparse
import requests
from queue import Queue
from threading import Thread
from flask import Flask, request
from werkzeug import serving


def dblp_fetch(query, q, ret_type="json", verbose=False):
    if verbose:
        print(f"Fetching for query {query}\n")
    if ret_type == "json":
        response = requests.request("GET", f"https://dblp.uni-trier.de/search/publ/api" +
                                    f"?q={query}&format=json")
        q.put((query, response))
    else:
        q.put((query, "INVALID"))


app = Flask(__name__)


def _helper(q):
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


def main(args):
    @app.route("/", methods=["POST"])
    def main():
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
            content.update(_helper(q))
            j += 1
        return json.dumps(content)
    serving.run_simple("127.0.0.1", args.server_port, app, threaded=args.threaded)


if __name__ == '__main__':
    parser = argparse.ArgumentParser()
    parser.add_argument("--no-threaded", dest="threaded", action="store_false")
    parser.add_argument("--server-port", "-p", type=int, default=9999)
    parser.add_argument("--batch-size", "-b", type=int, default=16)
    parser.add_argument("--verbose", "-v", action="store_true")
    args = parser.parse_args()
    main(args)
