from typing import List, Dict, Any, Union, Optional
import os
import json
import requests
from subprocess import Popen, PIPE
import shlex
import pathlib


assoc = [(x, i) for i, x in enumerate(["acl", "arxiv", "corpus", "doi"])]


def load_ss_cache(data_dir):
    """Load the ss_cache metadata from the disk.

    The cache is indexed as a file in `metadata` and the file data itself is
    named as the Semantic Scholar `corpusId` for the paper. We load metadata on
    startup and fetch the rest as needed.

    Args:
        data_dir: Directory where the cache is located

    """
    with open(os.path.join(data_dir, "metadata")) as f:
        _cache = [*filter(None, f.read().split("\n"))]
    ss_cache = {"acl": {}, "doi": {}, "arxiv": {}, "corpus": {}}
    for _ in _cache:
        c = _.split(",")
        for key, ind in assoc:
            if c[ind]:
                ss_cache[key][c[ind]] = c[-1]
    print(f"Loaded cache with {sum(len(x) for x in ss_cache.values())} entries")
    return ss_cache


# NOTE: There's a separate acl_id here, because SS allows query by acl_id but
#       doesn't return it if it exists in the result.
def save_data(data, data_dir, ss_cache, acl_id):
    """Save Semantic Scholar cache to disk.

    We read and write data for individual papers instead of one big json object.

    Args:
        data: data for the paper
        data_dir: Directory where the cache is located
        ss_cache: The Semantic Scholar cache
        acl_id: ACL Id for the paper

    """
    with open(os.path.join(data_dir, data["paperId"]), "w") as f:
        json.dump(data, f)
    c = [acl_id if acl_id else "",
         data["arxivId"] if data["arxivId"] else "",
         str(data["corpusId"]),
         data["doi"] if data["doi"] else "",
         data["paperId"]]
    for key, ind in assoc:
        if c[ind]:
            ss_cache[key][c[ind]] = c[-1]
    # ss_cache["acl"][c[0]] = c[-1]
    # ss_cache["arxiv"][c[1]] = c[-1]
    # ss_cache["corpus"][c[2]] = c[-1]
    # ss_cache["doi"][c[3]] = c[-1]
    with open(os.path.join(data_dir, "metadata"), "a") as f:
        f.write(",".join(c) + "\n")
    print("Updated metadata")


def semantic_scholar_paper_details(id_type: str, ID: str, data_dir: str,
                                   ss_cache: Dict[str, Dict[str, Any]], force: bool):
    """Get semantic scholar paper details

    The Semantic Scholar cache is checked first and if it's a miss then the
    details are fetched from the server.

    Args:
        id_type: type of the paper identifier one of
                 `['ss', 'doi', 'mag', 'arxiv', 'acl', 'pubmed', 'corpus']`
        ID: paper identifier
        data_dir: Directory where the cache is loacaded
        ss_cache: The Semantic Scholar cache
        force: Force fetch from Semantic Scholar server, ignoring cache

    """
    urls = {"ss": f"https://api.semanticscholar.org/v1/paper/{ID}",
            "doi": f"https://api.semanticscholar.org/v1/paper/{ID}",
            "mag": f"https://api.semanticscholar.org/v1/paper/MAG:{ID}",
            "arxiv": f"https://api.semanticscholar.org/v1/paper/arXiv:{ID}",
            "acl": f"https://api.semanticscholar.org/v1/paper/ACL:{ID}",
            "pubmed": "https://api.semanticscholar.org/v1/paper/PMID:{ID}",
            "corpus": f"https://api.semanticscholar.org/v1/paper/CorpusID:{ID}"}
    if id_type not in urls:
        return json.dumps("INVALID ID TYPE")
    else:
        if id_type == "ss" and not force and ID in os.listdir(data_dir):
            print(f"Fetching from disk for {id_type}, {ID}")
            with open(os.path.join(data_dir, ID)) as f:
                return json.load(f)
        elif (id_type in {"doi", "acl", "arxiv", "corpus"}
              and ID in ss_cache[id_type] and ss_cache[id_type][ID]
              and not force):
            print(f"Fetching from cache for {id_type}, {ID}")
            with open(os.path.join(data_dir, ss_cache[id_type][ID])) as f:
                return json.load(f)
        else:
            acl_id = ""
            if id_type == "acl":
                acl_id = ID
            if not force:
                print(f"Data not in cache for {id_type}, {ID}. Fetching")
            else:
                print(f"Forced Fetching for {id_type}, {ID}")
            url = urls[id_type] + "?include_unknown_references=true"
            response = requests.get(url)
            if response.status_code == 200:
                save_data(json.loads(response.content), data_dir, ss_cache, acl_id)
                return response.content  # already JSON
            else:
                print(f"Server error. Could not fetch")
                return json.dumps(None)


class SemanticSearch:
    # Example params:
    #
    def __init__(self, debugger_path: Union[pathlib.Path, str]):
        self.default_params = {'queryString': '',
                               'page': 1,
                               'pageSize': 10,
                               'sort': 'relevance',
                               'authors': [],
                               'coAuthors': [],
                               'venues': [],
                               'yearFilter': None,
                               'requireViewablePdf': False,
                               'publicationTypes': [],
                               'externalContentTypes': [],
                               'fieldsOfStudy': [],
                               'useFallbackRankerService': False,
                               'useFallbackSearchCluster': False,
                               'hydrateWithDdb': True,
                               'includeTldrs': False,
                               'performTitleMatch': True,
                               'includeBadges': False}
        self.params = self.default_params.copy()
        self.update_params(debugger_path)

    def update_params(self, debugger_path):
        if debugger_path:
            check_flag = False
            try:
                import psutil
                for p in psutil.process_iter():
                    cmd = p.cmdline()
                    if cmd and ("google-chrome" in cmd[0] or "chromium" in cmd[0]):
                        check_flag = True
                        break
                print(f"Process {cmd[0]} found.")
            except Exception as e:
                print(e)
            if check_flag:
                print(f"Trying to update Semantic Scholar Search params")
                p = Popen(shlex.split(f"node {debugger_path}"), stdout=PIPE, stderr=PIPE)
                out, err = p.communicate()
                if err:
                    print("Chromium running but error communicating with it. Can't update params")
                    print(err.decode())
                else:
                    try:
                        vals = json.loads(out)
                        if "request" in vals and 'postData' in vals['request']:
                            if isinstance(vals['request']['postData'], dict):
                                vals = vals['request']['postData']
                            elif isinstance(vals['request']['postData'], str):
                                vals = json.loads(vals['request']['postData'])
                            else:
                                raise Exception("Not sure what data was sent")
                        self.params = vals.copy()
                        new_params = set(vals.keys()) - set(self.default_params.keys())
                        if new_params:
                            print(f"New params in SS Search {new_params}")
                        not_params = set(self.default_params.keys()) - set(vals.keys())
                        if not_params:
                            print(f"Params not in SS Search {not_params}")
                        values_to_update = {'performTitleMatch': True,
                                            'includeBadges': False,
                                            'includeTldrs': False,
                                            'fieldsOfStudy': ['computer-science']}
                        for k, v in values_to_update.items():
                            if k in self.params:
                                self.params[k] = v
                            else:
                                print(f"Could not update param {k}")
                        print(f"Updated params {self.params}")
                    except Exception as e:
                        print(f"Error updating params {e}. Will use default params")
                        self.params = self.default_params.copy()
            else:
                print("Chromium with debug port not running. Can't update params")
        else:
            print(f"Debug script path not given. Using default params")

    def semantic_scholar_search(self, query: str, cs_only: bool = False, **kwargs):
        """Perform a search on semantic scholar and return the results in JSON format
        By default the search is performed in Computer Science subjects

        pub_types can be ["Conference", "JournalArticle"]
        yearFilter has to be a :class:`dict` of type {"max": 1995, "min": 1990}

        """
        params = self.params.copy()
        params["queryString"] = query
        if 'yearFilter' in kwargs:
            yearFilter = kwargs['yearFilter']
            if yearFilter and not ("min" in yearFilter and "max" in yearFilter and
                                   yearFilter["max"] > yearFilter["min"]):
                print("Invalid Year Filter. Disabling.")
                yearFilter = None
            params['yearFilter'] = yearFilter
        if cs_only:
            params['fieldsOfStudy'] = ['computer-science']
        else:
            params['fieldsOfStudy'] = []
        for k, v in kwargs.items():
            if k in params and isinstance(v, type(params[k])):
                params[k] = v
        headers = {'User-agent': 'Mozilla/5.0', 'Origin': 'https://www.semanticscholar.org'}
        print("Sending request to semanticscholar search with query" +
              f": {query} and params {self.params}")
        response = requests.post("https://www.semanticscholar.org/api/1/search",
                                 headers=headers, json=params)
        if response.status_code == 200:
            results = json.loads(response.content)["results"]
            print(f"Got {len(results)} results for query: {query}")
            return response.content  # already json
        else:
            return json.dumps({"error": f"ERROR for {query}, {str(response.content)}"})
