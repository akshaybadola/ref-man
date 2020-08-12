import os
import json
import requests


assoc = [(x, i) for i, x in enumerate(["acl", "arxiv", "corpus", "doi"])]


def load_ss_cache(data_dir):
    with open(os.path.join(data_dir, "metadata")) as f:
        _cache = [*filter(None, f.read().split("\n"))]
    ss_cache = {"acl": {}, "doi": {}, "arxiv": {}, "corpus": {}}
    for _ in _cache:
        c = _.split(",")
        for key, ind in assoc:
            if c[ind]:
                ss_cache[key][c[ind]] = c[-1]
        # ss_cache["acl"][c[0]] = c[-1]
        # ss_cache["arxiv"][c[1]] = c[-1]
        # ss_cache["corpus"][c[2]] = c[-1]
        # ss_cache["doi"][c[3]] = c[-1]
    print(f"Loaded cache {ss_cache}")
    return ss_cache


def save_data(data, data_dir, ss_cache, acl_id):
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


def semantic_scholar_paper_details(id_type, id, data_dir, ss_cache, force):
    urls = {"ss": f"https://api.semanticscholar.org/v1/paper/{id}",
            "doi": f"https://api.semanticscholar.org/v1/paper/{id}",
            "mag": f"https://api.semanticscholar.org/v1/paper/MAG:{id}",
            "arxiv": f"https://api.semanticscholar.org/v1/paper/arXiv:{id}",
            "acl": f"https://api.semanticscholar.org/v1/paper/ACL:{id}",
            "pubmed": "https://api.semanticscholar.org/v1/paper/PMID:{id}",
            "corpus": f"https://api.semanticscholar.org/v1/paper/CorpusID:{id}"}
    if id_type not in urls:
        return json.dumps("INVALID ID TYPE")
    else:
        if id_type == "ss" and not force:
            print(f"Fetching from disk for {id_type}, {id}")
            with open(os.path.join(data_dir, id)) as f:
                return json.load(f)
        elif id_type in {"doi", "acl", "arxiv", "corpus"}\
             and id in ss_cache[id_type] and ss_cache[id_type][id]\
             and not force:
            print(f"Fetching from cache for {id_type}, {id}")
            with open(os.path.join(data_dir, ss_cache[id_type][id])) as f:
                return json.load(f)
        else:
            acl_id = ""
            if id_type == "acl":
                acl_id = id
            if not force:
                print(f"Data not in cache for {id_type}, {id}. Fetching")
            else:
                print(f"Forced Fetching for {id_type}, {id}")
            url = urls[id_type] + "?include_unknown_references=true"
            response = requests.get(url)
            if response.status_code == 200:
                save_data(json.loads(response.content), data_dir, ss_cache, acl_id)
                return response.content  # already JSON
            else:
                print(f"Server error. Could not fetch")
                return json.dumps(None)


def semantic_scholar_search(query, title_only=False, authors=[], cs_only=True,
                            pub_types=[], has_github=False, year_filter=None):
    """Perform a search on semantic scholar and return the results in JSON format
    By default the search is performed in Computer Science subjects

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
