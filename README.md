# Reference Manager

Emacs plugin to generate bibliography file automatically from a given plugin using various other libraries.
Also added functionality for browing gscholar with *eww*.

# Features

This project has now grown into a monolith and I'm refactoring it slowly to
decouple the features. These features are useful but not entirely stable at
present.

## Bibliography and file management

- Conversion from and to `org` property drawer to `bibtex` format
  - Import and export from `.bib` files
  - Sanitization and auto generation of keys
- Fetching of bibliography data from multiple sources and storing in org entries
  1. DBLP
  2. Semantic Scholar
  3. Crossref
  4. Google Scholar
- Fetching of pdfs from supported sources and storage in a dedicated directory
  See `ref-man-url-get-pdf-link-helper` in `ref-man-url.el`.

I've used a separate python module for network access with threading as it's
more efficient to do so. The communication is done via an http server.

## Reading and Navigation

Org headings serve as publication titles. For any notes one can simply store
them with the corresponding org headings. However for linking and citations
there are also some utility functions:

- Parsing of org headlines with filters in specified buffers into a cache.
- Insertion of link to any heading with `ido`
- Search and removal of duplicate headings.

## Research sources and Exploration

Currently two sources for exploration, search and archival are supported.

### Google Scholar

One can browse Google Scholar in a dedicated buffer derived from *eww* with
custom functions and keybindings for:

1. Easy navigation, filtering by date.
2. Import google scholar entry at point to org headline with metadata.
   - With optional fetching of PDF simultaneously
3. Import bibtex for google scholar entry at point.
4. Rendering via *chromium* debugger to avoid "prove you're not a robot".

### Semantic Scholar

We can also use [Semantic Scholar](https://www.semanticscholar.org). The module
can search Semantic Scholar with a search string, or lookup the SS database
through their API.

1. Search in Semantic Scholar and insert entry as org headline
2. Lookup an entry in Semantic Scholar database from any of the supported lookup
   types (arxiv, doi etc.)
3. Fetch the entry metadata and store in a cache
4. Parse the metadata and display in a separate org buffer with more details:
   - Abstract
   - Other Semantic Scholar metrics like `isInfluential`
   - All the parsed references from the metadata
   - All the papers which have cited that particular paper

Since we can see all the references and citations of that paper in a single org
buffer, we can view/download any references from the paper with ease without
constantly having to go to the back of the paper. We can also fetch the PDF (if
the source is supported by the module) and quickly check that paper also.

In addition, with all the papers which have cited the paper one is reading also
present, a quick bird's eye view of the state of the art is possible and more
recent interesting publications can also be downloaded quickly.

#### Science Parse

Semantic Scholar uses [science parse](https://github.com/allenai/science-parse)
to parse the PDFs' metadata. They provide the full model on that link and one
can also run that service locally in case one comes across a pdf not in their
database.

### Document preparation and publishing

Since we can embed Math markup, images, tables and links in the org buffer, it
can be exported to a fairly functional document. I've used a pandoc backend for
easy export to multiple formats.

#### Features

1. Automatic conversion of links in text to citations
   - A bibliography section is added automatically at the end if required.
2. Custom flags and switches for pandoc via its yaml header
3. Automatic insertion of additional bibliography files with yaml metadata
4. Easy export to html, PDF or Latex format

### Backup and File Sharing

The entire pdf and metadata cache can be uploaded to a supported cloud storage
for easy backup, access and sharing. I've used `rclone` for that and any backend
supported by `rclone` can therefore be theoretically used. We can:

1. Convert to html and mail an org buffer or subtree with pdf links being
   attached automatically from the specific remote storage.

For mail I use [mu4e](https://www.djcbsoftware.nl/code/mu/mu4e.html) and a
separate module which I call
[org-mu4e](https://github.com/akshaybadola/org-mu4e) which is built on top of
[org-mime](https://github.com/org-mime/org-mime) as a backend.

# Roadmap

There are some bugs and a lot of incomplete features. I had constructed a PyQt
GUI for viewing the citations as a graph but that project was shelved due to
lack of time. It can easily be repurposed and integrated with this project as a
backend.

Another very useful thing would be to have a JS based UI layer which can
interact with Emacs as a daemon for people who aren't so comfortable with
Emacs. We can parse `org` metadata (possibly with multiple threads) and render
it with HTML. It would be much more useful to the broader scientific community.

- Refactoring to make it more modular and remove redundant code.
- More comprehensive Documentation and Tutorial
- Unit/Regression testing setup
- Finish pending/incomplete features
- A mind-map/network layer for visualization
- UI layer on top for non emacs users as an optional module

# License

All the code in the repo is licensed under GPLv3. See LICENSE.md file in the repo.

For all libraries being used along with this codebase, please refer to their
licencses.

For any external modules or services (like Semantic Scholar or DBLP) being used,
please see their individual terms of services.

