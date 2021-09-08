# ChangeLog for the ref-man

## [2021-02-02 Tue 10:32]
- Fixed `ref-man--num-to-months`
- Added `condition-case` to `ref-man--validate-author`
- Changes to `ref-man-org-purge-entry`
- Added error message to `ref-man-parse-bib-property-key`
- Fixed extra `.pdf` being appended to filenames for `arxiv.org` urls
- Began implementing `ref-man-web-search`
- Some changes to `ref-man-web-gscholar`
- Added updating semantic scholar params with a node subprocess
- Added `progress` and `check_proxies` routes. `progress` is still pending. I
  think for that the Get class I'm trying to implement.

## [2021-02-11 Thu 02:52]
- Added .gitignore
- Version bump to 0.3.2
- Added url extraction from semanticscholar.org. Earlier it defaulted to first
  pdf link from buffer.
- Added `ref-man-url-get-best-pdf-url`.
- Lisp and python code follow separate versioning now.
- Fixed naming of PDF files downloaded from acm.org
- Fixed some pdf files names and downloads from openreview.net
- Fixed proxy_everything port.

## [2021-03-03 Wed 08:05]
- Python virtualenv is now always created and used.
- Staged debug_ss.js and package.json for chrome-remote
- Fixed python module `python -m ref_man` now runs correctly.

## [2021-03-04 Thu 16:28]
- `cache.py` is separate now and contains `CacheHelper`
- Pdfs remote links cache is now checked and fixed at startup if remote cache
  exists.

## [2021-03-06 Sat 00:36]
- Fixed some bugs in cache.
- Changed python process launch args to specify cache related args.
- Fixed an issue with semantic scholar params where it wouldn't switch to
  default if chrome was running and params couldn't be updated.

## [2021-03-22 Mon 08:56]
- Added `ref-man--invert-accents`.
- Strings are now inserted in org-mode without escaping non-ascii due to sytnax
  issues arising from that.
- Fixed a `cons` bug in `ref-man-url-get-best-pdf-url`
- Modified `ref-man-org-bibtex-read-from-headline`.
- Added `ref-man-org-get-bib-from-org-link`.
- Added command `ref-man-org-export-article` which exports the subtree to either pdf
  or html via pandoc. Primary advantage being auto generation of references.
- Defined `ref-man-bib-files` for `ref-man-org-export-article`. Can be added to
  other bibliography functions.

## [2021-03-23 Tue 08:38]
- Added `ref-man-fix-drawers-deleted-files` and utility function
  `ref-man-not-pdf-files`.

## [2021-03-23 Tue 08:38]
- `ref-man-chrome.el` is more modular now and later can be separated as a chrome
  debugger interface.
- New file `ref-man-py.el` separating python functionality from
  `ref-man-core.el`.
- Fix to `ref-man--generate-entry-from-hash` where I was passing `cons` instead
  of string to `ref-man--build-bib-key-from-plist`.
- Cleaner `ref-man--generate-buffer-and-fetch-if-required`
- Removed `util` as a dependency and added `ref-man-pairs-to-alist` which is a
  copy of `util/pairs-to-alist`.
- Some cleanup of the python module.
- Added `common_pyutil` as a dependency for the python module.
- Minor version bump to `0.4.0` with separation of python module.

## [2021-08-02 Mon 15:03]
- Modified README
- Updated requirements
- Version bump to `0.5.0`
- Replaced `cdass` with `a-get`
- Added the blog generation backend and settings for that.
- Added `ref-man-key-from-url` and `allow-misc` flag while exporting bibtex from
  org heading.
- Modularized `ref-man-docproc-export-article` and fixed some issues
- Fixed `References` heading was being added each time even without any
  citations
- Bibliography not in external files is now embedded in the yaml header itself.

## [2021-08-02 Mon 15:03]
- Added `ref-man-org-consolidate-drawers`, `ref-man-org-end-of-meta-data`,
  `ref-man-org-text-bounds`, `ref-man-org-bibtex-kill-headline-as-yaml`,
  `ref-man-org-delete-file-under-point`, `ref-man-org-update-from-from-crossref`,
  `ref-man-maybe-set-arxiv-id` in `ref-man-core.el`.
- Export functions are in new file `ref-man-export.el`.
- Added python version check in `ref-man-py-setup-env`
- Added fetcher for new version of AAAI site in
  `ref-man-url-get-pdf-link-helper` and some other additions.
- Added `ref-man-url-parse-cvf-venue` and `ref-man-url-cvf-pdf-link-helper`
  in `ref-man-url.el`.
- Changed Semantic Scholar function names, fetch and update are different
  functions now and you can display selective information.
- Fixed incorrect abstract formatting in `ref-man-org-insert-abstract` and it
  can optionally keep current text in entry
- Cleaned up initial server invocation from `ref_man.__main__.py`
- Changed `/get_cvpr_url` to `/get_cvf_url` and fixed some issues
- Added a lot of type annotations.
- Version bump to `0.6.0`.
- Py mod version bump to `0.3.2`.
