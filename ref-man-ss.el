;;; ref-man-ss.el --- Semantic Scholar API calls for `ref-man'. ;;; -*- lexical-binding: t; -*-

;; Copyright (C) 2018,2019,2020,2021,2022
;; Akshay Badola

;; Author:	Akshay Badola <akshay.badola.cs@gmail.com>
;; Maintainer:	Akshay Badola <akshay.badola.cs@gmail.com>
;; Time-stamp:	<Monday 26 September 2022 09:03:10 AM IST>
;; Keywords:	pdfs, references, bibtex, org, eww

;; This file is *NOT* part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by the Free
;; Software Foundation; either version 3, or (at your option) any later
;; version.

;; This program is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
;; FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for
;; more details.

;; You should have received a copy of the GNU General Public License along with
;; GNU Emacs; see the file COPYING.  If not, write to the Free Software
;; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.

;;; Commentary:
;;
;; Functions for interacting with Semantic Scholar data fetched from Semantic
;; Scholar Graph API. See https://www.semanticscholar.org/product/api
;;
;; These functions use a local intermediary files cache and interact
;; with a `ref-man-py' process, which is a flask (see https://palletsprojects.com/p/flask/)
;; process doing a bunch of things.


(require 'widget)
(eval-when-compile
  (require 'wid-edit))

(require 'ref-man-py)
(require 'ref-man-util)

(defvar ref-man-ss-citation-filter-preferred-venues nil
  "Alist of preferred venues.

They should be in format ((symbol (\"list\" \"of\" \"keywords\"))).")

(defvar ref-man-ss-nonascii-punc-chars
  '(("â" . "-")
    ("â" . "-")
    ("â" . "\"")
    ("â" . "\"")
    ("â" . "-")
    ("Ã¢ÂÂ" . "--")
    ("Ã¢ÂÂ" . "--")
    ("Ã¢ÂÂ" . " ")
    ("Ã¢ÂÂ" . "\"")
    ("Ã¢ÂÂ" . "\"")))


(defvar ref-man-ss-nonascii-special-chars
  '(("ÃÂ»" . "λ")
    ("Å" . "ł")
    ("ÃÂ²" . "β")))

(defun ref-man-ss-fix-nonascii-chars-in-entry ()
  "Fix nonascii chars in current org entry.

These chars would be introduced due to encoding issues with SS
data."
  (interactive)
  (let ((regexp (mapconcat
                 (lambda (x) (format "\\(%s\\)" (car x)))
                 ref-man-ss-nonascii-punc-chars "\\|"))
        (n (length ref-man-ss-nonascii-punc-chars))
        (vecmap (apply #'vector ref-man-ss-nonascii-punc-chars)))
    (save-excursion
      (save-restriction
        (org-narrow-to-subtree)
        (goto-char (point-min))
        (while (re-search-forward regexp nil t)
          (let ((idx (cl-loop
                      for i from 1 to n
                      until (match-string i)
                      finally return (- i 1))))
            (replace-match (cdr (aref vecmap idx)))))))))


(defun ref-man-ss-replace-nonascii-punc-chars (str)
  "Replace nonascii chars due to SS encoding errors in string STR."
  (interactive)
  (let ((regexp (mapconcat
                 (lambda (x) (format "\\(%s\\)" (car x)))
                 ref-man-ss-nonascii-punc-chars "\\|"))
        (n (length ref-man-ss-nonascii-punc-chars))
        (vecmap (apply #'vector ref-man-ss-nonascii-punc-chars)))
    (with-temp-buffer
      (insert str)
      (goto-char (point-min))
      (while (re-search-forward regexp nil t)
        (let ((idx (cl-loop
                    for i from 1 to n
                    until (match-string i)
                    finally return (- i 1))))
          (replace-match (cdr (aref vecmap idx)))))
      (buffer-string))))


(defun ref-man-ss-fetch-paper-details (ssid &optional update-on-disk)
  "Fetch Semantic Scholar data for ID SSID.

The data is cached on the disk and if the data for entry is
already present, the cached entry is fetched.  With optional
argument UPDATE-ON-DISK, force update the data in cache."
  (let* ((idtype-id ssid)
         (opts `(("id_type" . ,(car idtype-id))
                 ("id" . ,(cdr idtype-id))))
         (opts (if update-on-disk
                   (-concat opts '(("force" . "")))
                 opts))
         (ss-data (when idtype-id
                    (message "[ref-man] %s Semantic Scholar Data for %s id: %s"
                             (if update-on-disk "Force fetching" "Fetching")
                             (car idtype-id) (cdr idtype-id))
                    (with-current-buffer
                        (url-retrieve-synchronously
                         (ref-man-py-url "s2_paper" opts) t) ; silent
                      (goto-char (point-min))
                      (forward-paragraph)
                      (json-read)))))
    ss-data))

(defun ref-man-ss-fetch-paper-citations (ssid &optional params filters)
  "Fetch paper citations for SSID.

Optional PARAMS specifies any filters to be added to the
citations.  By default citations are fetched in increments of
100, but that can be changed with PARAMS.

Optional FILTERS are declarative filters that are applied to the
results.

PARAMS can be queried from the service.

Example PARAMS and FILTERS alist for getting 100 citations from
years 2012-2018:

PARAMS: '((count . 100))

FILTERS: '((year . ((min . 2012) (max . 2018))))
"
  (let* ((url (ref-man-py-url (format "s2_citations/%s" ssid) params))
         (buf (if (and filters (cdr filters))
                  (ref-man--post-json-synchronous url filters t)
                (url-retrieve-synchronously url t))))
    (prog1
        (with-current-buffer buf
          (goto-char (point-min))
          (forward-paragraph)
          (json-read))
      (kill-buffer buf))))

(defun ref-man-ss-parse-search-result (result)
  "Parse the RESULT of a Semantic Scholar search.

RESULT should be an alist."
  (let ((retval nil))
    (when-let ((cites (a-get result 'citationStats)))
      (push `("CITATIONCOUNT". ,(number-to-string (a-get cites 'numCitations))) retval)
      (push `("INFLUENTIALCITATIONCOUNT". ,(number-to-string (a-get cites 'numKeyCitations))) retval))
    (push `("PAPERID". ,(a-get result 'id)) retval)
    (push `("ABSTRACT". ,(a-get (a-get result 'paperAbstract) 'text)) retval)
    (push `("DOI". ,(a-get (a-get result 'doiInfo) 'doi)) retval)
    (push `("URL". ,(a-get (a-get result 'primaryPaperLink) 'url)) retval)
    (push `("YEAR". ,(a-get (a-get result 'year) 'text)) retval)
    (push `("VENUE". ,(a-get (a-get result 'venue) 'text)) retval)
    (when-let ((date (a-get result 'pubDate)))
      (push `("MONTH" . ,(capitalize (a-get ref-man--num-to-months
                                            (string-to-number
                                             (nth 1 (split-string date "-"))))))
            retval))
    (seq-do (lambda (x)
              (if (eq (car x) 'name)
                  (push `("JOURNAL". ,(cdr x)) retval)
                (push `(,(upcase (symbol-name (car x))). ,(cdr x)) retval)))
            (a-get result 'journal))
    (push `("AUTHOR". ,(mapconcat (lambda (x)
                                    (ref-man--build-bib-author
                                     (a-get (aref x 0) 'name)))
                                  (a-get result 'authors) " and "))
          retval)
    (push `("TITLE". ,(a-get (a-get result 'title) 'text)) retval)
    (push '("TYPE" . "article") retval)
    (-remove (lambda (x) (or (not (cdr x)) (string-empty-p (cdr x)))) retval)))

(defun ref-man-ss-search (search-string &rest _args)
  "Search for SEARCH-STRING via Graph API on Semantic Scholar."
  (if (string-empty-p search-string)
      (user-error "Empty Search String")
    (let* ((opts `(("q" . ,search-string)))
           (url (ref-man-py-url "s2_search" opts))
           (buf (url-retrieve-synchronously url))
           (result (with-current-buffer buf
                     (goto-char (point-min))
                     (forward-paragraph)
                     (json-read))))
      (if (eq (car result) 'error)
          (user-error (format "Error occurred %s" (a-get result 'error)))
        (a-get result 'data)))))

(defun ref-man-ss-get-results-search-semantic-scholar (search-string &optional args)
  (let* ((opts (if args
                   (-concat `(("q" . ,search-string)) args)
                 `(("q" . ,search-string))))
         (url (ref-man-py-url "semantic_scholar_search" opts))
         (buf (if args (ref-man--post-json-synchronous url args)
                (url-retrieve-synchronously url)))
         (result (with-current-buffer buf
                   (goto-char (point-min))
                   (forward-paragraph)
                   (json-read)))
         (results (if (eq (car result) 'error)
                      (user-error (format "Error occurred %s" (a-get result 'error)))
                    (a-get result 'results))))
    results))

(defun ref-man-ss-graph-search-results-to-ido-prompts (results)
  "Parse the search RESULTS from SS Graph API as `ido' prompts for user insertion."
  (let ((j 1))
    (mapcar (lambda (x)
              (prog1 (format "%d: %s, %s" j
                             (a-get x 'title)
                             (mapconcat
                              (lambda (y) (a-get y 'name)) (a-get x 'authors) ", "))
                (setq j (+ 1 j))))
            results)))

(defun ref-man-ss-search-results-to-ido-prompts (results)
  "Parse the search RESULTS from SS as `ido' prompts for user insertion."
  (let ((j 1))
    (mapcar (lambda (x)
              (prog1 (format "%d: %s, %s" j
                             (a-get (a-get x 'title) 'text)
                             (mapconcat
                              (lambda (y) (a-get (aref y 0) 'name))
                              (a-get x 'authors) ", "))
                (setq j (+ 1 j))))
            results)))


(defun ref-man-ss-search-presentations-to-ido-prompts (results)
  "Parse the search RESULTS from SS as `ido' prompts for user insertion.

In this case the `matchedPresentations' key is extracted."
  (let ((j 1))
    (mapcar (lambda (x)
              (prog1 (format "%d: %s, %s" j
                             (a-get x 'title)
                             (string-join (a-get x 'authors) ", "))
                (setq j (+ 1 j))))
            results)))


(setq ref-man-ss-citation-filters `((author)
                                    ;; (title "transfer")
                                    (year 2020 2022)
                                    ;; ,(cons 'venue (ref-man-ss-citation-filter-get-venues))
                                    (citationcount 100 100000)
                                    ;; (influentialcitationcount 1 100)
                                    )
      ref-man-ss-filter-count 200)

(defun ref-man-ss-citation-filter-get-venues ()
  (-flatten (a-vals ref-man-ss-citation-filter-venues)))

(defun ref-man-ss-filter-selected-buffer ()
  (interactive)
  (let* ((buffer (if current-prefix-arg
                     (ido-completing-read
                      "Semantic Scholar Buffer: "
                      (-keep (lambda (x) (and (string-match-p "^*Semantic Scholar/.+" (buffer-name x))
                                              (buffer-name x)))
                             (buffer-list)))
                   (current-buffer)))
         (default-filters ref-man-ss-citation-filters)
         (count ref-man-ss-filter-count)
         (filters (mapcar #'ref-man-org-filter-conversion-vals (-filter (lambda (x) (cdr x))
                                                                        default-filters)))
         (ssid (with-current-buffer buffer
                 (save-excursion
                   (goto-char (point-min))
                   (org-entry-get (point) "PAPERID"))))
         (data (ref-man-ss-fetch-paper-citations ssid `((count . ,count)) `(filters ,filters))))
    (with-current-buffer buffer
      (ref-man-org-update-filtered-subr data t))))

(defun ref-man-ss-citation-filter-widget-handler (from wid changed &rest args)
  (let ((enabled (plist-get (cdr wid) :value)))
    (message
     (pcase from
       ('author "Authors %s")
       ('title "Title Regexp %s")
       ('year "Year Range %s")
       ('venues "Preferred Venues %s")
       ('cite-count "Citation Count %s")
       ('inf-cite-count "Influential Citation Count %s")
       (_ (debug)))
     (if enabled "Enabled" "Disabled"))))

(defun ref-man-ss-citation-filter-widget (buf-name)
  (switch-to-buffer (format "*filter citations settings for %s*" buf-name))
  (kill-all-local-variables)
  (let ((inhibit-read-only t))
    (erase-buffer))
  (remove-overlays)
  (widget-insert "Ref Man Filter Semantic Scholar Citations.\n")

  (widget-insert "\n")
  (widget-create 'checkbox
                 :notify (lambda (from changed &rest args)
                           (my/ref-man-filter-handler 'author from changed args))
                 nil)
  (widget-insert " Author Filter\n")
  (widget-create 'editable-list
                 :size 13
                 :notify (lambda (from changed &rest args)
                           (my/ref-man-filter-handler 'author-item from changed args))
                 :entry-format "%i %d %v" ; Text after the field!
                 :indent 2
                 '(editable-field :value "author-id"))

  (widget-insert "\n")
  (widget-create 'checkbox
                 :notify (lambda (from changed &rest args)
                           (my/ref-man-filter-handler 'title from changed args))
                 nil)
  (widget-create 'editable-field :size 1 :format " Title regexp: %v" "")

  (widget-insert "\n")
  (widget-create 'checkbox
                 :notify (lambda (from changed &rest args)
                           (my/ref-man-filter-handler 'venues from changed args))
                 nil)

  (widget-insert " Preferred Venues:  ")
  (seq-do (lambda (x)
            (widget-insert (format "%s: " x))
            (widget-create 'checkbox
                           :notify (lambda (from changed &rest args)
                                     (my/ref-man-filter-handler (cons 'venue x) from changed args))
                           t)
            (widget-insert ", "))
          (-butlast (a-keys my/ref-man-ss-citation-filter-venues)))
  (let ((x (-last-item (a-keys my/ref-man-ss-citation-filter-venues))))
    (widget-insert (format "%s: " x))
    (widget-create 'checkbox
                   :notify (lambda (from changed &rest args)
                             (my/ref-man-filter-handler (cons 'venue x) from changed args))
                   t))

  (widget-insert "\n")
  (widget-create 'checkbox
                 :notify (lambda (from changed &rest args)
                           (my/ref-man-filter-handler 'year from changed args))
                 nil)
  (widget-insert " Year Range: ")
  (widget-create 'editable-field :size 4 :format "min = %v" "2000")
  (widget-create 'editable-field :size 4 :format "  max = %v" "2022")

  (widget-insert "\n")
  (widget-create 'checkbox
                 :notify (lambda (from changed &rest args)
                           (my/ref-man-filter-handler 'cite-count from changed args))
                 nil)
  (widget-insert  " Citation Count: ")
  (widget-create 'editable-field :size 4 :format "min = %v" "1")
  (widget-create 'editable-field :size 4 :format "  max = %v" "1000")

  (widget-insert "\n")
  (widget-create 'checkbox
                 :notify (lambda (from changed &rest args)
                           (my/ref-man-filter-handler 'inf-cite-count from changed args))
                 t)
  (widget-insert  " Influential Citation Count: ")
  (widget-create 'editable-field :size 4 :format "min = %v" "1")
  (widget-create 'editable-field :size 4 :format "  max = %v" "1000")

  (widget-create 'editable-field :size 2 :format "\nNumber of Results = %v" "30"
                 :notify (lambda (from changed &rest args)
                           (my/ref-man-filter-handler 'num-results from changed args)))
  (use-local-map widget-keymap)
  (widget-setup))


(provide 'ref-man-ss)
