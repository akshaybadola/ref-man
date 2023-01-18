;;; ref-man-ss.el --- Semantic Scholar API calls for `ref-man'. ;;; -*- lexical-binding: t; -*-

;; Copyright (C) 2018,2019,2020,2021,2022,2023
;; Akshay Badola

;; Author:	Akshay Badola <akshay.badola.cs@gmail.com>
;; Maintainer:	Akshay Badola <akshay.badola.cs@gmail.com>
;; Time-stamp:	<Wednesday 18 January 2023 14:22:59 PM IST>
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

(defvar ref-man-ss-fetch-max-display-citations 500
  "Max number of citations to display in one go.

Used by `ref-man-ss-display-all-data'.")

(defvar ref-man-ss-nonascii-eascii-chars
  '(("Ã©" . "é")))

(defvar ref-man-ss-nonascii-punc-chars
  '(("â" . "-")
    ("â" . "-")
    ("â" . "\"")
    ("â" . "\"")
    ("â" . "-")
    ("â" . "--")
    ("â" . "'")
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
                 (-concat ref-man-ss-nonascii-punc-chars
                          ref-man-ss-nonascii-eascii-chars)
                 "\\|"))
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
            (replace-match (cdr (aref vecmap idx)) t)))))))


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

PARAMS: \\='((count . 100))

FILTERS: \\='((year . ((min . 2012) (max . 2018))))"
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

(defun ref-man-ss-citation-filter-get-venues ()
  (-flatten (a-vals ref-man-ss-citation-filter-preferred-venues)))

(eval-when-compile
  (defvar ref-man-ss-citation-filters
    `((author)
      (title)
      (year)
      ,(cons 'venue (ref-man-ss-citation-filter-get-venues))
      (citationcount)
      (influentialcitationcount))
    "ref-man references/citation filters.

Used to filter citations in a *Semantic Scholar* buffer.  They
are an alist of properties and values.
The possible keys are:

AUTHOR: ((author_names . (list of author_names))
         (author_ids . (list of author_ids))
         (exact . nil))
\\='exact in above refers to exact match in author names
\\='exact is ignored by author_ids

E.g.
(author (author_names . (\"Lady Bracknell\")) (author_ids . ()) (exact . t))

will match any entry, any of whose authors exactly matches \"Lady
Bracknell\"

Or:
(author (author_names (\"bracknell\" \"miller\")) (author_ids) (exact))
(author (author_names (\"bracknell\" \"miller\")) (author_ids (\"1253566\" \"2353566\")) (exact))

will match any entry, any of whose authors has either
\"bracknell\" or \"miller\" in their names

TITLE: (title (title_regexp \"some.*title.+regexp\") (invert))

YEAR: (year (min . 2010) (max . 2020))

VENUE: (venue (venues \"list\" \"of\" \"venue\" \"words\" \"to\" \"match\"))

CITATIONCOUNT: (citationcount (min . 10) (max . 1000))

INFLUENTIALCITATIONCOUNT: (influentialcitationcount  (min . 10) (max . 1000))
"))
(defvar ref-man-ss-filter-count 30)

(defun ref-man-ss-filter-conversion-vals (f)
  "Convenience function for conversion of user input for filters.

F is an element of filters as input by user.  See
`ref-man-ss-filter-selected-buffer' for how it's used."
  (pcase (car f)
    ((and (or 'year 'citationcount 'influentialcitationcount) c)
     `(,c
       (min . ,(if (numberp (cadr f)) (cadr f) (string-to-number (cadr f))))
       (max . ,(if (numberp (caddr f)) (caddr f) (string-to-number (caddr f))))))
    ((and 'venue c) `(venue . ((venues . ,(cdr f)))))
    ((and 'title c) `(title  . ((title_re  . ,(nth 1 f)) (invert . ,(nth 2 f)))))
    ((and 'author c) `(author . ((author_names . ,(nth 1 f))
                                 (author_ids . ,(when (= (length f) 3) (nth 2 f)))
                                 (exact . ,(when (= (length f) 4) (nth 3 f))))))
    (_ (user-error "Uknown filter %s" (car f)))))

(defun ref-man-ss-reset-filters ()
  "Reset the SS filters to their default values.

See `ref-man-ss-citation-filters'."
  (interactive)
  (setq ref-man-ss-citation-filters
        `((author)
          (title)
          (year)
          ,(cons 'venue (ref-man-ss-citation-filter-get-venues))
          (citationcount)
          (influentialcitationcount)))
  (message "Reset ref-man-ss filters."))

(defun ref-man-ss-display-all-data ()
  "Display all citations in a *Semantic Scholar* buffer removing any filters.

Useful for removing any filters and displaying the full citation
data upto a limit.  The limit is defined by
`ref-man-ss-fetch-max-display-citations'."
  (interactive)
  (let* ((buffer (current-buffer))
         (ssid (with-current-buffer buffer
                 (save-excursion
                   (goto-char (point-min))
                   (org-entry-get (point) "PAPERID"))))
         (count ref-man-ss-fetch-max-display-citations)
         ;; Try to get as many citations as possible
         (data (ref-man-ss-fetch-paper-citations ssid `((count . ,count)))))
    (with-current-buffer buffer
      (ref-man-org-update-filtered-subr data t))))

(defun ref-man-ss-filter-selected-buffer ()
  "Filter citations in a *Semantic Scholar* buffer.

The filters can be customized in `ref-man-ss-citation-filters'."
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
         (filters (mapcar #'ref-man-ss-filter-conversion-vals (-filter (lambda (x) (cdr x))
                                                                        default-filters)))
         (ssid (with-current-buffer buffer
                 (save-excursion
                   (goto-char (point-min))
                   (org-entry-get (point) "PAPERID"))))
         (data (ref-man-ss-fetch-paper-citations ssid `((count . ,count)) `(filters ,filters))))
    ;; (ref-man-ss-citation-filter-widget (buffer-name buffer) default-filters)
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

(defun ref-man-ss--widget-action-func (what)
  (lambda (from changed &rest args)
    (ref-man-ss-citation-filter-widget-handler what from changed args)))

(defun ref-man-ss--widget-notify-func (what)
  (lambda (from changed &rest args)
    (ref-man-ss-citation-filter-widget-handler what from changed args)))

(defun ref-man-ss-citation-filter-widget (buf-name default-filters)
  (let ((buf (get-buffer-create
              (format "*SS Filters - %s" (cadr (split-string buf-name "/")))))
        (win (util/get-or-create-window-on-side)))
    (with-current-buffer buf
      (kill-all-local-variables)
      (let ((inhibit-read-only t))
        (erase-buffer))
      (let ((action-func 'ref-man-ss--widget-action-func)
            (notify-func 'ref-man-ss--widget-notify-func))
        (remove-overlays)
        (widget-insert "Ref Man Filter Semantic Scholar Citations.\n")

        (widget-insert "\n")
        (widget-create 'checkbox
                       :notify (funcall action-func 'author)
                       (cdr (a-get default-filters 'author)))
        (widget-insert " Author Filter\n")
        (widget-create 'editable-list
                       :size 13
                       :notify (lambda (from changed &rest args)
                                 (ref-man-ss-citation-filter-widget-handler 'author-item from changed args))
                       :entry-format "%i %d %v" ; Text after the field!
                       :indent 2
                       '(editable-field :value "author-id"))

        (widget-insert "\n")
        (widget-create 'checkbox
                       :notify (funcall action-func 'title)
                       (cdr (a-get default-filters 'title)))
        (widget-create 'editable-field :size 1
                       :format " Title regexp: %v"
                       :action (funcall action-func 'title-val)
                       "")

        (widget-insert "\n")
        (widget-create 'checkbox
                       :notify (funcall notify-func 'venues)
                       (cdr (a-get default-filters 'venue)))
        (widget-insert " Preferred Venues:  ")
        (let ((venue-keys (a-keys my/ref-man-ss-citation-filter-venues)))
          (cl-loop for i from 0
                   for x in venue-keys do
                   (widget-insert (format "%s: " x))
                   (widget-create 'checkbox
                                  :notify (funcall notify-func (cons 'venue x))
                                  t)
                   (if (= (% (+ i 1) 5) 0)
                       (widget-insert (concat "\n" (make-string 22 ? )))
                     (unless (= i (- (length venue-keys) 1))
                       (widget-insert ", ")))))
        (widget-insert "\n")
        (widget-create 'checkbox
                       :notify (funcall notify-func 'year)
                       (cdr (a-get default-filters 'year)))
        (widget-insert " Year Range: ")
        (widget-create 'editable-field :size 4 :format "min = %v" "2000")
        (widget-create 'editable-field :size 4 :format "  max = %v" "2022")

        (widget-insert "\n")
        (widget-create 'checkbox
                       :notify (funcall notify-func 'cite-count)
                       (cdr (a-get default-filters 'citationcount)))
        (widget-insert  " Citation Count: ")
        (widget-create 'editable-field :size 4 :format "min = %v" "1")
        (widget-create 'editable-field :size 4 :format "  max = %v" "1000")

        (widget-insert "\n")
        (widget-create 'checkbox
                       :notify (funcall notify-func 'inf-cite-count)
                       (cdr (a-get default-filters 'influentialcitationcount)))
        (widget-insert  " Influential Citation Count: ")
        (widget-create 'editable-field :size 4 :format "min = %v" "1")
        (widget-create 'editable-field :size 4 :format "  max = %v" "1000")

        (widget-create 'editable-field :size 2 :format "\nNumber of Results = %v"
                       :action (funcall action-func 'num-results)
                       (number-to-string ref-man-ss-filter-count))
        (use-local-map widget-keymap)
        (widget-setup))
      (set-window-buffer win buf))))


(provide 'ref-man-ss)
