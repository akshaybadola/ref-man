;; ref-man.el --- Manage bibliographic references and associated documents in emacs. ;;; -*- lexical-binding: t; -*-

;; Copyright (C) 2018,2019
;; Akshay Badola

;; Author:	Akshay Badola <akshay.badola.cs@gmail.com>
;; Maintainer:	Akshay Badola <akshay.badola.cs@gmail.com>
;; Time-stamp:	<Thu May 16 14:54:32 IST 2019>
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

;; Have to restructure it
;; Separating org-config from org-functions in the package

(require 'cl)
(require 'eww)
(require 'org)
(require 'seq)
(require 'shr)
(require 'url)
(require 'xml)
(require 'async)
;; Primary function I use from 'bibtex is 'bibtex-parse-entry
(require 'bibtex)
;; It's only required for one function 'org-bibtex-read-file
;; But it's a pretty complicated function
(require 'org-bibtex)                   
(require 'biblio-core)
(require 'gscholar-bibtex)

;;
;; 
(setq ref-man-org-links-file-path (expand-file-name "~/.temp-org-links.org"))
(setq ref-man-pubs-directory (expand-file-name "~/org/pdfs/"))
;; (setq ref-man-temp-bib-file-path (expand-file-name "~/lib/docprocess/all.bib"))
(setq ref-man-temp-bib-file-path (expand-file-name "~/.temp.bib"))
(setq ref-man-org-store-dir (expand-file-name "~/org/pubs_org/"))

;; Internal global variables
(setq ref-man--org-heading-gscholar-launch-buffer nil)
(setq ref-man--org-heading-gscholar-launch-point nil)
(setq ref-man--org-gscholar-launch-buffer nil)
(setq ref-man--org-gscholar-launch-point nil)
(setq ref-man--eww-import-link nil)

(defvar shr-map
  (let ((map (make-sparse-keymap)))
    (define-key map "a" 'shr-show-alt-text)
    (define-key map "i" 'shr-browse-image)
    (define-key map "z" 'shr-zoom-image)
    (define-key map [?\t] 'shr-next-link)
    (define-key map [?\M-\t] 'shr-previous-link)
    (define-key map [follow-link] 'mouse-face)
    (define-key map [mouse-2] 'shr-browse-url)
    (define-key map "I" 'shr-insert-image)
    (define-key map "w" 'shr-copy-url)
    (define-key map "u" 'shr-copy-url)
    (define-key map "RET" 'shr-browse-url)
    (define-key map "o" 'shr-save-contents)
    (define-key map "\r" 'shr-browse-url)
    map))


;;;;;;;;;;;;;;;;;;;;;;;;
;; ref-man constants  ;;
;;;;;;;;;;;;;;;;;;;;;;;;

;;
;; Constants. perhaps can name them better
;; Also should be shifted to defcustom
;;
(setq ref-man-venue-priorities (let* ((confs '("icml" "nips" "iccv" "cvpr" "eccv"))
       (confs-seq (number-sequence (length confs) 1 -1)))
       (mapcar* 'cons confs confs-seq)))
(setq ref-man-key-list '(authors title venue volume number pages year doi ee))
(setq ref-man-stop-words '("i" "it" "its" "what" "which" "who" "that" "these" "is" "are" "was" "were" "have" "has" "had" "having" "do" "does" "did" "a" "an" "the" "and" "because" "as" "of" "at" "by" "for" "with" "about" "against" "between" "into" "through" "during" "before" "after" "above" "below" "to" "from" "up" "down" "in" "out" "on" "off" "over" "under" "again" "further" "then" "once" "here" "there" "when" "where" "why" "how" "all" "any" "both" "each" "few" "more" "most" "other" "some" "such" "no" "nor" "not" "only" "own" "same" "so" "than" "too" "very" "s" "t" "can" "will" "just" "don" "should" "now"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; end ref-man constants  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; elementary utility functions ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun path-join (path-elements-list)
  (mapconcat (lambda (x) (string-remove-suffix "/" x)) path-elements-list "/"))

(defun firstn (x n)
  (butlast x (- (length x) n)))

(defun butfirst (x &optional n)
  (let ((n (if n n 1)))
    (last x (- (length x) n))))

(defun max-ind (seq)
  (let* ((max-ind--max-val 0) (max-ind--temp-ind -1) (max-ind--max 0))
    (loop for x in seq
          do
          (progn
            (setq max-ind--temp-ind (+ 1 max-ind--temp-ind))
            (if x (if (> x max-ind--max-val)
                      (progn (setq max-ind--max-val x)
                             (setq max-ind--max max-ind--temp-ind))))))
    max-ind--max))

(defun ref-man--trim-whitespace (str)
  "Trims the string and replaces multiple spaces with a single one"
  (replace-in-string (string-trim str) "[ ]+" " "))

(defun sort-words (reverse beg end)
  "Sort words in region alphabetically, in REVERSE if negative.
    Prefixed with negative \\[universal-argument], sorts in reverse.
    The variable `sort-fold-case' determines whether alphabetic case
    affects the sort order.
    See `sort-regexp-fields'."
  (interactive "*P\nr")
  (sort-regexp-fields reverse "\\w+" "\\&" beg end))

(defun ref-man--fix-curly (str)
  "gets text between parentheses {}"
  (string-remove-suffix "}" (string-remove-prefix "{" str)))

(defun ref-man--is-bibtex-key (item)
  (string= (car item) "=key="))

(defun replace-in-string (in what with)
  (replace-regexp-in-string
   (regexp-quote what) with in nil 'literal))

(defun sort-symbols (reverse beg end)
  "Sort symbols in region alphabetically, in REVERSE if negative.
    See `sort-words'."
  (interactive "*P\nr")
  (sort-regexp-fields reverse "\\(\\sw\\|\\s_\\)+" "\\&" beg end))

(defun ref-man--is-stop-word (x)
  (member x ref-man-stop-words))

(defun ref-man--get-or-create-window-on-side ()
  (let* ((orig-win (selected-window))
         (win (cond ((window-in-direction 'right orig-win)
                     (window-in-direction 'right orig-win))
                    ((window-in-direction 'left orig-win)
                     (window-in-direction 'left orig-win))
                    (t (split-window-horizontally)))))
    win))

;; The characters directly borrowed from org-ref.
;; The function is too long and is an ugly hack because I couldn't
;; find an effective way to update a local variable in a cl-loop.
;; And setting a global variable was causing trouble with parallel
;; implementation.
(defun ref-man--remove-non-ascii (str)
  (let* ((str (replace-in-string str "í" "{\\\\'i}"))
         (str (replace-in-string str "æ" "{\\\\ae}"))
         (str (replace-in-string str "ć" "{\\\\'c}"))
         (str (replace-in-string str "é" "{\\\\'e}"))
         (str (replace-in-string str "ä" "{\\\\\"a}"))
         (str (replace-in-string str "è" "{\\\\`e}"))
         (str (replace-in-string str "à" "{\\\\`a}"))
         (str (replace-in-string str "á" "{\\\\'a}"))
         (str (replace-in-string str "ø" "{\\\\o}"))
         (str (replace-in-string str "ë" "{\\\\\"e}"))
         (str (replace-in-string str "ü" "{\\\\\"u}"))
         (str (replace-in-string str "ñ" "{\\\\~n}"))
         (str (replace-in-string str "ņ" "{\\\\c{n}}"))
         (str (replace-in-string str "ñ" "{\\\\~n}"))
         (str (replace-in-string str "å" "{\\\\aa}"))
         (str (replace-in-string str "ö" "{\\\\\"o}"))
         (str (replace-in-string str "á" "{\\\\'a}"))
         (str (replace-in-string str "í" "{\\\\'i}"))
         (str (replace-in-string str "ó" "{\\\\'o}"))
         (str (replace-in-string str "ó" "{\\\\'o}"))
         (str (replace-in-string str "ú" "{\\\\'u}"))
         (str (replace-in-string str "ú" "{\\\\'u}"))
         (str (replace-in-string str "ý" "{\\\\'y}"))
         (str (replace-in-string str "š" "{\\\\v{s}}"))
         (str (replace-in-string str "č" "{\\\\v{c}}"))
         (str (replace-in-string str "ř" "{\\\\v{r}}"))
         (str (replace-in-string str "š" "{\\\\v{s}}"))
         (str (replace-in-string str "İ" "{\\\\.i}"))
         (str (replace-in-string str "ğ" "{\\\\u{g}}"))
         (str (replace-in-string str "δ" "$\\\\delta$"))
         (str (replace-in-string str "ç" "{\\\\c{c}}"))
         (str (replace-in-string str "ß" "{\\\\ss}"))
         (str (replace-in-string str "≤" "$\\\\le$"))
         (str (replace-in-string str "≥" "$\\\\ge$"))
         (str (replace-in-string str "<" "$<$"))
         (str (replace-in-string str "θ" "$\\\\theta$"))
         (str (replace-in-string str "μ" "$\\\\mu$"))
         (str (replace-in-string str "→" "$\\\\rightarrow$"))
         (str (replace-in-string str "⇌" "$\\\\leftrightharpoons$"))
         (str (replace-in-string str "×" "$\\\\times$"))
         (str (replace-in-string str "°" "$\\\\deg$"))
         (str (replace-in-string str "ş" "{\\\\c{s}}"))
         (str (replace-in-string str "γ" "$\\\\gamma$"))
         (str (replace-in-string str "ɣ" "$\\\\gamma$"))
         (str (replace-in-string str "º" "degc"))
         (str (replace-in-string str "η" "$\\\\eta$"))
         (str (replace-in-string str "µ" "$\\\\mu$"))
         (str (replace-in-string str "α" "$\\\\alpha$"))
         (str (replace-in-string str "β" "$\\\\beta$"))
         (str (replace-in-string str "ɛ" "$\\\\epsilon$"))
         (str (replace-in-string str "ⅵ" "\\textrm{vi}"))
         (str (replace-in-string str "ⅲ" "\\textrm{iii}"))
         (str (replace-in-string str "ⅴ" "\\textrm{v}"))
         (str (replace-in-string str "λ" "$\\\\lambda$"))
         (str (replace-in-string str "π" "$\\\\pi$"))
         (str (replace-in-string str "∞" "$\\\\infty$"))
         (str (replace-in-string str "χ" "$\\\\chi$"))
         (str (replace-in-string str "∼" "\\\\textasciitilde{}"))
         (str (replace-in-string str "‑" "\\\\textemdash{}"))
         (str (replace-in-string str " " " "))
         (str (replace-in-string str "…" "..."))
         (str (replace-in-string str "•" "\\\\textbullet "))
         (str (replace-in-string str " " " "))
         (str (replace-in-string str " " " "))
         (str (replace-in-string str " " " "))
         (str (replace-in-string str "–" "-"))
         (str (replace-in-string str "−" "-"))
         (str (replace-in-string str "–" "-"))
         (str (replace-in-string str "—" "-"))
         (str (replace-in-string str "‒" "\\\\textemdash{}"))
         (str (replace-in-string str "‘" "'"))
         (str (replace-in-string str "’" "'"))
         (str (replace-in-string str "’" "'"))
         (str (replace-in-string str "“" "\""))
         (str (replace-in-string str "’" "'"))
         (str (replace-in-string str "”" "\"")))
    str))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; end elementary utility functions ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Bib entry utility functions ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun ref-man--preferred-venue (results)
  (if (= 1 (length results))
      0
    (let* ((venues (mapcar (lambda (x) (gscholar-bibtex--xml-get-child x 'venue)) results))
           (prefs (mapcar (lambda (x) (cdr (assoc (car (last x)) ref-man-venue-priorities))) venues)))
      prefs)))

(defun ref-man--validate-author (author)
  (if (or (string-match-p "[0-9]+" (car (last author)))
                  (string-match-p "^i$\\|^ii$\\|^iii$\\|^iv$" (downcase (car (last author)))))
      (if (> (length author) 2) (butlast author) (nconc (butlast author) '("")))
    author))

;;
;; clean the xml entry and keep relevant itmes. uses gscholar-bibtex
;;
(defun ref-man--dblp-clean (result)
  "cleans the xml entry and keeps relevant itmes according to
ref-man-key-list, uses gscholar-bibtex. returns an assoc list of (key. value)
pairs for only the top result from ref-man-venue-priorities"
  (let ((result (nth (max-ind (ref-man--preferred-venue result)) result)))
    ;; TODO: handle this later
    (if result
        (remove '("nil")
                (mapcar
                 (lambda (x)
                   (if (eq x 'authors)
                       (list
                        (symbol-name 'authors)
                        (string-join (mapcar (lambda (x) (car (last x)))
                                             (butfirst (gscholar-bibtex--xml-get-child result x) 2)) ", "))
                     (cons (symbol-name (first (gscholar-bibtex--xml-get-child result x)))
                           (last (gscholar-bibtex--xml-get-child result x)))))
                 ref-man-key-list)))))

;;
;; TODO: crossref and dblp insert URL as dx.doi.org something which
;; redirects to the real url or may not even in some cases. If a pdf
;; url exists, don't mess with it and insert it as doi.
;;
;; 
(defun ref-man--build-bib-key (key-str &optional na)
  "builds a unique key with the format [author year first-title-word]
entry from the list of (key . value)"
  (let* ((first-author-str (car (split-string (ref-man--trim-whitespace (cadr (assoc "authors" key-str))) "," t)))
         (first-author (ref-man--validate-author (split-string first-author-str " " t)))
         (last-name (car (last first-author)))
         (year-pub (ref-man--trim-whitespace (car (cdr (assoc "year" key-str)))))
         (title (remove-if 'ref-man--is-stop-word (split-string (downcase (ref-man--trim-whitespace (cadr (assoc "title" key-str)))) " ")))
         (title-first (car (split-string (first title) "-")))
         (key (ref-man--remove-non-ascii (mapconcat 'downcase (list last-name year-pub title-first) ""))))
    (if na (concat "na_" key) key)))

(defun ref-man--build-bib-key-from-parsed-org-bibtex (bib-assoc)
  "builds a unique key with the format [author year
  first-title-word] entry from the list of (key . value). Trims
  the entries and converts multiple spaces to a single one."
  (let* ((first-author-str (car (split-string (ref-man--trim-whitespace (cdr (assoc :author bib-assoc))) "," t)))
         (first-author (ref-man--validate-author (split-string first-author-str " " t)))
         (last-name (car (last first-author)))
         (year-pub (ref-man--trim-whitespace (cdr (assoc :year bib-assoc))))
         (title (remove-if 'ref-man--is-stop-word (split-string (downcase (ref-man--trim-whitespace (cdr (assoc :title bib-assoc)))) " ")))
         (title-first (car (split-string (first title) "-")))
         (key (ref-man--remove-non-ascii (mapconcat 'downcase (list last-name year-pub title-first) ""))))
    key))

(defun ref-man--build-bib-assoc-from-parsed-org-bibtex (bib-assoc)
  "builds the association list. can be used to build both the bib
entry and org entry"
  (let* ((key (ref-man--build-bib-key-from-parsed-org-bibtex bib-assoc))
         (author (cons "author" (ref-man--trim-whitespace (cdr (assoc :author bib-assoc)))))
         (title (cons "title" (ref-man--trim-whitespace (cdr (assoc :title bib-assoc)))))
         (year (cons "year" (ref-man--trim-whitespace (cdr (assoc :year bib-assoc)))))
         (doi (cons "doi" (cdr (assoc :doi bib-assoc))))
         (volume (cons "volume"  (cdr (assoc :volume bib-assoc))))
         (number (cons "number"  (cdr (assoc :number bib-assoc))))
         (pages  (cons "pages" (cdr (assoc :pages bib-assoc))))
         (publisher  (cons "publisher" (cdr (assoc :publisher bib-assoc))))
         (abstract (cons "abstract" (cdr (assoc :abstract bib-assoc))))
         (url (cons "url" (cdr (assoc :ee bib-assoc))))
         (url (cons "url" (cdr (assoc :ee bib-assoc))))
         (url (if url url (cons "url" (cdr (assoc :url bib-assoc)))))
         (tmp-venue (cdr (assoc :journal bib-assoc))) ;; TODO: expand venue
         (tmp-venue (if tmp-venue tmp-venue (cdr (assoc :booktitle bib-assoc)))) ;; TODO: expand venue
         (tmp-venue (if tmp-venue tmp-venue (cdr (assoc :venue bib-assoc)))) ;; TODO: expand venue
         (venue (cons "venue" tmp-venue)) ;; TODO: expand venue
         (howpublished (cdr (assoc :howpublished bib-assoc)))
         (howpublished (when (and howpublished (> 1 (length (split-string howpublished "{"))))
                           (when (string-match-p "url" (nth 0 (split-string howpublished "{")))
                               (car (split-string (nth 1 (split-string howpublished "{")) "}"))))))
    (list key (remove-if-not 'cdr (list abstract author title year doi volume number pages url venue publisher howpublished)))))

(defun ref-man--build-bib-key-from-parsed-bibtex (bib-assoc)
  "builds a unique key with the format [author year first-title-word]
entry from the list of (key . value). Assumes the strings are all validated"
  (let* ((last-name (car (split-string
                          (car (split-string (ref-man--fix-curly (cdr (assoc "author" bib-assoc))) " and ")) ", ")))
         (year-pub (cdr (assoc "year" bib-assoc)))
         (title (remove-if 'ref-man--is-stop-word (split-string (ref-man--fix-curly (downcase (cdr (assoc "title" bib-assoc)))) " ")))
         (title-first (car (split-string (first title) "-"))))
    (ref-man--remove-non-ascii (mapconcat 'downcase (list last-name year-pub title-first) ""))))

(defun ref-man--build-bib-author (author-str)
  "builds the \"author\" value according to bibtex format"
  (let* ((author-str (ref-man--remove-non-ascii author-str))
         (author-str (replace-in-string (replace-in-string author-str "\\.$" "") ",$" ""))
         (authors (split-string author-str "," t))
         (result-authors
          (mapcar (lambda (x)
                    (let ((temp-auth (ref-man--validate-author (split-string x " " t))))
                      (if (= 1 (length temp-auth)) (car temp-auth)
                        (mapconcat 'identity (list (car (last temp-auth))
                                                   (mapconcat 'identity
                                                              (butlast temp-auth) " ")) ", ")))) authors)))
    (mapconcat 'identity result-authors " and ")))

(defun ref-man--build-vernacular-author (author-str)
  "Builds the \"author\" string as common spoken English. Assumes
  that the input is in bib_author format"
  (let* ((author-str (replace-in-string (replace-in-string author-str "\\.$" "") ",$" ""))
         (authors (split-string author-str " and " t "[ ]+"))
         (result-authors
          (mapcar (lambda (x) (mapconcat 'identity (reverse (split-string x ", ")) " "))
                  authors))
         (result-authors (mapconcat 'identity result-authors " and ")))
    result-authors))

(defun ref-man--build-bib-assoc (key-str &optional na)
  "builds the association list. can be used to build both the bib
entry and org entry"
  (let* ((key (ref-man--build-bib-key key-str na))
         (author (cons "author" (ref-man--build-bib-author
                                 (cadr (assoc "authors" key-str)))))
         (title (cons "title" (cadr (assoc "title" key-str))))
         (year (cons "year" (cadr (assoc "year" key-str))))
         (doi (cons "doi" (cadr (assoc "doi" key-str))))
         (volume (cons "volume" (cadr (assoc "volume" key-str))))
         (number (cons "number" (cadr (assoc "number" key-str))))
         (tmp-pages (cadr (assoc "pages" key-str)))
         (pages (cons "pages" (when tmp-pages
                                (replace-in-string
                                 (replace-in-string tmp-pages "-" "--") " " ""))))
         (url (cons "url" (cadr (assoc "ee" key-str))))
         (venue (cons "venue" (cadr (assoc "venue" key-str)))))
    (list key (remove-if-not 'cdr (list author title year doi volume number pages url venue)))))

;; TODO: What if science-parse is called on multiple pdf files?
;;       For that I'll have to hold all the data in a list
;; TODO: Add async callback with message "DONE" when done
(defun ref-man-get-references ()
  "this is the only entry point to fetch and write the references
to a buffer right now. can change to have it in multiple steps."
  (interactive)
  (setq my/dblp-results nil)            ; TODO: Not used
  (let*
      ((pdf-file-name (expand-file-name (buffer-file-name (current-buffer))))
       (json-string
        (if (string-equal major-mode "pdf-view-mode")
            (shell-command-to-string (format "curl -s -H\
            \"content-type: application/pdf\" --data-binary @%s\
            \"http://localhost:%s/v1\"" pdf-file-name server-port))
          (progn (message "[pdf-refs] not pdf-view-mode") nil)))
       (json-object-type 'hash-table)
       (json-key-type 'string)
       (json-array-type 'list)
       (json-string (if json-string (json-read-from-string json-string) nil))
       ;; concats title and authors for each ref entry for easy lookup
       (refs-list (if json-string
                      (mapcar (lambda (x)
                                (cons (concat (gethash "title" x) " "
                                              (string-join (gethash "authors" x) " ")) x))
                              (gethash "references" json-string)) nil)))
    (if json-string
        (progn
          (setq ref-man--current-pdf-file-name pdf-file-name)
          (setq ref-man--science-parse-data json-string)
          (setq ref-man--document-title (gethash "title" json-string))
          (ref-man--generate-buffer-and-fetch-if-required refs-list))
      (progn (message "[pdf-refs] Empty PDF parse") nil))))

(defun ref-man--generate-org-buffer (&optional visiting-filename)
  "Generated buffer where all the fetch results will be inserted"
  (let ((buf (get-buffer-create
              (if visiting-filename visiting-filename (concat ref-man--document-title "_org"))))
        (win (ref-man--get-or-create-window-on-side)))
    (set-window-buffer win buf)
    (with-current-buffer buf (org-mode)) buf))

(defun ref-man--generate-org-buffer-content (org-buf refs-list bib-assoc visiting-filename)
  (with-current-buffer org-buf
    (ref-man--org-bibtex-write-top-heading-from-assoc bib-assoc)
    (org-insert-heading-after-current)
    (org-demote-subtree)
    (insert "Refs")
    (org-insert-heading-after-current)    
    (org-demote-subtree)
    (ref-man--dblp-fetch-parallel refs-list org-buf)
    (set-visited-file-name visiting-filename)))

;;
;; TODO: Better names please. WTF is `ref-refs`?
;; Maybe python-epc would be better.
;;
(defun ref-man--dblp-fetch-parallel (refs-list org-buf)
  "Fetches all dblp queries in parallel via async"
  (setq ref-man--dblp-fetch-parallel-results nil)
  (loop for ref-refs in refs-list do
        (async-start
         `(lambda ()
            ;; (defun replace-in-string (what with in)
            ;;   (replace-regexp-in-string (regexp-quote what) with in nil 'literal))
            ,(async-inject-variables "ref-refs")
            (let* ((query-url (format "http://dblp.uni-trier.de/search/publ/api?q=%s&format=xml" (car ref-refs)))
                   (buf (url-retrieve-synchronously query-url)))
              (prog2 (with-current-buffer buf (set-buffer-multibyte t))
                  (with-current-buffer buf (buffer-string))
                (kill-buffer buf))))
         `(lambda (buf-string)
            ,(async-inject-variables "ref-refs\\|org-buf\\|ref-man--dblp-fetch-parallel-results")
            (let ((guf (generate-new-buffer "*dblp-test*")))
              (with-current-buffer guf (insert buf-string))
              (with-current-buffer guf (set-buffer-multibyte t))
              (pcase-let ((`(,(and result `(result . ,_)))
                           (xml-parse-region nil nil guf)))
                (let ((key-str (remove nil
                                       (ref-man--dblp-clean
                                        (mapcar (lambda (hit)
                                                  (gscholar-bibtex--xml-get-child hit 'info))
                                                (xml-get-children (gscholar-bibtex--xml-get-child result 'hits) 'hit))))))
                  (with-current-buffer org-buf
                    (if key-str (ref-man--org-bibtex-write-ref-from-assoc (ref-man--build-bib-assoc key-str))
                      (ref-man--org-bibtex-write-ref-NA-from-keyhash (cdr ref-refs))))
                  (kill-buffer guf))))))))

;;
;; Called by ref-man--generate-buffer-and-fetch-if-required
;;
(defun ref-man--dblp-fetch-serial (query)
  "Fetch the dblp data synchronously. Would be used to insert the
top level heading"
  (let* ((query-url (format "http://dblp.uni-trier.de/search/publ/api?q=%s&format=xml" query))
         (buf (url-retrieve-synchronously query-url)))
    (with-current-buffer buf (set-buffer-multibyte t))
    (pcase-let ((`(,(and result `(result . ,_)))
                 (xml-parse-region nil nil buf)))
      (remove nil (ref-man--dblp-clean
                   (mapcar (lambda (hit)
                             (gscholar-bibtex--xml-get-child hit 'info))
                           (xml-get-children (gscholar-bibtex--xml-get-child result 'hits) 'hit)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Org generation and insertion stuff ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (defun my/generate-key-hash-from-science-parse ()
;;   (let ((key-hash (make-hash-table :test 'equal)))
;;     (when (gethash "authors" ref-man--science-parse-data)
;;         (puthash "authors"
;;                  (mapcar (lambda (x) (gethash "name" x)) (gethash "authors" ref-man--science-parse-data)) key-hash))
;;     (when (gethash "year" ref-man--science-parse-data)
;;         (puthash "year" (gethash "year" ref-man--science-parse-data) key-hash))
;;     (when (gethash "title" ref-man--science-parse-data)
;;         (puthash "title" (gethash "title" ref-man--science-parse-data) key-hash))
;;     (when (gethash "venue" ref-man--science-parse-data)
;;         (puthash "venue" (gethash "venue" ref-man--science-parse-data) key-hash))
;;     (when (gethash "pages" ref-man--science-parse-data)
;;         (puthash "pages" (gethash "pages" ref-man--science-parse-data) key-hash))
;;     (when (gethash "volume" ref-man--science-parse-data)
;;         (puthash "volume" (gethash "volume" ref-man--science-parse-data) key-hash))
;;     key-hash))

(defun ref-man--generate-key-str-from-science-parse ()
  (let ((key-str ()))
         (when (gethash "authors" ref-man--science-parse-data)
           (add-to-list 'key-str (cons "authors"
                                       (list (mapconcat (lambda (x) (gethash "name" x))
                                                        (gethash "authors" ref-man--science-parse-data) ", ")))))
         ;; (when (gethash "authors" ref-man--science-parse-data)
         ;;             (add-to-list 'key-str (cons "authors"  (list (mapcar (lambda (x) (gethash "name" x))
         ;;                                            (gethash "authors" ref-man--science-parse-data))))))
         (when (gethash "year" ref-man--science-parse-data)
           (add-to-list 'key-str (cons "year" (list (format "%s"
                                                            (gethash "year" ref-man--science-parse-data))))))
         (when (gethash "title" ref-man--science-parse-data)
             (add-to-list 'key-str (cons "title"  (list (gethash "title" ref-man--science-parse-data)))))
         (when (gethash "venue" ref-man--science-parse-data)
           (add-to-list 'key-str (cons "venue" (list (gethash "venue" ref-man--science-parse-data)))))
        key-str))

;; Fixed: "What if not key-str"
(defun ref-man--generate-buffer-and-fetch-if-required (refs-list)
  (let* ((key-str (ref-man--dblp-fetch-serial  ;; assoc list
                   (concat
                    (replace-regexp-in-string "[^\t\n\r\f -~]" ""
                                              (gethash "title" ref-man--science-parse-data)) " "
                    (string-join (mapcar (lambda (x) (gethash "name" x))
                                         (gethash "authors" ref-man--science-parse-data)) " "))))
         (na (not key-str))
         (key-str (if (not key-str) (ref-man--generate-key-str-from-science-parse) key-str))
         (bib-assoc (ref-man--build-bib-assoc key-str na))
         (filename  (car bib-assoc))
         (visiting-filename
          (path-join (list ref-man-org-store-dir (concat (string-remove-prefix "na_" filename) ".org"))))
         (buf (find-buffer-visiting visiting-filename)))
    (if (not filename)
        (message "[pdf-refs] filename could not be generated!")
      (progn (cond ((and buf (with-current-buffer buf (buffer-string)))
                    (progn (message "[pdf-refs] File is already opened and not empty. Switching")
                           (ref-man--generate-org-buffer (concat filename ".org"))))
                   ((and buf (not (with-current-buffer buf (buffer-string)))
                         (file-exists-p visiting-filename))
                    (with-current-buffer (get-buffer-create (concat filename ".org"))
                      (insert-file-contents visiting-filename t)))
                   ((and (not buf) (file-exists-p visiting-filename))
                    (progn (message "[pdf-refs] File already exists. Opening")
                           (let ((org-buf (ref-man--generate-org-buffer (concat filename ".org"))))
                             (if (not (with-current-buffer org-buf
                                        (insert-file-contents visiting-filename t) (buffer-string)))
                                 (ref-man--generate-org-buffer-content org-buf refs-list bib-assoc visiting-filename)))))
                   ((and (not buf) (not (file-exists-p visiting-filename)))
                    (let ((org-buf (ref-man--generate-org-buffer (concat filename ".org"))))
                      (ref-man--generate-org-buffer-content org-buf refs-list bib-assoc visiting-filename))))))))

(defun ref-man--org-bibtex-write-top-heading-from-assoc (entry)
  "Generate the top level org entry for data parsed with science-parse."
  (let* ((key (car entry))
         (entry (nth 1 entry)))
    (org-insert-heading)
    (insert (cdr (assoc "title" entry)))
    (insert "\n")
    (org-indent-line)
    (insert (gethash "abstractText" ref-man--science-parse-data)) ;; Hack to get abstractText
    (fill-paragraph)
    (org-insert-property-drawer)
    (loop for ent in entry
          do
          (if (not (string-equal (car ent) "abstract"))
              (org-set-property (upcase (car ent)) (ref-man--fix-curly (cdr ent)))))
    (org-set-property "CUSTOM_ID" key)
    (org-set-property "BTYPE" "article")
    (org-set-property "PDF-FILE" (concat "[[" ref-man--current-pdf-file-name "]]"))))

(defun ref-man--generate-NA-entry (key-hash)
  (if (and (gethash "title" key-hash) (gethash "authors" key-hash))
      (let* ((key (mapconcat (lambda (x) (replace-in-string (downcase x) " " ""))
                             (list "na" "_"
                                   (let ((first-author (split-string (car (gethash "authors" key-hash)) " ")))
                                     (if (= 1 (length first-author)) (car first-author)
                                       (nth 1 first-author)))
                                   (if (gethash "year" key-hash) (format "%s" (gethash "year" key-hash)) "_")
                                   (car (split-string (gethash "title" key-hash) " " t))) ""))
             (author '("author" . nil))
             (author (cons "author" (ref-man--build-bib-author
                                     (string-join (gethash "authors" key-hash) ", "))))
             (title (cons "title" (gethash "title" key-hash)))
             (volume (cons "volume" (gethash "volume" key-hash)))
             (number (cons "number" (gethash "number" key-hash)))
             (tmp-pages (cons "pages" (gethash "pages" key-hash)))
             (pages (cons "pages" (when tmp-pages
                                  (replace-in-string
                                   (replace-in-string (format "%s" tmp-pages) "-" "--") " " ""))))
             (year (cons "year" (format "%s" (gethash "year" key-hash))))
             (venue (cons "venue" (when (gethash "venue" key-hash)
                                      (replace-in-string (gethash "venue" key-hash) ",$" ""))))
             (entry (list key (remove-if-not 'cdr (list author title year venue volume number pages)))))
        entry)))

(defun ref-man--org-bibtex-write-ref-NA-from-keyhash (key-hash)
  (ref-man--org-bibtex-write-ref-from-assoc (ref-man--generate-NA-entry key-hash)))

(defun ref-man--org-bibtex-write-ref-from-assoc-misc (entry)
    (org-insert-heading-after-current)
    (insert (cdr (assoc :title entry)))
    (insert "\n")
    (org-insert-property-drawer)
    (loop for ent in entry
          do
          (when (not (string-equal (symbol-name (car ent)) ":type"))
            (org-set-property (upcase (car (cdr (split-string (symbol-name (car ent)) ":"))))
                              (cdr ent)))))

(defun ref-man--org-bibtex-write-ref-from-assoc-permissive (entry)
  "Generate an org entry from an association list retrieved via
json."
  (let* ((key (car entry))
         (entry (nth 1 entry)))
    (org-insert-heading-after-current)
    (insert (cdr (assoc "title" entry)))
    (insert "\n")
    (when (assoc "author" entry)
      (org-indent-line)
      (insert (format "- Authors: %s\n" (ref-man--remove-non-ascii (cdr (assoc "author" entry))))))
    (when (and (assoc "venue" entry) (assoc "year" entry))
      (org-indent-line)
      (insert (format "- %s\n" (concat (cdr (assoc "venue" entry)) ", " (cdr (assoc "year" entry))))))
    (when (assoc "howpublished" entry)
      (org-indent-line)
      (insert (format "- %s\n" (concat "Published as: " (cdr (assoc "venue" entry))))))
    (org-insert-property-drawer)
    (loop for ent in entry
          do
          (when (not (string-equal (car ent) "abstract"))
            (org-set-property (upcase (car ent))
                              (ref-man--remove-non-ascii (ref-man--fix-curly (cdr ent))))))
    (org-set-property "CUSTOM_ID" key)
    (if (string-equal (assoc :type bib-assoc) "misc")
        (org-set-property "BTYPE" "misc")
      (org-set-property "BTYPE" "article"))))

(defun ref-man--org-bibtex-write-ref-from-assoc (entry)
  "Generate an org entry from an association list retrieved via
json."
  (let* ((key (car entry))
         (entry (nth 1 entry)))
    (org-insert-heading-after-current)
    (insert (cdr (assoc "title" entry)))
    (insert "\n")
    (org-indent-line)
    (when (assoc "abstract" entry)
        (insert (cdr (assoc "abstract" entry)))
        (fill-paragraph)
        (insert "\n")
        (org-indent-line))
    (insert (format "- Authors: %s"
                    (ref-man--build-vernacular-author (ref-man--remove-non-ascii (cdr (assoc "author" entry))))))
    (org-insert-item)
    (insert (concat (cdr (assoc "venue" entry)) ", " (cdr (assoc "year" entry))))
    (org-insert-property-drawer)
    (loop for ent in entry
          do
          (when (not (string-equal (car ent) "abstract"))
            (org-set-property (upcase (car ent))
                              (ref-man--remove-non-ascii (ref-man--fix-curly (cdr ent))))))
    (org-set-property "CUSTOM_ID" key)
    (org-set-property "BTYPE" "article")))

(defun ref-man--org-bibtex-write-heading-from-bibtex (entry)
  "Generate an org entry from a bibtex association list, parsed
with 'bibtex from a bibtex entry"
  (org-insert-heading)
  (insert (ref-man--fix-curly (cdr (assoc "title" entry))))
  (insert "\n")
  ;; from where do I get the abstract?
  ;; assuming abstract is in the bib entry
  (when (assoc "abstract" entry)
      (insert (ref-man--fix-curly (cdr (assoc "abstract" entry))))
      (org-indent-line)
      (fill-paragraph))
  (org-insert-property-drawer)
  (loop for ent in entry
        do
        (pcase ent
          (`("abstract" . ,_))
          (`("=type=" . ,_) (org-set-property "BTYPE" (ref-man--fix-curly (cdr ent))))
          (`("=key=" . ,_) (org-set-property "CUSTOM_ID" (ref-man--fix-curly (cdr ent))))
          (`(,_ . ,_) (org-set-property (upcase (car ent)) (ref-man--fix-curly (cdr ent)))))))

(defun ref-man-org-bibtex-read-bib-file-to-buffer (filename &optional buffername)
  (interactive "sFile: ")
  (if (file-exists-p filename)
      (let* ((buffername
             (if buffername buffername (replace-in-string filename "\\.[a-z0-9]*?$" ".org")))
             (org-buf (ref-man--generate-org-buffer buffername)))
        (setq org-bibtex-entries nil)
        (org-bibtex-read-file filename)
        (with-current-buffer org-buf
          (org-insert-heading)
          (loop for entry in org-bibtex-entries
                do
                (progn
                  (if (string-equal (cdr (assoc :type entry)) "misc")
                      (ref-man--org-bibtex-write-ref-from-assoc-misc entry)
                    (ref-man--org-bibtex-write-ref-from-assoc
                     (ref-man--build-bib-assoc-from-parsed-org-bibtex entry)))))
          (goto-char (point-min))
          (delete-char 3)))
    (message (format "[pdf-refs] File %s does not exist" filename))))

(defun ref-man-org-bibtex-read-from-headline ()
  (interactive)
  (when (string-equal major-mode "org-mode")
    (let* ((props (org-entry-properties))
           (bib-str (list
                     (cons "type"  (concat "@" (cdr (assoc "BTYPE" props))))
                     (cons "key"  (cdr (assoc "CUSTOM_ID" props)))
                     (cons "title"  (cdr (assoc "TITLE" props)))
                     (cons "author"  (cdr (assoc "AUTHOR" props)))
                     (cons "venue"  (cdr (assoc "VENUE" props)))
                     (cons "booktitle"  (cdr (assoc "BOOKTITLE" props)))
                     (cons "volume"  (cdr (assoc "VOLUME" props)))
                     (cons "number"  (cdr (assoc "NUMBER" props)))
                     (cons "year"  (cdr (assoc "YEAR" props)))
                     (cons "pages"  (cdr (assoc "PAGES" props)))
                     (cons "doi"  (cdr (assoc "DOI" props)))
                     (cons "url"  (cdr (assoc "URL" props)))
                     (cons "publisher"  (cdr (assoc "PUBLISHER" props)))
                     (cons "organization"  (cdr (assoc "ORGANIZATION" props)))))
           (header (concat (cdr (assoc "type" bib-str)) "{" (cdr (assoc "key" bib-str)) ",\n"))
           (bib-str (delq (assoc "type" bib-str) bib-str))
           (bib-str (delq (assoc "key" bib-str) bib-str))
           (bib-str (concat header
                            (mapconcat (lambda (x)
                                         (if (cdr x) (concat "  " (car x) "={" (cdr x) "},\n")))
                                       bib-str "") "}\n")))
      (if (called-interactively-p 'any)
          (kill-new bib-str) bib-str))
    (message "[pdf-refs] Not org mode")))

(defun ref-man-org-bibtex-insert-headline-as-bib-to-file (&optional file)
  "Export current headline to kill ring as bibtex entry."
  (interactive)
  (let* ((result (ref-man-org-bibtex-read-from-headline))
        (temp-bib-file-name (file-name-nondirectory ref-man-temp-bib-file-path))
        (buf (if (get-buffer temp-bib-file-name) (get-buffer temp-bib-file-name)
               (find-file-noselect ref-man-temp-bib-file-path))))
    (with-current-buffer buf (goto-char (point-min))
                         (insert result))))

(defun ref-man--org-bibtex-convert-bib-to-property (assoc-list &optional buf)
  (let ((buf (if buf buf (current-buffer)))
        (entry assoc-list)
        (buf-point (if (not ref-man--org-heading-gscholar-launch-point)
                       (with-current-buffer buf (point))
                     ref-man--org-heading-gscholar-launch-point)))
    (with-current-buffer buf
      (goto-char buf-point)
      (when (cdr (assoc "title" assoc-list))
        (org-edit-headline (ref-man--fix-curly (cdr (assoc "title" assoc-list)))))
      (loop for ent in entry
            do
            (pcase ent
              (`("abstract" . ,_))
              (`("=type=" . ,_) (org-set-property "BTYPE" (ref-man--fix-curly (cdr ent))))
              (`("=key=" . ,_) (org-set-property "CUSTOM_ID" (ref-man--fix-curly (cdr ent))))
              (`(,_ . ,_) (org-set-property (upcase (car ent)) (ref-man--fix-curly (cdr ent)))))))))

(defun ref-man--sanitize-org-entry (&optional org-buf)
  (let (retval)
    (condition-case ex
        (setq retval
              (let ((org-buf (cond (org-buf org-buf)
                                   (ref-man--org-gscholar-launch-buffer ref-man--org-gscholar-launch-buffer)
                                   (ref-man--org-heading-gscholar-launch-buffer ref-man--org-heading-gscholar-launch-buffer)))
                    (org-point (cond ((and ref-man--org-gscholar-launch-buffer ref-man--org-gscholar-launch-point)
                                      ref-man--org-gscholar-launch-point)
                                     ((and ref-man--org-heading-gscholar-launch-buffer
                                           ref-man--org-heading-gscholar-launch-point)
                                       ref-man--org-heading-gscholar-launch-point)
                                      (org-buf (with-current-buffer org-buf (point)))
                                      (t nil))))
                (cond
                 ((not org-buf) "No suitable org buffer found")
                 ((not (equal (with-current-buffer org-buf major-mode)
                              'org-mode)) "Not org mode")
                 ((not (with-current-buffer org-buf
                         (org-entry-get org-point "CUSTOM_ID")))
                  (with-current-buffer org-buf
                    (org-set-property "CUSTOM_ID" "na_"))
                  "No property drawer or no property. Fixed")
                 (t (with-current-buffer org-buf
                      (org-entry-get org-point "CUSTOM_ID"))))))
      ('error (message (format "[pdf-refs] Caught exception: [%s]" ex))))
    (message (concat "[pdf-refs] " retval))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; End Org generation and insertion stuff ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;
;; Begin Biblio stuff ;;
;;;;;;;;;;;;;;;;;;;;;;;;
(defun org-search-heading-on-crossref-with-biblio ()
  "Searches for the current heading in google scholar in eww"
  (interactive)
  (setq ref-man--org-heading-gscholar-launch-point (point))
  (setq ref-man--org-heading-gscholar-launch-buffer (current-buffer))
  (save-excursion
    (let* ((query (org-get-heading t t))
           (target-buffer (window-buffer (ref-man--get-or-create-window-on-side)))
           (backend #'biblio-crossref-backend)
           (results-buffer (biblio--make-results-buffer target-buffer query backend)))
      (biblio-url-retrieve
       (funcall backend 'url query)
       (ref-man--biblio-callback results-buffer backend))
      results-buffer)))

(defun ref-man--biblio-callback (results-buffer backend)
  "Generate a search results callback for RESULTS-BUFFER.
Results are parsed with (BACKEND 'parse-buffer)."
  ;; TODO: Did let not work here?
  ;;       Gotta check.
  (setq my/biblio-callback-buf results-buffer)
  (biblio-generic-url-callback
   (lambda () ;; no allowed errors, so no arguments
     "Parse results of bibliographic search."
     (let ((results (biblio--tag-backend 'biblio-crossref-backend
                                         (funcall 'biblio-crossref-backend 'parse-buffer)))
           (win (ref-man--get-or-create-window-on-side)))
       (with-current-buffer my/biblio-callback-buf
         (ref-man--biblio-insert-results results (biblio--search-results-header))
         (local-set-key (kbd "n") 'biblio--selection-next)
         (local-set-key (kbd "p") 'biblio--selection-previous)
         (local-set-key (kbd "o") 'ref-man--parse-selected-entry-to-org))
       (set-window-buffer win my/biblio-callback-buf)
       (message "[pdf-refs] Tip: learn to browse results with `h'")))))

(defun ref-man--parse-selected-entry-to-org ()
  (interactive)
  (biblio--selection-forward-bibtex #'ref-man--biblio-insert-to-org))

(defun ref-man--biblio-insert-to-org (bibtex entry)
  (let* ((current-key (with-current-buffer ref-man--org-heading-gscholar-launch-buffer
                       (org-entry-get ref-man--org-heading-gscholar-launch-point "CUSTOM_ID")))
         (bibtex (replace-in-string (replace-in-string
                  (progn (set-text-properties 0 (length bibtex) nil bibtex) bibtex)
                  "\n" "") "[[:blank:]]+" " "))
        (bib-assoc (with-temp-buffer (insert bibtex)
                                     (goto-char (point-min))
                                     (bibtex-parse-entry)))
        (new-key (ref-man--build-bib-key-from-parsed-bibtex bib-assoc))
        (bib-assoc (remove-if 'ref-man--is-bibtex-key bib-assoc)))
    (setf (alist-get "=key=" bib-assoc) new-key)
    (cond ((not bib-assoc) (message "[pdf-refs] Received nil entry"))
          ((string-match-p "na_" current-key)
           (ref-man--org-bibtex-convert-bib-to-property bib-assoc ref-man--org-heading-gscholar-launch-buffer))
          ((y-or-n-p "Authoritative entry. Really replace?")
           (ref-man--org-bibtex-convert-bib-to-property bib-assoc ref-man--org-heading-gscholar-launch-buffer)))
    (pop-to-buffer ref-man--org-heading-gscholar-launch-buffer)))

(defun ref-man--biblio-insert-results (items &optional header)
  "Populate current buffer with ITEMS and HEADER, then display it."
  (let ((inhibit-read-only t))
    (erase-buffer)
    (biblio--insert-header header)
    (seq-do #'biblio-insert-result items))
  (biblio--selection-first)
  (hl-line-highlight))

;;;;;;;;;;;;;;;;;;;;;;
;; End Biblio stuff ;;
;;;;;;;;;;;;;;;;;;;;;;



(defun ref-man--eww-mode-hook ()
  (bind-key "b" 'ref-man-eww-keypress-b eww-mode-map)
  (bind-key "b" 'ref-man-eww-keypress-b eww-link-keymap)  
  (bind-key "i" 'ref-man-eww-keypress-i eww-mode-map)
  (bind-key "i" 'ref-man-eww-keypress-i eww-link-keymap)
  (bind-key "v" 'ref-man-eww-keypress-v eww-mode-map)
  (bind-key "v" 'ref-man-eww-keypress-v eww-link-keymap)
  (bind-key "d" 'ref-man-eww-keypress-d eww-mode-map)
  (bind-key "d" 'ref-man-eww-keypress-d eww-link-keymap)
  (bind-key "n" 'next-line eww-mode-map)
  (bind-key "p" 'previous-line eww-mode-map)
  (bind-key "]" 'ref-man-eww-next eww-mode-map)
  (bind-key "[" 'ref-man-eww-previous eww-mode-map))

(add-hook 'eww-mode-hook 'ref-man--eww-mode-hook)

(global-set-key (kbd "C-c e e") 'eww)
(global-set-key (kbd "C-c e g") 'ref-man-eww-gscholar)
(setq browse-url-browser-function 'eww-browse-url)
(setq ref-man--gscholar-launch-buffer-list nil)

;; (setq url-user-agent "User-Agent: Mozilla/5.0 (X11; Linux x86_64; rv:46.0) Gecko/20100101 Firefox/46.0")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; BEGIN string utility functions ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun ref-man--non-gscholar-url-p (url)
  (and (not (string-match-p "javascript" url))
       (not (string-match-p "scholar" url))
       (not (string-match-p "google" url))))

(defun ref-man--filter-eww-non-gscholar (url-list)
  (remove-if
   (lambda (x) (string-match-p "javascript" x))
   (remove-if
    (lambda (x) (string-match-p "scholar" x))
    (remove-if
     (lambda (x) (string-match-p "google.com" x))
     url-list))))

(defun ref-man--downloadable-pdf-url-p (url)
  "Does the given url contain a pdf to download"
  (and (ref-man--non-gscholar-url-p url)
       ;; add exception for arxiv.org
       (cond ((string-match-p "arxiv.org" url)
              (string-match-p "/pdf/" url))
             ((string-match-p "aaai.org" url)
              (string-match-p "/download/" url))
             ((string-match-p "openreview.net" url)
              (string-match-p "pdf" url))
             (t (string-match-p "\\.pdf" url)))))

(defun ref-man--url-matches-filename-p (url filename)
  (let* ((obj (url-generic-parse-url url))
         (path (car (url-path-and-query obj))))
         (string-match-p filename path)))

(defun ref-man--filename-from-url (url)
  (let* ((obj (url-generic-parse-url url))
         ;; (path (car (url-path-and-query obj)))
         (path (cond ((string-match-p "openreview" url)
                      (concat "openreview_"
                              (nth 1 (split-string
                                   (cdr (url-path-and-query
                                         (url-generic-parse-url url))) "="))
                              "." "pdf"))
                     ((string-match-p "aaai.org" url)
                      (concat "aaai_" (string-join (last (split-string url "/") 2) "_") ".pdf"))
                     (t (car (url-path-and-query obj)))))
         (file (path-join (list ref-man-pubs-directory (file-name-nondirectory path)))))
    file))

(defun ref-man--check-pdf-file-exists (url)
  (let* ((file-name-a (ref-man--filename-from-url url))
         (file-name-b (concat file-name-a ".pdf")))
    (cond ((file-exists-p file-name-a) file-name-a)
          ((file-exists-p file-name-b) file-name-b)
          (t nil))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; END utility functions ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; BEGIN eww navigation and keymap ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; use gscholar specific bindings only on google scholar pages
(defun ref-man-eww-previous ()
  (interactive)
  (let ((url (with-current-buffer (get-buffer "*eww*") (plist-get eww-data :url))))
    (if (string-match-p "scholar\\.google\\.com" url)
        (catch 'retval
          (save-excursion
            (goto-char (point-min))
            (while (search-forward "Previous")
              (let ((url (get-text-property (- (point) 1) 'shr-url)))
                ;; (if (string-match-p "scholar.*start=" url)
                ;;     (progn (eww-browse-url url) (message "Going to Previous Page")
                ;;            (throw 'retval t)))
                (when (string-match-p "scholar.*start=" url)
                  (eww-browse-url url) (message "Going to Previous Page")
                  (throw 'retval t))))))
      (eww-previous-url))))

(defun ref-man-eww-next ()
  (interactive)
  (let ((url (with-current-buffer (get-buffer "*eww*") (plist-get eww-data :url))))
    (if (string-match-p "scholar\\.google\\.com" url)
        (catch 'retval
          (save-excursion
            (goto-char (point-min))
            (while (search-forward "Next")
              (let ((url (get-text-property (- (point) 1) 'shr-url)))
                (when (and url (string-match-p "scholar.*start=" url))
                  (eww-browse-url url) (message "Going to Next Page")
                  (throw 'retval t))))))
                ;; (if (and url (string-match-p "scholar.*start=" url))
                ;;     (progn (eww-browse-url url) (message "Going to Next Page")
                ;;            (throw 'retval t)))
      (eww-next-url))))

;; CHECK: import link to org-buffer doesn't attach the pdf file automatically
;;       if it's already downloaded.
;;       Same should be there for import first and download later.
;;       However, link to the article and link to pdf may not be the same in general
(defun ref-man-eww-keypress-i ()
  (interactive)
  (let ((url (with-current-buffer (get-buffer "*eww*") (plist-get eww-data :url))))
    (if (string-match-p "scholar\\.google\\.com" url) ; Only imports from scholar for now
        (ref-man-import-gscholar-link-to-org-buffer (get-text-property (point) 'shr-url))
      (eww-view-source))))

(defun ref-man-eww-keypress-v ()
  "View and download if required url. If no url under point get
the first one from top"
  (interactive)
  ;; assuming already in eww or shr mode
  (let ((url (get-text-property (point) 'shr-url)))
    (if (and url (ref-man--downloadable-pdf-url-p url))
        (ref-man-eww-view-and-download-if-required-pdf url)
      (eww-view-source))))

(defun ref-man-eww-keypress-c ()
  "Cycle between pdf (or perhaps other predicated) links in the eww buffer.
Goes to the next pdf link and cycles round if the last link is reached."
  (interactive)
  ;; (if current-prefix-arg)
  (let* ((buf (get-buffer "*eww*"))
         (url (with-current-buffer buf (plist-get eww-data :url)))
         (links (if (string-match-p "\\.arxiv\\.org" url)
                    (ref-man-eww-get-all-links buf nil nil "pdf")
                  (ref-man-eww-get-all-links buf nil nil "\\.pdf"))))))
 
(defun ref-man-eww-keypress-b (&optional org-buf)
  (interactive (list (if (and current-prefix-arg (not (boundp 'org-buf)))
                         (completing-read "Org buffer: " (mapcar (lambda (x) (format "%s" x)) (buffer-list)))
                       nil)))
  (let ((url (with-current-buffer (get-buffer "*eww*") (plist-get eww-data :url))))
    (if (string-match-p "scholar\\.google\\.com" url)
        (ref-man-eww-get-bibtex-from-scholar org-buf)
      (eww-add-bookmark))))

(defun ref-man-eww-keypress-d ()
  (interactive)
  ;; was here for scholar.google.com checking
  ;; * ((url (with-current-buffer (get-buffer "*eww*") (plist-get eww-data :url)))
  (let ((url (get-text-property (point) 'shr-url)))
    (if (and url (ref-man--downloadable-pdf-url-p url))
        (ref-man-eww-download-pdf url)
      (ref-man-eww-download-pdf url))
    (eww-download)))

;;;;;;;;;;;;;;;;;;;;;;;;
;; END eww navigation ;;
;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Begin eww utility functions ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun ref-man--get-non-google-url (buf pdf-url)
  "For the corresponding url which is the url for the pdf
download, get the link which corresponds to it"
  (let ((all-urls (ref-man--filter-eww-non-gscholar
                   (ref-man-eww-get-all-links buf t nil nil)))) ; (from eww buffer) from-begin
    (nth (+ (-elem-index pdf-url all-urls) 1) all-urls)))

(defun ref-man--eww-get-import-link-data (buf link)
  "Extracts link, its text and corresponding metadata from an eww buffer"
  (save-excursion
    (setq ref-man--eww-import-link link)
    (if link
        (with-current-buffer buf
          (let*
              ((link-text-begin (progn (when (not (equal link (get-text-property (point) 'shr-url)))
                                         (while (and (not (equal link (get-text-property (point) 'shr-url)))
                                                     (not (bobp)))
                                           (backward-char)))
                                       (while (equal link (get-text-property (point) 'shr-url))
                                         (backward-char))
                                       (forward-char) (point)))
               (link-text-end (progn ; (goto-char link-text-begin)
                                (while (equal link (get-text-property (point) 'shr-url))
                                  (forward-char))
                                (point)))
               (metadata (progn (goto-char link-text-end)
                                (forward-line 2)
                                (with-current-buffer buf
                                  (buffer-substring-no-properties
                                   (point-at-bol) (point-at-eol)))))
               (link-text (buffer-substring-no-properties link-text-begin link-text-end)))
            (list link link-text metadata))))))

(defun ref-man--eww-check-bibtex-buffer-from-scholar ()
  "Checks if the *Import from bibtex* buffer contains valid data or not"
  (let* ((buf (get-buffer " *scholar-entry*"))
         (buf-string (if buf (with-current-buffer buf (buffer-string))
                       (message "[ref-man] Could not create buffer for scholar entry") nil)))
    (if buf-string (cond ((string-match-p "systems have detected unusual" buf-string)
                          (message "[ref-man] Scholar is detecting a robot"))
                         ((string-match-p "client does not have permission" buf-string)
                          (message "[ref-man] Scholar doesn't like EWW"))
                         (t buf-string))
      (message "[ref-man] Empty reply from scholar") nil)))

;; ONLY called from ref-man-eww-get-bibtex-from-scholar
(defun ref-man--eww-browse-url (url org)
  (let ((buf (get-buffer-create " *scholar-entry*")))
    (with-current-buffer buf (eww-setup-buffer)
                         (plist-put eww-data :url url)
                         (plist-put eww-data :title "")
                         (eww-update-header-line-format)
                         (let ((inhibit-read-only t))
                           (goto-char (point-min)))
                         (url-retrieve url #'ref-man--eww-render
                                       (list url (current-buffer) org)))))

;; FIXED: Fix this! ref-man--org-heading-gscholar-launch-point is used only
;;       for inserting bibtex. The other variable
;;       ref-man--org-gscholar-launch-buffer is used to insert children and
;;       other stuff and is used in other functions - They can cause
;;       confusion.
;; TODO: Have changed some of my/org-*gscholar*launch* variables
;;       Should change the rest.
;; TODO: Why does it save my/bibtex-entry?
(defun ref-man--eww-render (status url buf org)
  (eww-render status url nil buf)
  (let ((buf-string (ref-man--eww-check-bibtex-buffer-from-scholar)))
    (if buf-string
        (progn
          (setq my/bibtex-entry buf-string) ; TODO: Not used
          (message "Saved bibtex to my/bibtex-entry")
          ;; Sanitize org entry and insert into org buffer
          (let ((org-buf (cond ((stringp org) (get-buffer org))
                               (ref-man--org-gscholar-launch-buffer ref-man--org-gscholar-launch-buffer)
                               (t nil))))
            (if org-buf
                (progn
                  (message (concat "Trying to insert into org buffer: "
                                   (buffer-name org-buf)))
                  (ref-man--sanitize-org-entry org-buf)
                  (let ((bib-assoc (with-current-buffer buf (goto-char (point-min)) (bibtex-parse-entry)))
                        (current-key (with-current-buffer org-buf
                                       (org-entry-get (point) "CUSTOM_ID"))))
                    (cond ((string-match-p "na_" current-key)
                           (ref-man--org-bibtex-convert-bib-to-property
                            bib-assoc org-buf))
                          ((y-or-n-p "Authoritative entry. Really replace?")
                           (ref-man--org-bibtex-convert-bib-to-property
                            bib-assoc org-buf)))))
              (let* ((temp-bib-file-name (file-name-nondirectory ref-man-temp-bib-file-path))
                     (buf (if (get-buffer temp-bib-file-name) (get-buffer temp-bib-file-name)
                            (find-file-noselect ref-man-temp-bib-file-path))))
                (with-current-buffer buf (goto-char (point-min))
                                     (insert buf-string))
                (message (concat "[ref-man] inserted bib entry into " temp-bib-file-name))))))
      (message "[ref-man] Could not get entry from scholar"))
    (when buf (kill-buffer buf))))

(defun ref-man--eww-gscholar-get-next-non-google-link (buf)
  (ref-man--eww-gscholar-get-non-google-link buf nil))

(defun ref-man--eww-gscholar-get-previous-non-google-link (buf)
  (ref-man--eww-gscholar-get-non-google-link buf t))

(defun ref-man--eww-gscholar-get-non-google-link (buf previous)
  "The buffer is assumed to be gscholar. The function tries to
import the *next* non-google link"
  (save-excursion
    (with-current-buffer buf
      (let ((step (if previous -1 1)))
        (if (and (get-text-property (point) 'shr-url)
                 (ref-man--non-gscholar-url-p (get-text-property (point) 'shr-url))) ; (point) has link
            (get-text-property (point) 'shr-url)
          (while (and (not (bobp))
                      (not (eobp))
                      (not (if (get-text-property (point) 'shr-url)
                               (ref-man--non-gscholar-url-p (get-text-property (point) 'shr-url)))))
            (forward-char step))
          (get-text-property (point) 'shr-url))))))

(defun ref-man--eww-gscholar-get-previous-pdf-link (buf)
  (ref-man--eww-gscholar-get-pdf-link buf t))

(defun ref-man--eww-gscholar-get-next-pdf-link (buf)
  (ref-man--eww-gscholar-get-pdf-link buf nil))

(defun ref-man--eww-gscholar-get-pdf-link (buf previous)
  "The buffer is assumed to be gscholar. The function tries to
import the *next* non-google link"
  (save-excursion
    (with-current-buffer buf
      (let ((step (if previous -1 1)))
        (if (and (get-text-property (point) 'shr-url)
                 (ref-man--downloadable-pdf-url-p (get-text-property (point) 'shr-url))) ; (point) has link
            (get-text-property (point) 'shr-url)
          (while (and (not (eobp))
                      (not (if (get-text-property (point) 'shr-url)
                               (ref-man--downloadable-pdf-url-p (get-text-property (point) 'shr-url)))))
            (forward-char step))
          (get-text-property (point) 'shr-url))))))

;; DEPRECATED
(defun ref-man--eww-get-gscholar-link-for-import (buf)
  "As the buffer is assumed to be gscholar, it tries to import
the *previous* non-google link"
  (save-excursion
    (with-current-buffer buf
      (if (and (get-text-property (point) 'shr-url)
               (ref-man--non-gscholar-url-p (get-text-property (point) 'shr-url))) ; (point) has link
          (get-text-property (point) 'shr-url)
        (while (and (not (bobp))
                    (not (if (get-text-property (point) 'shr-url)
                             (ref-man--non-gscholar-url-p (get-text-property (point) 'shr-url)))))
          (backward-char 1))
        (get-text-property (point) 'shr-url)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; End eww utility functions ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Begin eww callable functions ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; This is the big function
;; If eww is called from any other place, set the
;; ref-man--org-heading-gscholar-launch-buffer and
;; ref-man--org-heading-gscholar-launch-point to nil
(defun ref-man-eww-gscholar (url)
  "Fetch URL and render the page.
If the input doesn't look like a URL or a domain name."
  (interactive
   (let* ((uris (eww-suggested-uris))
	  (prompt (concat "Enter URL or keywords"
			  (if uris (format " (default %s)" (car uris)) "")
			  ": ")))
     (list (read-string prompt nil nil uris))))
  (if (eq major-mode 'org-mode)
      (progn (setq ref-man--org-gscholar-launch-buffer (current-buffer))
             (setq ref-man--org-gscholar-launch-point (point)))
    (setq ref-man--org-gscholar-launch-buffer nil))
  (setq url (string-trim url))
  (cond ((string-match-p "\\`file:/" url))
	;; Don't mangle file: URLs at all.
        ((string-match-p "\\`ftp://" url)
         (user-error "FTP is not supported"))
        (t
	 ;; Anything that starts with something that vaguely looks
	 ;; like a protocol designator is interpreted as a full URL.
         (if (or (string-match "\\`[A-Za-z]+:" url)
		 ;; Also try to match "naked" URLs like
		 ;; en.wikipedia.org/wiki/Free software
		 (string-match "\\`[A-Za-z_]+\\.[A-Za-z._]+/" url)
		 (and (= (length (split-string url)) 1)
		      (or (and (not (string-match-p "\\`[\"'].*[\"']\\'" url))
			       (> (length (split-string url "[.:]")) 1))
			  (string-match eww-local-regex url))))
             (progn
               (unless (string-match-p "\\`[a-zA-Z][-a-zA-Z0-9+.]*://" url)
                 (setq url (concat "http://" url)))
               ;; Some sites do not redirect final /
               (when (string= (url-filename (url-generic-parse-url url)) "")
                 (setq url (concat url "/"))))
           (progn (setq query-string url)
                  (setq url (concat "https://scholar.google.com/scholar?q="
                                    (replace-regexp-in-string " " "+" url)))))))
  ;; FIXME: Fix this code!
  ;; CHECK: Did I write the chrome code for nothing?
  (if (string-match-p "scholar\\.google\\.com" url)
      (let ((buf (generate-new-buffer " *scholar*")))
        (with-current-buffer buf (insert (gscholar-bibtex-google-scholar-search-results
                                          query-string)))
        (if (get-buffer "*google-scholar*")
            (kill-buffer (get-buffer "*google-scholar*")))
        (when (get-buffer "*html*")
          (with-current-buffer (get-buffer "*html*")
            (setq-local buffer-read-only nil)))
        (shr-render-buffer buf)
        (pop-to-buffer-same-window "*html*")
        (rename-buffer "*google-scholar*")
        (kill-buffer buf))
    (progn
      (pop-to-buffer-same-window
       (if (eq major-mode 'eww-mode)
           (current-buffer)
         (get-buffer-create "*eww*")))
      (eww-setup-buffer)
      (plist-put eww-data :url url)
      (plist-put eww-data :title "")
      (eww-update-header-line-format)
      (let ((inhibit-read-only t))
        (insert (format "Loading %s..." url))
        (goto-char (point-min)))
      (url-retrieve url 'eww-render
		    (list url nil (current-buffer))))))

;; Much cleaner code now
;; Although, I think I'll remove the debug code later
;; TODO: if the cursor is on a link, I get two pdf links
;; TODO: ref-man-eww-keypress-d and maybe some others don't work
;;       as expected. The keypresses on regions of google-scholar
;;       which are not links, give me incorrect results.
(defun ref-man-eww-get-all-links (&optional buf frombegin before-point substring)
  "Get all links from given buffer :buf (defaults to *eww*) with the given options
:frombegin gets all the links in the buffer
:before-point restricts the search to current point otherwise till eob
:substring filters the urls by substring regexp"
  (interactive)
  (save-excursion
    (let ((buf (if buf buf (get-buffer "*eww*"))))
      (with-current-buffer buf            ; should be an shr buffer; usually *eww*
        (if before-point (setq ref-man--eww-buffer-endpoint (point))
          (setq ref-man--eww-buffer-endpoint (buffer-end 1)))
        (if frombegin (goto-char (point-min)))
        (setq ref-man--eww-buffer-links nil)
        ;; Below was ref-man--egal--current-url
        (setq ref-man--egal--prev-url (get-text-property (point) 'shr-url))
        (setq ref-man--egal--current-url nil)
        (setq ref-man--egal--url-text-start (point))
        (setq ref-man--egal--url-text-end (point))
        (while (< (point) ref-man--eww-buffer-endpoint)
          ;; (debug)    
          ;; Debug info
          ;; (message (concat (format "%s" ref-man--egal--url-text-start) ", "
          ;; (format "%s" ref-man--egal--url-text-end)))
          ;; (message (format "%s" (string-match-p substring
          ;; (buffer-substring-no-properties ref-man--egal--url-text-start ref-man--egal--url-text-end))))
          (when (and ref-man--egal--prev-url
                     (stringp ref-man--egal--prev-url))
            (if substring
                (when (string-match-p substring ref-man--egal--prev-url)
                  (setq ref-man--eww-buffer-links
                        (nconc ref-man--eww-buffer-links (list ref-man--egal--prev-url))))
              (setq ref-man--eww-buffer-links
                    (nconc ref-man--eww-buffer-links (list ref-man--egal--prev-url)))))
          (setq ref-man--egal--prev-url ref-man--egal--current-url)
          (setq ref-man--egal--current-url nil)
          (setq ref-man--egal--url-text-start (+ 1 (point)))
          (setq ref-man--egal--url-text-end (+ 1 (point)))
          (while (and (not (eobp))
                      (equal (get-text-property (point) 'shr-url) ref-man--egal--prev-url))
            (forward-char 1))               ;; not next link (same link)
          (setq ref-man--egal--url-text-end (point))
          (setq ref-man--egal--current-url (get-text-property (point) 'shr-url)))))
    ref-man--eww-buffer-links))

;; ONLY Called from ref-man-eww-keypress-b
(defun ref-man-eww-get-bibtex-from-scholar (&optional to-org)
  "Extracts the NEXT bibtex entry from a web page rendered with eww
and stores it to my/bibtex-entry"
  (interactive)
  (save-excursion
    (let ((bib-url (progn (search-forward "import into bibtex")
                          (backward-char)
                          (car (eww-links-at-point)))))
          ;; (to-org (if (eq current-prefix-arg 4) nil t)))
      (ref-man--eww-browse-url bib-url to-org))))

;; TODO: Make this async with org-property insertion
;;       (ref-man-eww-download-pdf url) already has it, perhaps due to async callback
(defun ref-man-eww-view-and-download-if-required-pdf (url)
  "View the pdf if it exists in the download directory and download if required.
Calls ref-man-maybe-create-or-insert-org-heading-and-property to
delegate org link handling.
If already at url, url is non nil and it's checked if it's
downloadable, else fetch a pdf url above it"
  (interactive)
  (if (eq major-mode 'eww-mode)
      (let* ((buf (current-buffer))         ; whatever its name is, but an eww buffer
             (url (if (and url (ref-man--downloadable-pdf-url-p url)) ; if it can be downloaded
                      url (car (last (ref-man-eww-get-all-links buf t t "pdf"))))) ; else
             (file (ref-man--check-pdf-file-exists url)))
        ;; (ref-man-maybe-create-or-insert-org-heading-and-property file)
        (if file
            (find-file-other-window file)
          (ref-man-eww-download-pdf url t)))
    (message "[ref-man] Not in eww-mode")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; End eww callable functions ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Begin org utility functions ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun ref-man--org-property-is-url-p (prop)
  (and (listp prop) (string-equal (symbol-name (car prop)) ":uri")))

;; From https://emacs.stackexchange.com/a/16914
;; TODO: Make this async for large buffers
;;       Or perhaps cache the results
;; Not really hidden
(defun ref-man-get-links-of-type-from-org-buffer (buf type &optional restrict)
  (with-current-buffer buf
    (save-restriction
      (when restrict (org-narrow-to-subtree))
      (org-element-map (org-element-parse-buffer) 'link
        (lambda (link)
          (when (string= (org-element-property :type link) type)
            (concat type ":" (org-element-property :path link))))))))

(defun ref-man--insert-link-into-org-buffer (org-buf link link-text metadata)
  "For a given link and metadata generates an org headline in the
given buffer with the metadata"
  (save-excursion
    (with-current-buffer org-buf
      (if ref-man--org-gscholar-launch-buffer
          (progn (goto-char ref-man--org-gscholar-launch-point)
                 (goto-char (line-end-position))
                 (org-insert-heading-respect-content)
                 (org-do-demote))
        (org-mode)
        (org-datetree-find-date-create (org-date-to-gregorian (org-read-date t nil "now")))
        (goto-char (point-at-eol))
        (org-insert-subheading nil))
      (insert (concat link-text "\n"))
      (org-indent-line) (insert (concat metadata "\n"))
      (org-indent-line) (insert (concat "[[" link "][link]]"))
      (message (concat "[ref-man] " "Imported entry " link-text " into buffer " (buffer-name org-buf)))
      (when (and ref-man--org-gscholar-launch-buffer ref-man--org-gscholar-launch-point)
        (goto-char ref-man--org-gscholar-launch-point)))))

(defun ref-man--insert-org-pdf-file-property (file)
  "Given a filename, check if the property PDF-FILE exists for the
corresponding headline and insert."
  (let ((props (org-entry-properties))
        (file-entry (concat "[[" file "]]")))
    (if (and props (cdr (assoc "PDF-FILE" props)))
        (if (y-or-n-p "Entry already exists. Replace?")
            (org-set-property "PDF-FILE" file-entry))
      (org-set-property "PDF-FILE" file-entry))))

(defun ref-man--eww-pdf-download-callback (status url)
  (unless (plist-get status :error)
    (let ((file (ref-man--filename-from-url url)))
      (with-current-buffer ref-man--org-gscholar-launch-buffer
        (save-excursion
          (goto-char ref-man--org-heading-gscholar-launch-point)
          (ref-man--insert-org-pdf-file-property file)))
      (if (file-exists-p file)
          (when (y-or-n-p "File exists. Replace?")
            (goto-char (point-min))
            (re-search-forward "\r?\n\r?\n")
            (write-region (point) (point-max) file)
            (message "[ref-man] Saved %s" file))
        (goto-char (point-min))
        (re-search-forward "\r?\n\r?\n")
        (write-region (point) (point-max) file)
        (message "[ref-man] Saved %s" file)
        ;; (ref-man-maybe-create-or-insert-org-heading-and-property file)
        ))))

(defun ref-man--get-number-of-links-from-text ()
  "From text corresponding to an org heading, get the total
  number of http links and return the topmost one found"
  (save-restriction
    (org-narrow-to-subtree)
    (length (org-element-map (org-element-parse-buffer) 'link
              (lambda (link)
                (when (string-match-p "[http|https]" (org-element-property :type link))
                  (org-element-property :raw-link link)))))))

;; TODO: Big bug here. Fix
;;       I just need it from the current entry, not from the entire subtree
(defun ref-man--get-first-link-from-text ()
  "From text corresponding to an org heading, fetch http links
  and return the topmost one found"
  (save-restriction
    (org-narrow-to-subtree)             ; THIS: is where it has to be fixed 
    (car (org-element-map (org-element-parse-buffer) 'link
           (lambda (link)
             (when (string-match-p "^[http|https]" (org-element-property :type link)) link))))))

(defun ref-man--move-only-link-to-property ()
  (save-excursion
    (save-restriction
      (org-narrow-to-subtree)
      (org-element-map (org-element-parse-buffer) 'link
        (lambda (link)
          (when (string-match-p "^[http|https]" (org-element-property :type link))
            (let ((link-str (org-element-property :raw-link link)))
              (delete-region (org-element-property :begin link)
                             (org-element-property :end link))
              (org-set-property "URL" link-str))))))))

(defun ref-man--move-first-link-to-property ()
  (save-excursion
    (save-restriction
      (org-narrow-to-subtree)
      (org-element-map (org-element-parse-buffer) 'link
        (lambda (link)
          (when (string-match-p "^[http|https]" (org-element-property :type link))
            (let ((link-str (org-element-property :raw-link link)))
              (delete-region (org-element-property :begin link)
                             (org-element-property :end link))
              (org-set-property "URL" link-str))))))))

(defun ref-man--get-first-pdf-link-from-buffer (buf)
  (car (ref-man-eww-get-all-links buf nil nil "pdf")))

;; TODO: Get supllementary material also
(defun ref-man--get-pdf-link-from-nips-url (url)
  (let* ((buf (url-retrieve-synchronously url t))
         (temp-buf (get-buffer-create " *temp-buf*"))
         (link (with-current-buffer temp-buf 
                 (shr-insert-document
                  (with-current-buffer buf
                    (libxml-parse-html-region (point-min) (point-max))))
                 (goto-char (point-min))
                 (ref-man--get-first-pdf-link-from-buffer (current-buffer)))))
    (kill-buffer temp-buf)
    (cond ((string-match-p "^[http|https]" link) link)
          ((string-match-p "^/paper/" link)
           (concat (string-join (firstn (split-string url "/") 3) "/") link))
          (t nil))))

(defun ref-man--get-pdf-link-from-aaai-url (url)
  (let* ((buf (url-retrieve-synchronously url t))
         (temp-buf (get-buffer-create " *temp-buf*"))
         (link (with-current-buffer temp-buf
                 (shr-insert-document
                  (with-current-buffer buf
                    (libxml-parse-html-region (point-min) (point-max))))
                 (goto-char (point-min))
                 (car (last (ref-man-eww-get-all-links (current-buffer)))))))
    (kill-buffer temp-buf)
    (replace-in-string link "view" "download")))

(defun ref-man--get-pdf-link-from-cvf-url (url)
  (let* ((buf (url-retrieve-synchronously url t))
         (temp-buf (get-buffer-create " *temp-buf*"))
         (link (with-current-buffer temp-buf
                 (shr-insert-document
                  (with-current-buffer buf
                    (libxml-parse-html-region (point-min) (point-max))))
                 (goto-char (point-min))
                 (ref-man--get-first-pdf-link-from-buffer (current-buffer)))))
    (kill-buffer temp-buf)
    (cond ((string-match-p "^[http|https]" link) link)
          ((string-match-p "^../../content_.*" link)
           (concat (string-join (firstn (split-string url "/") 3) "/") "/"
                   (string-join (butfirst (split-string link "/") 2) "/")))
          (t nil))))

(defun ref-man--get-pdf-link-from-openreview-url (url)
  (let* ((buf (url-retrieve-synchronously url t))
         (temp-buf (get-buffer-create " *temp-buf*"))
         (link (with-current-buffer temp-buf
                 (shr-insert-document
                  (with-current-buffer buf
                    (libxml-parse-html-region (point-min) (point-max))))
                 (goto-char (point-min))
                 (concat "https://openreview.net" (ref-man--get-first-pdf-link-from-buffer (current-buffer))))))
    (kill-buffer temp-buf)
    (when (string-match-p "^[http|https]" link) link)))

(defun ref-man--get-pdf-url-according-to-source (url)
  (when url
    (cond ((string-match-p "doi.org" url)
           (ref-man--get-pdf-link-from-doi url))
          ((string-match-p "arxiv.org" url)
           (concat (replace-in-string url "/abs/" "/pdf/") ".pdf"))
          ((string-match-p "aclweb.org" url)
           (concat url ".pdf"))
          ((string-match-p "papers.nips.cc" url)
           (ref-man--get-pdf-link-from-nips-url url))
          ((string-match-p "openaccess.thecvf.com" url)
           (ref-man--get-pdf-link-from-cvf-url url))
          ((string-match-p "aaai.org" url)
           (ref-man--get-pdf-link-from-aaai-url url))
          ((string-match-p "openreview.net" url)
           (ref-man--get-pdf-link-from-openreview-url url))
          (t url))))

(defun ref-man--try-get-bib-url-according-to-source (url)
  (when url
    (cond ((string-match-p "doi.org" url)
           (ref-man--get-bibtex-link-from-doi url))
          ((string-match-p "arxiv.org" url)
           (concat (replace-in-string url "/abs/" "/pdf/") ".pdf"))
          ((string-match-p "aclweb.org" url)
           (concat url ".pdf"))
          ((string-match-p "papers.nips.cc" url)
           (ref-man--get-bibtex-link-from-nips-url url))
          ((string-match-p "openaccess.thecvf.com" url)
           (ref-man--get-bibtex-link-from-cvf-url url))
          ((string-match-p "aaai.org" url)
           (ref-man--get-bibtex-link-from-aaai-url url))
          ((string-match-p "openreview.net" url)
           (ref-man--get-bibtex-link-from-openreview-url url))
          (t url))))

(defun ref-man--try-get-abstract-according-to-source (url)
  (when url
    (cond ((string-match-p "doi.org" url)
           (ref-man--get-abstract-from-doi url))
          ((string-match-p "arxiv.org" url)
           (concat (replace-in-string url "/abs/" "/pdf/") ".pdf"))
          ((string-match-p "aclweb.org" url)
           (concat url ".pdf"))
          ((string-match-p "papers.nips.cc" url)
           (ref-man--get-abstract-from-nips-url url))
          ((string-match-p "openaccess.thecvf.com" url)
           (ref-man--get-abstract-from-cvf-url url))
          ((string-match-p "aaai.org" url)
           (ref-man--get-abstract-from-aaai-url url))
          ((string-match-p "openreview.net" url)
           (ref-man--get-abstract-from-openreview-url url))
          (t url))))

(defun ref-man--get-pdf-link-from-doi (url)
  "Buffer redirects correctly to IEEE (or some other site), but I
  can't really download from there"
  ;; If link is cvpr or iccv, then find the cvf link and go to that site
  (message "Not Implemented yet") nil)

(defun ref-man--get-bibtex-link-from-doi (url)
  "Buffer redirects correctly to IEEE (or some other site), but I
  can't really download from there"
  ;; If link is cvpr or iccv, then find the cvf link and go to that site
  (message "Not Implemented yet") nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; End org utility functions ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;
;; Begin org functions ;;
;;;;;;;;;;;;;;;;;;;;;;;;;

;; TODO: Replace all property setters with
;;       ref-man--insert-org-pdf-file-property

(defun ref-man-import-arxiv-link-to-org-buffer (url)
  (message "Not Implemented"))

;; TODO: Currently no TODO attribute is set on the org entry and no
;;       timestamp is marked. I should fix that.
;; TODO: Allow import to specified location in a specified org file also
;;       e.g. research/reading/misc.
(defun ref-man-import-gscholar-link-to-org-buffer (url)
"Before call should check the buffer as it can't be called if
buffer is not gscholar"
  (interactive)
  (save-excursion
    (if (and (or (eq major-mode 'shr-mode) (eq major-mode 'eww-mode))
             (string-match-p "scholar\\.google\\.com" (plist-get eww-data :url)))
      (let* ((eww-buf (current-buffer))
             (org-buf (if ref-man--org-gscholar-launch-buffer
                          ref-man--org-gscholar-launch-buffer
                        (let ((org-links-file-name (file-name-nondirectory ref-man-org-links-file-path)))
                          (if (get-buffer org-links-file-name) (get-buffer org-links-file-name)
                            (find-file-noselect ref-man-org-links-file-path)))))
             (link (if url url (ref-man--eww-get-gscholar-link-for-import eww-buf)))
             (args (ref-man--eww-get-import-link-data eww-buf link)))
        (if args                        ; link link-text-begin link-text-end metadata
            (apply #'ref-man--insert-link-into-org-buffer (cons org-buf args))
          (message "[ref-man] Could not get link to import")))
      (message "[ref-man] Not gscholar buffer"))))

(defun ref-man-try-fetch-pdf-from-url (url)
  ;; Here I'll have to write rules to fetch pdfs from different urls,
  ;; e.g. arxiv.org, nips, cvpr, iccv, aaai, tacl, aclweb, pmlr (jmlr, icml)
  ;; Should it just return a downloadable link or download the file itself?
  (setq ref-man--org-gscholar-launch-buffer (current-buffer)) ; needn't be at heading
  (setq ref-man--org-heading-gscholar-launch-point (point))
  (let ((url (ref-man--get-pdf-url-according-to-source url)))
    (when url
      (if (ref-man--downloadable-pdf-url-p url)
          (if (ref-man--check-pdf-file-exists url)
              (ref-man--insert-org-pdf-file-property (ref-man--check-pdf-file-exists url))
            (message (concat "Fetching " url))
            (url-retrieve url #'ref-man--eww-pdf-download-callback (list url)))
        (message (concat "Browsing " url))
        (eww-browse-url url)))))

(defun ref-man-try-fetch-and-store-pdf-in-org-subtree-entries ()
  (interactive)
  (message "Not Implemented right now"))

;; TODO: I have not incorporated dblp extracted entries as they refer
;;       to DOIs or direct arxiv links. Have to fix that.
;; TODO: If the pdf is extracted from a site like cvf, or nips
;;       or whatever, the corresponding bibtex should also be
;;       extracted and stored from the website itself.
(defun ref-man-try-fetch-and-store-pdf-in-org-entry ()
  (interactive)
  (let* ((props (org-entry-properties))
         (pdf-file (cond ((assoc "PDF-FILE" props)
                          (cdr (assoc "PDF-FILE" props)))
                         ((assoc "FILE_NAME" props)
                          (cdr (assoc "FILE_NAME" props)))
                         (t nil))))
    (cond ((and pdf-file (assoc "FILE_NAME" props))
           ;; Don't need ref-man--org-gscholar-launch-buffer or point here
           (ref-man--insert-org-pdf-file-property pdf-file)
           (org-delete-property "FILE_NAME"))
          ((and pdf-file (not (assoc "URL" props)))
           (org-element-property :raw-link (ref-man--get-first-link-from-text)))
          ((and (not pdf-file) (assoc "URL" props))
           (ref-man-try-fetch-pdf-from-url (cdr (assoc "URL" props))))
          ((not pdf-file) (ref-man-try-fetch-and-store-pdf-in-org-entry-text))
          (t (message "Reached default state. Please check!")))))

;; FIXED: Since the retrieval is now async, if I move to another
;;        headline while the file is being downloaded, the file link
;;        may get inserted in the other headline.
;;        Will need to store some list for the corresponding points
;;        with push and pop function so that the correct property is
;;        inserted to each headline.
;; TODO: Even though the above issue is fixed, I'm now confronted with
;;       the issue that if I call the function on two org entries, the
;;       global variable will be over written and there's no way to
;;       avoid that except by using assoc lists and mutexes.
(defun ref-man-try-fetch-and-store-pdf-in-org-entry-text ()
  "For the first link in text, ascertain if the corresponding pdf
file exists or not. In case it doesn't, retrieve the pdf file
from the url if it is a downloadable url or an arxiv url. If the
link is not a pdf link, call eww-browse-url. If the file exists,
don't retrieve the pdf file.
In both cases, store the file link in PDF-FILE property in the org
property drawer for the headline at point."
  (interactive)
  (let* ((props (text-properties-at (point)))
         (url (car (mapcar (lambda (x) (cadr x))
                           (remove-if-not #'ref-man--org-property-is-url-p props))))
         (url (if url url (org-element-property :raw-link
                                                (ref-man--get-first-link-from-text)))))
    (when url
      (cond ((= 1 (ref-man--get-number-of-links-from-text))
             (ref-man--move-only-link-to-property))
            ((< 1 (ref-man--get-number-of-links-from-text))
             (y-or-n-p "Move first link to property?")
             (let* ((link (ref-man--get-first-link-from-text))
                    (link-str (org-element-property :raw-link link)))
               (delete-region (org-element-property :begin link)
                              (org-element-property :end link))
               (org-set-property "URL" link-str))))
      (ref-man-try-fetch-pdf-from-url url))))

;; TODO: This should always be async with callback
;;       One issue then will be, how to correspond between threads
;;       
;;       That is, A single global variable will be overwritten and
;;       say, I call it twice, and two url names are pushed on to
;;       the stack, after the download is finished for the either,
;;       that one is viewed, but will the ensuing download make it
;;       switch to the new file?
;;       - For the download problem, the url can be mapped to the
;;         unique filename so it's ok
;;       - But which one to view is a problem
;;       - If I'm already viewing some pdf, the other one can just
;;         message, else it opens it up in the adjacent window.
(defun ref-man-eww-download-pdf (url &optional view)
  "Download the pdf file from google scholar and optionally view it"
  (interactive)
  (if (not view)
      (url-retrieve url #'ref-man--eww-pdf-download-callback (list url))
    (let* ((buf (url-retrieve-synchronously url t))
           (buf-string (with-current-buffer buf (buffer-string))))
      (if buf-string
          (let ((file (ref-man--filename-from-url url)))
            ;; (file (concat ref-man-pubs-directory (file-name-nondirectory path))))
            ;; (eww-make-unique-file-name
            ;;  (eww-decode-url-file-name (file-name-nondirectory path))
            ;;  ref-man-pubs-directory)
            ;; (setq my/eww-imported-link nil)
            (if (file-exists-p file)      ; This really will never execute
                (if (y-or-n-p "File exists. Replace?")
                    (with-current-buffer buf
                      (goto-char (point-min))
                      (re-search-forward "\r?\n\r?\n")
                      (write-region (point) (point-max) file)))
              (with-current-buffer buf
                (goto-char (point-min))
                (re-search-forward "\r?\n\r?\n")
                (write-region (point) (point-max) file)))
            (find-file-other-window file)))
      (kill-buffer buf))))

;; called from an org buffer
;; when to set ref-man--org-gscholar-launch-buffer nil?
;; 
;; when gscholar is called from any other buffer than previous
;; ref-man--org-gscholar-launch-point
(defun ref-man-org-search-heading-on-gscholar-with-eww ()
  "Searches for the current heading in google scholar in
eww. Stores the buffer and the position from where it was called."
  (interactive)
  (if (eq major-mode 'org-mode)
      (progn
        (setq ref-man--org-heading-gscholar-launch-point (point)) ; must be at heading?
        (setq org-buf (current-buffer))
        (save-excursion
          (let ((query-string (org-get-heading t t)))
            (ref-man-eww-gscholar query-string))))
    (message "[ref-man] Not in org-mode")))

;; TODO: Fix this
;; This function may take up a lot of processing. Should implement some cache for it
;; Can be called after downloading pdf from any website
(defun ref-man-maybe-create-or-insert-org-heading-and-property (file)
  "For a given filename, a heading and/or :PDF-FILE: property is
inserted in ref-man--org-gscholar-launch-buffer if it exists, or
temp-org-buffer.
a) org-gscholar-launch-buffer if it exists
b) temp-org-buffer otherwise
If the pdf already exists in the download directory
a) org-gscholar-launch-buffer is checked for the entry"
  ;; - If org-gscholar-launch-buffer exists
  ;;   When does it exist though? What if I go to eww from org with eww?
  ;;   - Save excursion, get pdf file name and search in the buffer
  ;;   - If it matches in any link [[][]] in heading then insert file link for the org heading
  ;;     - Else if *eww* buffer was gscholar or arxiv
  ;;       generate org-heading-after-checking if it exists
  ;;     - Else generate org-heading-from-title
  ;; - Else Repeat with temp-org-buffer instead
  (let* ((filename (file-name-nondirectory file))
         (buf (if ref-man--org-gscholar-launch-buffer ref-man--org-gscholar-launch-buffer
                (get-buffer-create ref-man-org-links-file-path)))
         (existing-files (map 'file-name-nondirectory
                              (ref-man--get-links-of-type-from-org-buffer buf "file")))
         (file-match (if (and existing-files (car (seq-drop-while
                                                   (lambda (x) (not (string-match-p filename x)))
                                                   existing-files))))))
    ;; Where is the corresponding headline?
    ;; Check if eww-import-link exists instead
    (cond ((buf file-match)
           (with-current-buffer buf
             (if ref-man--eww-import-link
                 org-set-property "URL" ref-man--eww-import-link)
             ;; (ref-man--insert-org-pdf-file-property file)
             (with-current-buffer ref-man--org-gscholar-launch-buffer
               (save-excursion
                 (goto-char ref-man--org-heading-gscholar-launch-point)
                 (ref-man--insert-org-pdf-file-property file))))))))

(defun ref-man--at-bib-heading-p ()
  (let ((props (org-entry-properties)))
    (and (org-at-heading-p) (assoc "BTYPE" props) (assoc "CUSTOM_ID" props) t)))

(defun ref-man-parse-subtree-to-buffer-as-bibtex (&optional bib-buf)
  "Inserts contents of an org-subtree as bibtex entries to a bib file"
  (interactive (list (if (not (boundp 'bib-buf))
                         (completing-read "Bib buffer: " (mapcar (lambda (x) (format "%s" x)) (buffer-list)))
                       nil)))
  (if (org-at-heading-p)
      (when heading-has-children
          
    ;; Import heading to bib buffer
    ;; Descend into subtree
    ;; On all children do recursive call to self
        )
    (message "Not at heading"))                             ; HERE
  (when (ref-man--at-bib-heading-p)
    (ref-man-parse-subtree-to-buffer-as-bibtex bib-buf))
  (if (not bib-buf)
      (progn (message "bib-buf is required when called non-interactively") nil)
    (with-current-buffer (get-buffer bib-buf)
      (goto-char (point-min))
      ;; Insert stuff
      )))

;;;;;;;;;;;;;;;;;;;;;;;
;; End org functions ;;
;;;;;;;;;;;;;;;;;;;;;;;

(provide 'ref-man)
