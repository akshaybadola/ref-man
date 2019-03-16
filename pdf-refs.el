;; Should I put a copyright assertion here? Isn't the whole purpose of
;; GPL to not have a copyright? It's confusing.
;;
;; Author: Akshay Badola <akshay.badola.cs@gmail.com>
;; Keywords: extensions

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the gnu general public license as published by
;; the free software foundation, either version 3 of the license, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but without any warranty; without even the implied warranty of
;; merchantability or fitness for a particular purpose.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

(require 'cl)
(require 'url)
(require 'xml)
(require 'org)
(require 'async)
(require 'bibtex)
(require 'org-bibtex)
(require 'biblio-core)
(require 'gscholar-bibtex)

;; TODO should be defcustom
(setq my/org-store-dir "/home/joe/org/pubs_org/")
(if (string-match-p "droid" (system-name))
    (setq my/bib-store-dir "/home/joe/pubs/bibtex/")
(setq my/bib-store-dir "/home/joe/PhD/bibtex/"))

;;;;;;;;;;;;;;;;;;;;;;;
;; utility functions ;;
;;;;;;;;;;;;;;;;;;;;;;;

(defun my/venue-pref (results)
  (if (= 1 (length results))
      0
    (let* ((venues (mapcar (lambda (x) (gscholar-bibtex--xml-get-child x 'venue)) results))
           (prefs (mapcar (lambda (x) (cdr (assoc (car (last x)) my/venue-priorities))) venues)))
      prefs)))

(defun my/fix (str)
  "gets text between parentheses {}"
  (string-remove-suffix "}" (string-remove-prefix "{" str)))

(defun my/is-bibtex-key (item)
  (string= (car item) "=key="))

;;
;; Constants. perhaps can name them better
;; Also should be shifted to defcustom
;;
(setq my/venue-priorities (let* ((confs '("icml" "nips" "iccv" "cvpr" "eccv"))
       (confs-seq (number-sequence (length confs) 1 -1)))
       (mapcar* 'cons confs confs-seq)))
(setq my/key-list '(authors title venue volume number pages year doi ee))
(setq my/stop-words '("i" "it" "its" "what" "which" "who" "that" "these" "is" "are" "was" "were" "have" "has" "had" "having" "do" "does" "did" "a" "an" "the" "and" "because" "as" "of" "at" "by" "for" "with" "about" "against" "between" "into" "through" "during" "before" "after" "above" "below" "to" "from" "up" "down" "in" "out" "on" "off" "over" "under" "again" "further" "then" "once" "here" "there" "when" "where" "why" "how" "all" "any" "both" "each" "few" "more" "most" "other" "some" "such" "no" "nor" "not" "only" "own" "same" "so" "than" "too" "very" "s" "t" "can" "will" "just" "don" "should" "now"))
(defun my/is-stop-word (x)
  (member x my/stop-words))
;;
;; clean the xml entry and keep relevant itmes. uses gscholar-bibtex
;;
(defun my/dblp-clean (result)
  "cleans the xml entry and keeps relevant itmes according to
my/key-list. uses gscholar-bibtex. returns an assoc list of (key. value)
pairs for only the top result from my/venue-priorities."
  (let ((result (nth (my/max-ind (my/venue-pref result)) result)))
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
                 my/key-list)))))

;;
;; TODO: crossref inserts URL as dx.doi.org something which redirects
;; to the real url or may not even in some cases. If a pdf url exists,
;; don't mess with it and insert it as doi.
;;
;; TODO: build a different function so that \{etc} aren't there in bib keys
;; my/remove-non-ascii does something else entirely and I don't want accents
;; in the keys
(defun my/build-bib-key (key-str &optional na)
  "builds a unique key with the format [author year
  first-title-word] entry from the list of (key . value)"
  (let* ((first-author-str (car (split-string (my/trim (car (cdr (assoc "authors" key-str)))) "," t)))
         (first-author (my/validate-author (split-string first-author-str " " t)))
         (last-name (car (last first-author)))
         (year-pub (my/trim (car (cdr (assoc "year" key-str)))))
         (title (remove-if 'my/is-stop-word (split-string (downcase (my/trim (car (cdr (assoc "title" key-str))))) " ")))
         (title-first (car (split-string (first title) "-")))
         (key (my/remove-non-ascii (mapconcat 'downcase (list last-name year-pub title-first) "")))
         )
    (if na (concat "na_" key) key)))

(defun my/build-bib-key-from-parsed-org-bibtex (bib-assoc)
  "builds a unique key with the format [author year
  first-title-word] entry from the list of (key . value). Trims
  the entries and converts multiple spaces to a single one."
  (let* ((first-author-str (car (split-string (my/trim (cdr (assoc :author bib-assoc))) "," t)))
         (first-author (my/validate-author (split-string first-author-str " " t)))
         (last-name (car (last first-author)))
         (year-pub (my/trim (cdr (assoc :year bib-assoc))))
         (title (remove-if 'my/is-stop-word (split-string (downcase (my/trim (cdr (assoc :title bib-assoc)))) " ")))
         (title-first (car (split-string (first title) "-")))
         (key (my/remove-non-ascii (mapconcat 'downcase (list last-name year-pub title-first) "")))
         )
    key))

(defun my/build-bib-assoc-from-parsed-org-bibtex (bib-assoc)
  "builds the association list. can be used to build both the bib
entry and org entry"
  (let* ((key (my/build-bib-key-from-parsed-org-bibtex bib-assoc))
         (author (cons "author" (my/trim (cdr (assoc :author bib-assoc)))))
         (title (cons "title" (my/trim (cdr (assoc :title bib-assoc)))))
         (year (cons "year" (my/trim (cdr (assoc :year bib-assoc)))))
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
         (howpublished (if (and howpublished (> 1 (length (split-string howpublished "{"))))
             (if (string-match-p "url" (nth 0 (split-string howpublished "{")))
                 (car (split-string (nth 1 (split-string howpublished "{")) "}")) nil)
             nil))
         )
    (list key (remove-if-not 'cdr (list abstract author title year doi volume number pages url venue publisher howpublished)))))

(defun my/build-bib-key-from-parsed-bibtex (bib-assoc)
  "builds a unique key with the format [author year
  first-title-word] entry from the list of (key . value). Assumes
  the strings are all validated"
  (let* ((last-name (car (split-string
                          (car (split-string (my/fix (cdr (assoc "author" bib-assoc))) " and ")) ", ")))
         (year-pub (cdr (assoc "year" bib-assoc)))
         (title (remove-if 'my/is-stop-word (split-string (my/fix (downcase (cdr (assoc "title" bib-assoc)))) " ")))
         (title-first (car (split-string (first title) "-")))
         )
    (my/remove-non-ascii (mapconcat 'downcase (list last-name year-pub title-first) ""))))

(defun my/validate-author (author)
  (if (or (string-match-p "[0-9]+" (car (last author)))
                  (string-match-p "^i$\\|^ii$\\|^iii$\\|^iv$" (downcase (car (last author)))))
      (if (> (length author) 2) (butlast author) (nconc (butlast author) '("")))
    author))

(defun my/build-bib-author (author-str)
  "builds the \"author\" value according to bibtex format"
  (let* ((author-str (my/remove-non-ascii author-str))
         (author-str (replace-in-string (replace-in-string author-str "\\.$" "") ",$" ""))
         (authors (split-string author-str "," t))
         (result-authors
          (mapcar (lambda (x)
                    (let ((temp-auth (my/validate-author (split-string x " " t))))
                      (if (= 1 (length temp-auth)) (car temp-auth)
                        (mapconcat 'identity (list (car (last temp-auth))
                                                   (mapconcat 'identity
                                                              (butlast temp-auth) " ")) ", ")))) authors)))
    (mapconcat 'identity result-authors " and ")))

(defun my/build-vernacular-author (author-str)
  "Builds the \"author\" string as common spoken English. Assumes
  that the input is in bib_author format"
  (let* ((author-str (replace-in-string (replace-in-string author-str "\\.$" "") ",$" ""))
         (authors (split-string author-str " and " t "[ ]+"))
         (result-authors
          (mapcar (lambda (x) (mapconcat 'identity (reverse (split-string x ", ")) " "))
                  authors))
         (result-authors (mapconcat 'identity result-authors " and ")))
    result-authors))

(defun my/build-bib-assoc (key-str &optional na)
  "builds the association list. can be used to build both the bib
entry and org entry"
  (let* ((key (my/build-bib-key key-str na))
         (author (cons "author" (my/build-bib-author
                                 (car (cdr (assoc "authors" key-str))))))
         (title (cons "title" (car (cdr (assoc "title" key-str)))))
         (year (cons "year" (car (cdr (assoc "year" key-str)))))
         (doi (cons "doi" (car (cdr (assoc "doi" key-str)))))
         (volume (cons "volume" (car (cdr (assoc "volume" key-str)))))
         (number (cons "number" (car (cdr (assoc "number" key-str)))))
         (tmp-pages (car (cdr (assoc "pages" key-str))))
         (pages (cons "pages" (if tmp-pages
                                  (replace-in-string
                                   (replace-in-string tmp-pages "-" "--") " " "")
                                nil)))
         (url (cons "url" (car (cdr (assoc "ee" key-str)))))
         (venue (cons "venue" (car (cdr (assoc "venue" key-str))))) ;; TODO: expand venue
         )
    (list key (remove-if-not 'cdr (list author title year doi volume number pages url venue)))))

;; TODO: Add async callback with message "DONE" when done
(defun my/get-references ()
  "this is the only entry point to fetch and write the references
to a buffer right now. can change to have it in multiple steps."
  (interactive)
  (setq my/dblp-results ())
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
       (refs-list (if json-string (mapcar (lambda (x)
                                            (cons (concat (gethash "title" x) " "
                                                          (string-join (gethash "authors" x) " ")) x))
                                          (gethash "references" json-string)) nil)))
    (if json-string
        (progn
          (setq my/pdf-file-name pdf-file-name)
          (setq my/science-parse-data json-string)
          (setq my/abstract (gethash "abstracttext" json-string))
          (setq my/title (gethash "title" json-string))
          (setq my/refs-list refs-list)
          (setq my/bibs "")
          (my/generate-buffer-and-fetch refs-list))
      (progn (message "[pdf-refs] empty pdf parse") nil))
    )
  nil)

(defun my/generate-org-buffer (&optional visiting-filename)
  "Generated buffer where all the fetch results will be inserted"
  (let ((buf (get-buffer-create
              (if visiting-filename visiting-filename (concat my/title "_org"))))
        (win (my/get-or-create-window-on-side)))
    (set-window-buffer win buf)
    (with-current-buffer buf (org-mode))
    buf))

(defun my/get-or-create-window-on-side ()
  (let* ((orig-win (selected-window))
         (win (cond ((window-in-direction 'right orig-win)
                     (window-in-direction 'right orig-win))
                    ((window-in-direction 'left orig-win)
                     (window-in-direction 'left orig-win))
                    (t (split-window-horizontally)))))
    win))

;;
;; Maybe python-epc would be better.
;;
(defun my/dblp-fetch-parallel (refs-list org-buf)
  "Fetches all dblp queries in parallel via async"
  (loop for ref-refs in refs-list do
        (async-start
         `(lambda ()
            ;; (defun replace-in-string (what with in)
            ;;   (replace-regexp-in-string (regexp-quote what) with in nil 'literal))
            ,(async-inject-variables "ref-refs")
            (let* ((mah-url (format "http://dblp.uni-trier.de/search/publ/api?q=%s&format=xml" (car ref-refs)))
                   (buf (url-retrieve-synchronously mah-url)))
              (prog2 (with-current-buffer buf (set-buffer-multibyte t))
                  (with-current-buffer buf (buffer-string))
                (kill-buffer buf))))
         `(lambda (buf-string)
            ,(async-inject-variables "ref-refs\\|org-buf")
            (let ((guf (generate-new-buffer "*dblp-test*")))
              (with-current-buffer guf (insert buf-string))
              (with-current-buffer guf (set-buffer-multibyte t))
              (pcase-let ((`(,(and result `(result . ,_)))
                           (xml-parse-region nil nil guf)))
                (let ((key-str (remove nil
                                       (my/dblp-clean
                                        (mapcar (lambda (hit)
                                                  (gscholar-bibtex--xml-get-child hit 'info))
                                                (xml-get-children (gscholar-bibtex--xml-get-child result 'hits) 'hit))
                                        ))))
               (with-current-buffer org-buf
                 (if key-str (my/org-bibtex-write-ref-from-assoc (my/build-bib-assoc key-str))
                   (my/org-bibtex-write-ref-NA-from-keyhash (cdr ref-refs))))
               (kill-buffer guf)
             )))))))

;;
;; Called by my/generate-buffer-and-fetch
;;
(defun my/dblp-fetch-serial (query)
  "Fetch the dblp data synchronously. Would be used to insert the
top level heading"
  (let* ((mah-url (format "http://dblp.uni-trier.de/search/publ/api?q=%s&format=xml" query))
         (buf (url-retrieve-synchronously mah-url)))
    (with-current-buffer buf (set-buffer-multibyte t))
    (pcase-let ((`(,(and result `(result . ,_)))
                 (xml-parse-region nil nil buf)))
      (remove nil (my/dblp-clean
                   (mapcar (lambda (hit)
                             (gscholar-bibtex--xml-get-child hit 'info))
                           (xml-get-children (gscholar-bibtex--xml-get-child result 'hits) 'hit)))))
    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Org generation and insertion stuff ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (defun my/generate-key-hash-from-science-parse ()
;;   (let ((key-hash (make-hash-table :test 'equal)))
;;     (if (gethash "authors" my/science-parse-data)
;;         (puthash "authors"
;;                  (mapcar (lambda (x) (gethash "name" x)) (gethash "authors" my/science-parse-data)) key-hash))
;;     (if (gethash "year" my/science-parse-data)
;;         (puthash "year" (gethash "year" my/science-parse-data) key-hash))
;;     (if (gethash "title" my/science-parse-data)
;;         (puthash "title" (gethash "title" my/science-parse-data) key-hash))
;;     (if (gethash "venue" my/science-parse-data)
;;         (puthash "venue" (gethash "venue" my/science-parse-data) key-hash))
;;     (if (gethash "pages" my/science-parse-data)
;;         (puthash "pages" (gethash "pages" my/science-parse-data) key-hash))
;;     (if (gethash "volume" my/science-parse-data)
;;         (puthash "volume" (gethash "volume" my/science-parse-data) key-hash))
;;     key-hash))

(defun my/generate-key-str-from-science-parse ()
  (let ((key-str ()))
         (if (gethash "authors" my/science-parse-data)
                     (add-to-list 'key-str (cons "authors"  (list (mapconcat (lambda (x) (gethash "name" x))
                                                    (gethash "authors" my/science-parse-data) ", ")))))
         ;; (if (gethash "authors" my/science-parse-data)
         ;;             (add-to-list 'key-str (cons "authors"  (list (mapcar (lambda (x) (gethash "name" x))
         ;;                                            (gethash "authors" my/science-parse-data))))))
         (if (gethash "year" my/science-parse-data)
                      (add-to-list 'key-str (cons "year" (list (format "%s" (gethash "year" my/science-parse-data))))))
         (if (gethash "title" my/science-parse-data)
             (add-to-list 'key-str (cons "title"  (list (gethash "title" my/science-parse-data)))))
         (if (gethash "venue" my/science-parse-data)
                      (add-to-list 'key-str (cons "venue" (list (gethash "venue" my/science-parse-data)))))
        key-str))

;; Fixed: "What if not key-str"
(defun my/generate-buffer-and-fetch (refs-list)
  (let* ((key-str (my/dblp-fetch-serial  ;; assoc list
                   (concat
                    (replace-regexp-in-string "[^\t\n\r\f -~]" ""  (gethash "title" my/science-parse-data)) " "
                    (string-join (mapcar (lambda (x) (gethash "name" x))
                                         (gethash "authors" my/science-parse-data)) " "))))
         (na (not key-str))
         (key-str (if (not key-str) (my/generate-key-str-from-science-parse) key-str))
         (bib-assoc (my/build-bib-assoc key-str na))
         (filename  (car bib-assoc))
         (visiting-filename
          (path-join (string-remove-prefix "na_" filename) ".org"))
         )
    ;; What if not filename? I guess that's a parse error
    (if (file-exists-p visiting-filename)
        (progn (message "[pdf-refs] File already exists. Opening")
               (with-current-buffer (get-buffer-create (concat filename ".org"))
                 (insert-file-contents visiting-filename t))
               (my/generate-org-buffer (concat filename ".org")))
      (if filename
          (let ((org-buf (my/generate-org-buffer visiting-filename)))
            (with-current-buffer org-buf
              (my/org-bibtex-write-top-heading-from-assoc bib-assoc)
              (org-insert-heading-after-current)
              (org-shiftmetaright)
              (my/dblp-fetch-parallel refs-list org-buf)
              (set-visited-file-name visiting-filename))
            )
        (message "[pdf-refs] PDF Parse Error"))
      )
    ))

(defun my/org-bibtex-write-top-heading-from-assoc (entry)
  "Generate the top level org entry for data parsed with science-parse."
  (let* ((key (car entry))
         (entry (nth 1 entry))
         (pdf-file-name (if (string-match-p (concat "^" my/pdf-pubs-directory) my/pdf-file-name)
                            my/pdf-file-name nil)))
    (org-insert-heading)
    (insert (cdr (assoc "title" entry)))
    (insert "\n")
    (org-indent-line)
    (insert (gethash "abstractText" my/science-parse-data)) ;; Hack to get abstractText
    (fill-paragraph)
    (org-insert-property-drawer)
    (loop for ent in entry
          do
          (if (not (string-equal (car ent) "abstract"))
              (org-set-property (upcase (car ent)) (my/fix (cdr ent)))
            ))
    (org-set-property "CUSTOM_ID" key)
    (org-set-property "BTYPE" "article")
  ))

;; The characters directly borrowed from org-ref.
;; The function is too long and is an ugly hack because I couldnt'
;; find an effective way to update a local variable in a cl-loop.
;; And setting a global variable was causing trouble with parallel
;; implementation.
(defun my/remove-non-ascii (author-str)
  (let* ((author-str (replace-in-string author-str "í" "{\\\\'i}"))
         (author-str (replace-in-string author-str "æ" "{\\\\ae}"))
         (author-str (replace-in-string author-str "ć" "{\\\\'c}"))
         (author-str (replace-in-string author-str "é" "{\\\\'e}"))
         (author-str (replace-in-string author-str "ä" "{\\\\\"a}"))
         (author-str (replace-in-string author-str "è" "{\\\\`e}"))
         (author-str (replace-in-string author-str "à" "{\\\\`a}"))
         (author-str (replace-in-string author-str "á" "{\\\\'a}"))
         (author-str (replace-in-string author-str "ø" "{\\\\o}"))
         (author-str (replace-in-string author-str "ë" "{\\\\\"e}"))
         (author-str (replace-in-string author-str "ü" "{\\\\\"u}"))
         (author-str (replace-in-string author-str "ñ" "{\\\\~n}"))
         (author-str (replace-in-string author-str "ņ" "{\\\\c{n}}"))
         (author-str (replace-in-string author-str "ñ" "{\\\\~n}"))
         (author-str (replace-in-string author-str "å" "{\\\\aa}"))
         (author-str (replace-in-string author-str "ö" "{\\\\\"o}"))
         (author-str (replace-in-string author-str "á" "{\\\\'a}"))
         (author-str (replace-in-string author-str "í" "{\\\\'i}"))
         (author-str (replace-in-string author-str "ó" "{\\\\'o}"))
         (author-str (replace-in-string author-str "ó" "{\\\\'o}"))
         (author-str (replace-in-string author-str "ú" "{\\\\'u}"))
         (author-str (replace-in-string author-str "ú" "{\\\\'u}"))
         (author-str (replace-in-string author-str "ý" "{\\\\'y}"))
         (author-str (replace-in-string author-str "š" "{\\\\v{s}}"))
         (author-str (replace-in-string author-str "č" "{\\\\v{c}}"))
         (author-str (replace-in-string author-str "ř" "{\\\\v{r}}"))
         (author-str (replace-in-string author-str "š" "{\\\\v{s}}"))
         (author-str (replace-in-string author-str "İ" "{\\\\.i}"))
         (author-str (replace-in-string author-str "ğ" "{\\\\u{g}}"))
         (author-str (replace-in-string author-str "δ" "$\\\\delta$"))
         (author-str (replace-in-string author-str "ç" "{\\\\c{c}}"))
         (author-str (replace-in-string author-str "ß" "{\\\\ss}"))
         (author-str (replace-in-string author-str "≤" "$\\\\le$"))
         (author-str (replace-in-string author-str "≥" "$\\\\ge$"))
         (author-str (replace-in-string author-str "<" "$<$"))
         (author-str (replace-in-string author-str "θ" "$\\\\theta$"))
         (author-str (replace-in-string author-str "μ" "$\\\\mu$"))
         (author-str (replace-in-string author-str "→" "$\\\\rightarrow$"))
         (author-str (replace-in-string author-str "⇌" "$\\\\leftrightharpoons$"))
         (author-str (replace-in-string author-str "×" "$\\\\times$"))
         (author-str (replace-in-string author-str "°" "$\\\\deg$"))
         (author-str (replace-in-string author-str "ş" "{\\\\c{s}}"))
         (author-str (replace-in-string author-str "γ" "$\\\\gamma$"))
         (author-str (replace-in-string author-str "ɣ" "$\\\\gamma$"))
         (author-str (replace-in-string author-str "º" "degc"))
         (author-str (replace-in-string author-str "η" "$\\\\eta$"))
         (author-str (replace-in-string author-str "µ" "$\\\\mu$"))
         (author-str (replace-in-string author-str "α" "$\\\\alpha$"))
         (author-str (replace-in-string author-str "β" "$\\\\beta$"))
         (author-str (replace-in-string author-str "ɛ" "$\\\\epsilon$"))
         (author-str (replace-in-string author-str "ⅵ" "\\textrm{vi}"))
         (author-str (replace-in-string author-str "ⅲ" "\\textrm{iii}"))
         (author-str (replace-in-string author-str "ⅴ" "\\textrm{v}"))
         (author-str (replace-in-string author-str "λ" "$\\\\lambda$"))
         (author-str (replace-in-string author-str "π" "$\\\\pi$"))
         (author-str (replace-in-string author-str "∞" "$\\\\infty$"))
         (author-str (replace-in-string author-str "χ" "$\\\\chi$"))
         (author-str (replace-in-string author-str "∼" "\\\\textasciitilde{}"))
         (author-str (replace-in-string author-str "‑" "\\\\textemdash{}"))
         (author-str (replace-in-string author-str " " " "))
         (author-str (replace-in-string author-str "…" "..."))
         (author-str (replace-in-string author-str "•" "\\\\textbullet "))
         (author-str (replace-in-string author-str " " " "))
         (author-str (replace-in-string author-str " " " "))
         (author-str (replace-in-string author-str " " " "))
         (author-str (replace-in-string author-str "–" "-"))
         (author-str (replace-in-string author-str "−" "-"))
         (author-str (replace-in-string author-str "–" "-"))
         (author-str (replace-in-string author-str "—" "-"))
         (author-str (replace-in-string author-str "‒" "\\\\textemdash{}"))
         (author-str (replace-in-string author-str "‘" "'"))
         (author-str (replace-in-string author-str "’" "'"))
         (author-str (replace-in-string author-str "’" "'"))
         (author-str (replace-in-string author-str "“" "\""))
         (author-str (replace-in-string author-str "’" "'"))
         (author-str (replace-in-string author-str "”" "\"")))
    author-str))

(defun my/generate-NA-entry (key-hash)
  (if (and (gethash "title" key-hash) (gethash "authors" key-hash))
      (let* ((key (mapconcat (lambda (x) (replace-in-string (downcase x) " " ""))
                             (list "na" "_"
                                   (let ((first-author (split-string (car (gethash "authors" key-hash)) " ")))
                                     (if (= 1 (length first-author)) (car first-author)
                                       (nth 1 first-author)))
                                   (if (gethash "year" key-hash) (format "%s" (gethash "year" key-hash))
                                     "_")
                                   (car (split-string (gethash "title" key-hash) " " t))) ""))
             (author '("author" . nil))
             (author (cons "author" (my/build-bib-author
                                     (string-join (gethash "authors" key-hash) ", "))))
             (title (cons "title" (gethash "title" key-hash)))
             (volume (cons "volume" (gethash "volume" key-hash)))
             (number (cons "number" (gethash "number" key-hash)))
             (tmp-pages (cons "pages" (gethash "pages" key-hash)))
             (pages (cons "pages" (if tmp-pages
                                  (replace-in-string
                                   (replace-in-string (format "%s" tmp-pages) "-" "--") " " "")
                                nil)))
             (year (cons "year" (format "%s" (gethash "year" key-hash))))
             (venue (cons "venue" (if (gethash "venue" key-hash)
                                      (replace-in-string (gethash "venue" key-hash) ",$" ""))))
             (entry (list key (remove-if-not 'cdr (list author title year venue volume number pages))))
             )
        entry)))

(defun my/org-bibtex-write-ref-NA-from-keyhash (key-hash)
  (my/org-bibtex-write-ref-from-assoc (my/generate-NA-entry key-hash)))

(defun my/org-bibtex-write-ref-from-assoc-misc (entry)
    (org-insert-heading-after-current)
    (insert (cdr (assoc :title entry)))
    (insert "\n")
    (org-insert-property-drawer)
    (loop for ent in entry
          do
          (if (not (string-equal (symbol-name (car ent)) ":type"))
              (org-set-property (upcase (car (cdr (split-string (symbol-name (car ent)) ":"))))
                                (cdr ent)))
          ))

(defun my/org-bibtex-write-ref-from-assoc-permissive (entry)
  "Generate an org entry from an association list retrieved via
json."
  (let* ((key (car entry))
         (entry (nth 1 entry))
         )
    (org-insert-heading-after-current)
    (insert (cdr (assoc "title" entry)))
    (insert "\n")
    (if (assoc "author" entry)
        (progn
          (org-indent-line)
          (insert (format "- Authors: %s\n" (my/remove-non-ascii (cdr (assoc "author" entry)))))))
    (if (and (assoc "venue" entry) (assoc "year" entry))
        (progn
          (org-indent-line)
          (insert (format "- %s\n" (concat (cdr (assoc "venue" entry)) ", " (cdr (assoc "year" entry)))))))
    (if (assoc "howpublished" entry)
        (progn
          (org-indent-line)
          (insert (format "- %s\n" (concat "Published as: " (cdr (assoc "venue" entry)))))))
    (org-insert-property-drawer)
    (loop for ent in entry
          do
          (if (not (string-equal (car ent) "abstract"))
              (org-set-property (upcase (car ent)) (my/remove-non-ascii (my/fix (cdr ent))))
            ))
    (org-set-property "CUSTOM_ID" key)
    (if (string-equal (assoc :type bib-assoc) "misc")
        (org-set-property "BTYPE" "misc")
      (org-set-property "BTYPE" "article"))
  ))

(defun my/org-bibtex-write-ref-from-assoc (entry)
  "Generate an org entry from an association list retrieved via
json."
  (let* ((key (car entry))
         (entry (nth 1 entry))
         )
    (org-insert-heading-after-current)
    (insert (cdr (assoc "title" entry)))
    (insert "\n")
    (org-indent-line)
    (if (assoc "abstract" entry)
        (progn (insert (cdr (assoc "abstract" entry)))
               (fill-paragraph)
               (insert "\n")
               (org-indent-line)))
    (insert (format "- Authors: %s"
                    (my/build-vernacular-author (my/remove-non-ascii (cdr (assoc "author" entry))))))
    (org-insert-item)
    (insert (concat (cdr (assoc "venue" entry)) ", " (cdr (assoc "year" entry))))
    (org-insert-property-drawer)
    (loop for ent in entry
          do
          (if (not (string-equal (car ent) "abstract"))
              (org-set-property (upcase (car ent)) (my/remove-non-ascii (my/fix (cdr ent))))
            ))
    (org-set-property "CUSTOM_ID" key)
    (org-set-property "BTYPE" "article")
  ))

;; This isn't used apparently?
;; TODO: Have to write functions for conversion to and from
;; org properties to bibtex.
(defun my/org-bibtex-write-heading-from-bibtex (entry)
  "Generate an org entry from a bibtex association list, parsed
with 'bibtex from a bibtex entry"
  (org-insert-heading)
  (insert (my/fix (cdr (assoc "title" entry))))
  (insert "\n")
  ;; from where do I get the abstract?
  ;; assuming abstract is in the bib entry
  (if (assoc "abstract" entry)
      (progn (insert (my/fix (cdr (assoc "abstract" entry))))
             (org-indent-line)
             (fill-paragraph)))
  (org-insert-property-drawer)
  (loop for ent in entry
        do
        (pcase ent
          (`("abstract" . ,_))
          (`("=type=" . ,_) (org-set-property "BTYPE" (my/fix (cdr ent))))
          (`("=key=" . ,_) (org-set-property "CUSTOM_ID" (my/fix (cdr ent))))
          (`(,_ . ,_) (org-set-property (upcase (car ent)) (my/fix (cdr ent)))))
        ))

(defun my/org-bibtex-read-bib-file-to-buffer (filename &optional buffername)
  (interactive "sFile: ")
  (if (file-exists-p filename)
      (let* ((buffername
             (if  buffername buffername (replace-in-string filename "\\.[a-z0-9]*?$" ".org")))
             (org-buf (my/generate-org-buffer buffername)))
        (setq org-bibtex-entries nil)
        (org-bibtex-read-file filename)
        (with-current-buffer org-buf
          (org-insert-heading)
          (loop for entry in org-bibtex-entries
                do
                (progn
                  (if (string-equal (cdr (assoc :type entry)) "misc")
                      (my/org-bibtex-write-ref-from-assoc-misc entry)
                    (my/org-bibtex-write-ref-from-assoc
                     (my/build-bib-assoc-from-parsed-org-bibtex entry)))
                  ))
          (goto-char (point-min))
          (delete-char 3)
          ))
    (message (format "[pdf-refs] File %s does not exist" filename)))
  )

(defun my/org-bibtex-read-from-headline ()
  (interactive)
  (if (string-equal major-mode "org-mode")
      (let* ((props (org-entry-properties))
             (bib-str (list
                       (cons "type"  (concat "@" (cdr (assoc "BTYPE" props))))
                       (cons "key"  (cdr (assoc "CUSTOM_ID" props)))
                       (cons "title"  (cdr (assoc "TITLE" props)))
                       (cons "author"  (cdr (assoc "AUTHOR" props)))
                       (cons "venue"  (cdr (assoc "VENUE" props)))
                       (cons "volume"  (cdr (assoc "VOLUME" props)))
                       (cons "number"  (cdr (assoc "NUMBER" props)))
                       (cons "year"  (cdr (assoc "YEAR" props)))
                       (cons "pages"  (cdr (assoc "PAGES" props)))
                       (cons "doi"  (cdr (assoc "DOI" props)))
                       (cons "url"  (cdr (assoc "URL" props)))
                       (cons "publisher"  (cdr (assoc "PUBLISHER" props)))
                       ))
             (header (concat (cdr (assoc "type" bib-str)) "{" (cdr (assoc "key" bib-str)) ",\n"))
             (bib-str (delq (assoc "type" bib-str) bib-str))
             (bib-str (delq (assoc "key" bib-str) bib-str)))
        (concat header
                (mapconcat (lambda (x)
                             (if (cdr x) (concat "  " (car x) "={" (cdr x) "},\n")))
                           bib-str "") "}\n"))
    (message "[pdf-refs] Not org mode")))

(defun my/org-bibtex-insert-headline-as-bib-to-file ()
  "Export current headline to kill ring as bibtex entry."
  (interactive)
  (let* ((result (my/org-bibtex-read-from-headline))
        (temp-bib-file-name (file-name-nondirectory temp-bib-file-path))
        (buf (if (get-buffer temp-bib-file-name) (get-buffer temp-bib-file-name)
               (find-file-noselect temp-bib-file-path))))
    (with-current-buffer buf (goto-char (point-min))
                         (insert result))))

(defun my/org-bibtex-convert-bib-to-property (assoc-list &optional buf)
  (let ((buf (if buf buf (current-buffer)))
        (entry assoc-list))
  (with-current-buffer buf
    (loop for ent in entry
          do
          (pcase ent
            (`("abstract" . ,_))
            (`("=type=" . ,_) (org-set-property "BTYPE" (my/fix (cdr ent))))
            (`("=key=" . ,_) (org-set-property "CUSTOM_ID" (my/fix (cdr ent))))
            (`(,_ . ,_) (org-set-property (upcase (car ent)) (my/fix (cdr ent)))))
          ))))

(defun my/sanitize-org-entry ()
  (let (retval)
    (condition-case ex
        (setq retval
              (cond
               ((not (equal (with-current-buffer my/org-heading-gscholar-launch-buffer major-mode)
                            'org-mode)) "Not org mode")
               ((not (with-current-buffer my/org-heading-gscholar-launch-buffer
                       (org-entry-get my/org-heading-gscholar-launch-point "CUSTOM_ID")))
                (with-current-buffer my/org-heading-gscholar-launch-buffer
                  (org-set-property "CUSTOM_ID" "na_"))
                "No property drawer or no property. Fixed")
               (t (with-current-buffer my/org-heading-gscholar-launch-buffer
                    (org-entry-get my/org-heading-gscholar-launch-point "CUSTOM_ID")))))
      ('error (message (format "[pdf-refs] Caught exception: [%s]" ex))))
    (message (concat "[pdf-refs] " retval)))
  )
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; End Org generation and insertion stuff ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;
;; Begin Biblio stuff ;;
;;;;;;;;;;;;;;;;;;;;;;;;
(defun org-search-heading-on-crossref-with-biblio ()
  "Searches for the current heading in google scholar in eww"
  (interactive)
  (setq my/org-heading-gscholar-launch-point (point))
  (setq my/org-heading-gscholar-launch-buffer (current-buffer))
  (save-excursion
    (let* ((query (org-get-heading t t))
           (target-buffer (window-buffer (my/get-or-create-window-on-side)))
           (backend #'biblio-crossref-backend)
           (results-buffer (biblio--make-results-buffer target-buffer query backend)))
      (biblio-url-retrieve
       (funcall backend 'url query)
       (my/biblio-callback results-buffer backend))
      results-buffer)))

(defun my/biblio-callback (results-buffer backend)
  "Generate a search results callback for RESULTS-BUFFER.
Results are parsed with (BACKEND 'parse-buffer)."
;; TODO: Did let not work here?
  (setq my/biblio-callback-buf results-buffer)
  (biblio-generic-url-callback
   (lambda () ;; no allowed errors, so no arguments
     "Parse results of bibliographic search."
     (let ((results (biblio--tag-backend 'biblio-crossref-backend
                                         (funcall 'biblio-crossref-backend 'parse-buffer)))
           (win (my/get-or-create-window-on-side)))
       (with-current-buffer my/biblio-callback-buf
         (my/biblio-insert-results results (biblio--search-results-header))
         (local-set-key (kbd "n") 'biblio--selection-next)
         (local-set-key (kbd "p") 'biblio--selection-previous)
         (local-set-key (kbd "o") 'my/parse-selected-entry-to-org))
       (set-window-buffer win my/biblio-callback-buf)
       (message "[pdf-refs] Tip: learn to browse results with `h'")))))

(defun my/parse-selected-entry-to-org ()
  (interactive)
  (biblio--selection-forward-bibtex #'my/biblio-insert-to-org))

(defun my/biblio-insert-to-org (bibtex entry)
  (let* ((current-key (with-current-buffer my/org-heading-gscholar-launch-buffer
                       (org-entry-get my/org-heading-gscholar-launch-point "CUSTOM_ID")))
         (bibtex (replace-in-string (replace-in-string
                  (progn (set-text-properties 0 (length bibtex) nil bibtex) bibtex)
                  "\n" "") "[[:blank:]]+" " "))
        (bib-assoc (with-temp-buffer (insert bibtex)
                                     (goto-char (point-min))
                                     (bibtex-parse-entry)))
        (new-key (my/build-bib-key-from-parsed-bibtex bib-assoc))
        (bib-assoc (remove-if 'my/is-bibtex-key bib-assoc)))
    (setf (alist-get "=key=" bib-assoc) new-key)
    (cond ((not bib-assoc) (message "[pdf-refs] Received nil entry"))
          ((string-match-p "na_" current-key)
           (my/org-bibtex-convert-bib-to-property bib-assoc my/org-heading-gscholar-launch-buffer))
          ((y-or-n-p "Authoritative entry. Really replace?")
           (my/org-bibtex-convert-bib-to-property bib-assoc my/org-heading-gscholar-launch-buffer)))
    (pop-to-buffer my/org-heading-gscholar-launch-buffer)
    ))

(defun my/biblio-insert-results (items &optional header)
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


;; DEPRECATED
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;; shr, eww hacks for my/org-bibtex ;;
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;; HACK
;; ;; Redefine shr-map
;; ;; TODO: remove redefine and use (bind-key)
;; (defvar shr-map
;;   (let ((map (make-sparse-keymap)))
;;     (define-key map "a" 'shr-show-alt-text)
;;     (define-key map "i" 'shr-browse-image)
;;     (define-key map "z" 'shr-zoom-image)
;;     (define-key map [?\t] 'shr-next-link)
;;     (define-key map [?\M-\t] 'shr-previous-link)
;;     (define-key map [follow-link] 'mouse-face)
;;     (define-key map [mouse-2] 'shr-browse-url)
;;     (define-key map "I" 'shr-insert-image)
;;     (define-key map "w" 'shr-copy-url)
;;     (define-key map "u" 'shr-copy-url)
;;     (define-key map "RET" 'shr-browse-url)
;;     (define-key map "o" 'shr-save-contents)
;;     (define-key map "\r" 'shr-browse-url)
;;     map))
;; (defun org-search-heading-on-gscholar-with-eww ()
;;   "Searches for the current heading in google scholar in eww"
;;   (interactive)
;;   ;; TODO: This code is repetitive. Dislike
;;   (setq my/org-heading-gscholar-launch-point (point))
;;   (setq my/org-heading-gscholar-launch-buffer (current-buffer))
;;   (save-excursion
;;     (let ((buf (generate-new-buffer " *scholar*"))
;;           (query-string (org-get-heading t t)))
;;       (with-current-buffer buf (insert (gscholar-bibtex-google-scholar-search-results
;;                                         query-string)))
;;       (shr-render-buffer buf)
;;       (if (get-buffer "*google-scholar*")
;;           (kill-buffer (get-buffer "*google-scholar*")))
;;       (pop-to-buffer-same-window "*html*")
;;       (rename-buffer "*google-scholar*")
;;       (with-current-buffer (get-buffer "*google-scholar*")
;;         (local-set-key (kbd "RET") 'eww-follow-link)
;;         (local-set-key (kbd "b") 'my/eww-get-bibtex-from-scholar t)
;;         (local-set-key (kbd "d") (lambda () (interactive)
;;                                    (my/eww-download-pdf-from-scholar t)))
;;         (local-set-key (kbd "i") 'my/import-link-to-org-buffer)
;;         (local-set-key (kbd "q") 'quit-window)
;;         (local-set-key (kbd "v") (lambda () (interactive)
;;                                    (my/eww-view-and-download-if-required-pdf-from-scholar t)))
;;         (read-only-mode))
;;       (kill-buffer buf))))
;; (defun my/eww-get-bibtex-from-scholar-rest ()
;;   (my/sanitize-org-entry)
;;   (let* ((buf (get-buffer " *scholar-entry*"))
;;          (buf-string (if buf (with-current-buffer buf (buffer-string))
;;                        (progn (message "[pdf-refs] Could not create buffer for scholar entry") nil)))
;;          (bib-assoc (if buf-string (cond ((string-match-p "systems have detected unusual" buf-string)
;;                                           (progn (message "[pdf-refs] Scholar is detecting a robot") nil))
;;                                          ((string-match-p "client does not have permission" buf-string)
;;                                           (progn (message "[pdf-refs] Scholar doesn't like EWW") nil))
;;                                          (t (with-current-buffer buf (goto-char (point-min)) (bibtex-parse-entry))))
;;                       (progn (message "[pdf-refs] Empty reply from scholar") nil)))
;;          (current-key (with-current-buffer my/org-heading-gscholar-launch-buffer
;;                         (org-entry-get (point) "CUSTOM_ID"))))
;;     (cond ((not bib-assoc) (message "[pdf-refs] Could not get entry from scholar"))
;;           ((string-match-p "na_" current-key)
;;            (my/org-bibtex-convert-bib-to-property bib-assoc my/org-heading-gscholar-launch-buffer))
;;           ((y-or-n-p "Authoritative entry. Really replace?")
;;            (my/org-bibtex-convert-bib-to-property bib-assoc my/org-heading-gscholar-launch-buffer)))
;;     (if buf (kill-buffer buf))
;;     ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; End shr, eww hacks for my/org-bibtex ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
