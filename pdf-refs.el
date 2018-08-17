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
(require 'gscholar-bibtex)

;;
;; utility functions
;;
(defun firstn (x n)
  (butlast x (- (length x) n)))

(defun butfirst (x n)
  (last x (- (length x) n)))

(defun my/venue-pref (results)
  (if (= 1 (length results))
      0
    (let*
        ((venues (mapcar (lambda (x)
                          (gscholar-bibtex--xml-get-child x 'venue)) results))
         (prefs (mapcar (lambda (x) (cdr (assoc (car (last x)) my/venue-priorities))) venues)))
         prefs)))

(defun my/max-ind (seq)
  (let* ((my/max-val 0) (my/ind -1) (my/max 0))
    (loop for x in seq
          do
          (progn
            (setq my/ind (+ 1 my/ind))
            (if x (if (> x my/max-val)
                      (progn (setq my/max-val x)
                             (setq my/max my/ind))))))
    my/max))

(defun my/fix (str)
  "gets text between parentheses {}"
  (string-remove-suffix "}" (string-remove-prefix "{" str)))

;;
;; Constants. perhaps can name them better
;;
(setq my/venue-priorities (let* ((confs '("icml" "nips" "iccv" "cvpr" "eccv"))
       (confs-seq (number-sequence (length confs) 1 -1)))
       (mapcar* 'cons confs confs-seq)))
(setq my/key-list '(authors title venue volume number pages year doi ee))
(setq my/org-store-dir "/home/joe/phd/pubs/org")
(setq my/bib-store-dir "/home/joe/phd/pubs/bibs")

;;
;; clean the xml entry and keep relevant itmes. uses gscholar-bibtex
;; 
(defun my/dblp-clean (result)
  "cleans the xml entry and keeps relevant itmes according to
my/key-list. uses gscholar-bibtex. returns an assoc list of (key. value) 
pairs for only the top result from my/venue-priorities."
  (let ((result (nth (my/max-ind (my/venue-pref result)) result)))
    (if result ;; TODO handle this later    
        (remove '("nil") (mapcar (lambda (x)
                  (if (eq x 'authors)
                      (list (symbol-name 'authors) (string-join (mapcar (lambda (x)
                                                                          (car (last x))) (butfirst (gscholar-bibtex--xml-get-child result x) 2)) ", "))
                    (cons (symbol-name (first (gscholar-bibtex--xml-get-child result x)))
                          (last (gscholar-bibtex--xml-get-child result x)))))
                                 my/key-list)))))

;;
;; TODO: remove stop words from first title word
;;
;; TODO: build a different function so that \{etc} aren't there in bib keys
;; my/remove-non-ascii does something else entirely and I don't want accents
;; in the keys
(defun my/build-bib-key (key-str)
  "builds a unique key with the format [author year
  first-title-word] entry from the list of (key . value)"
  (let* ((first-author-str (car (split-string (car (cdr (assoc "authors" key-str))) "," t)))
         (first-author (my/validate-author (split-string first-author-str " " t)))
         (last-name (car (last first-author)))
         (year-pub (car (cdr (assoc "year" key-str))))
         (title-first (first (split-string (car (cdr (assoc "title" key-str))) " ")))
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
         (author-str (replace-in-string (replace-in-string author-str ".$" "") ",$" ""))         
         (authors (split-string author-str "," t))
         (result-authors (mapcar (lambda (x)
                                   (let ((temp-auth (my/validate-author (split-string x " " t))))
                                     (if (= 1 (length temp-auth)) (car temp-auth)
                                       (mapconcat 'car (list (last temp-auth) (butlast temp-auth)) ", "))
                                     ))
                                 authors)))
    (mapconcat 'identity result-authors " and ")))


(defun my/build-bib-assoc (key-str)
  "builds the association list. can be used to build both the bib
entry and org entry"
  (let* ((key (my/build-bib-key key-str))
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


(defun my/get-references ()
  "this is the only entry point to fetch and write the references
to a buffer right now. can change to have it in multiple steps."
  (interactive)
  (setq my/dblp-results ())
  (let*
      ((pdf-file-name (expand-file-name (buffer-file-name (current-buffer))))
       (json-string (shell-command-to-string (format "curl -s -H\
       \"content-type: application/pdf\" --data-binary @%s\
       \"http://localhost:9090/v1\"" pdf-file-name)))
       (json-object-type 'hash-table)
       (json-key-type 'string)
       (json-array-type 'list)
       (json-string (json-read-from-string json-string))
       ;; concats title and authors for each ref entry for easy lookup
       (refs-list (mapcar (lambda (x)
                            (cons (concat (gethash "title" x) " " (string-join (gethash "authors" x) " ")) x))
                          (gethash "references" json-string))))
    (setq my/science-parse-data json-string)
    (setq my/abstract (gethash "abstracttext" json-string))
    (setq my/title (gethash "title" json-string))
    (setq my/refs-list refs-list)
    (setq my/bibs "")
    (setq my/orig-win (selected-window))
    (my/generate-primary-buffer)
    (my/dblp-fetch-parallel refs-list)
    )
  nil)


(defun my/get-org-buffer ()
  "Generated buffer where all the fetch results will be inserted"
  (let ((buf (get-buffer (concat my/title "_org"))))
    buf))


(defun my/generate-org-buffer ()
  "Generated buffer where all the fetch results will be inserted"
  (let ((buf (get-buffer-create (concat my/title "_org")))
        (win (cond ((window-in-direction 'right my/orig-win)
                    (window-in-direction 'right my/orig-win))
                   ((window-in-direction 'left my/orig-win)
                    (window-in-direction 'left my/orig-win))
                   (split-window-horizontally))))
    (set-window-buffer win buf)
    (with-current-buffer buf (org-mode))
    buf))

;;
;; Maybe python-epc would be better.
;;
(defun my/dblp-fetch-parallel (refs-list)
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
            ,(async-inject-variables "ref-refs")
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
                                        )
                                       ))
                      (org-buf (my/get-org-buffer))
                      )
               (with-current-buffer org-buf
                 (if key-str (my/org-bibtex-write-ref-from-assoc (my/build-bib-assoc key-str))
                   (my/org-bibtex-write-ref-NA-from-keyhash (cdr ref-refs))))
               (kill-buffer guf)
             )))))))

;;
;; Called by my/generate-primary-buffer
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

(defun my/generate-primary-buffer ()
  (let* ((org-buf (my/generate-org-buffer))
         (bib-buf (my/generate-bib-buffer))
         (key-str (my/dblp-fetch-serial  ;; assoc list
                   (concat
                    (replace-regexp-in-string "[^\t\n\r\f -~]" ""  (gethash "title" my/science-parse-data)) " "
                    (string-join (mapcar (lambda (x) (gethash "name" x))
                                         (gethash "authors" my/science-parse-data)) " "))))
         ;; What if not key-str?
         (filename (my/build-bib-key key-str))
         )
    ;; What if not filename?
    (if filename
        (with-current-buffer org-buf
          (my/org-bibtex-write-heading-from-assoc (my/build-bib-assoc key-str))
          (org-insert-heading-after-current)
          (org-shiftmetaright)
          ;; TODO: fix setting of file name
          ;; (set-visited-file-name
          ;;  (concat (string-remove-suffix "/" my/org-store-dir) "/" filename ".org"))
          ))
    ))


(defun my/org-bibtex-write-heading-from-assoc (entry)
  "Generate an org entry from an association list retrieved via
json."
  (let* ((key (car entry))
         (entry (nth 1 entry))
         )
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


(defun my/org-bibtex-write-ref-NA-from-keyhash (key-hash)
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
        (my/org-bibtex-write-ref-from-assoc entry))))
  

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
    (insert (format "- Authors: %s" (my/remove-non-ascii (cdr (assoc "author" entry)))))
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


(defun my/pdf-refs-init ()
  ;; auth just to be safe
  (async-start-process "auth" "/home/joe/bin/myauth" nil)
  (if (not (string-match-p "akshay@10.5.0.88" (shell-command-to-string  "ps -ef | grep ssh")))
      (async-start-process "ls" "ssh" nil "-N" "-L" "9090:localhost:8080" "akshay@10.5.0.88"))
  (sleep-for 1)
  (if (not (string-match-p "Usage"
                           (shell-command-to-string "curl -s localhost:9090")))
      (message "ERROR! Check connections") (message "Established connection to server successfully")))


(my/pdf-refs-init)
