(require 'bibtex)

(defconst ref-man-stop-words '("a" "about" "above" "after" "again" "against" "all" "an" "and" "any" "are" "as" "at" "because" "before" "below" "between" "both" "by" "can" "did" "do" "does" "don" "down" "during" "each" "few" "for" "from" "further" "had" "has" "have" "having" "here" "how" "i" "in" "into" "is" "it" "its" "just" "more" "most" "no" "nor" "not" "now" "of" "off" "on" "once" "only" "other" "out" "over" "own" "same" "should" "so" "some" "such" "t" "than" "that" "the" "then" "there" "these" "through" "to" "too" "under" "up" "very" "was" "were" "what" "when" "where" "which" "who" "why" "will" "with"))

(defvar ref-man-bibtex-ascii-replacement-strings
  '(("í" . "{\\\\'i}")
    ("æ" . "{\\\\ae}")
    ("ć" . "{\\\\'c}")
    ("é" . "{\\\\'e}")
    ("ä" . "{\\\\\"a}")
    ("è" . "{\\\\`e}")
    ("à" . "{\\\\`a}")
    ("á" . "{\\\\'a}")
    ("ø" . "{\\\\o}")
    ("ë" . "{\\\\\"e}")
    ("ü" . "{\\\\\"u}")
    ("ñ" . "{\\\\~n}")
    ("ņ" . "{\\\\c{n}}")
    ("ñ" . "{\\\\~n}")
    ("å" . "{\\\\aa}")
    ("ö" . "{\\\\\"o}")
    ("á" . "{\\\\'a}")
    ("í" . "{\\\\'i}")
    ("ó" . "{\\\\'o}")
    ("ó" . "{\\\\'o}")
    ("ú" . "{\\\\'u}")
    ("ú" . "{\\\\'u}")
    ("ý" . "{\\\\'y}")
    ("š" . "{\\\\v{s}}")
    ("č" . "{\\\\v{c}}")
    ("ř" . "{\\\\v{r}}")
    ("š" . "{\\\\v{s}}")
    ("İ" . "{\\\\.i}")
    ("ğ" . "{\\\\u{g}}")
    ("δ" . "$\\\\delta$")
    ("ç" . "{\\\\c{c}}")
    ("ß" . "{\\\\ss}")
    ("≤" . "$\\\\le$")
    ("≥" . "$\\\\ge$")
    ("<" . "$<$")
    ("θ" . "$\\\\theta$")
    ("μ" . "$\\\\mu$")
    ("→" . "$\\\\rightarrow$")
    ("⇌" . "$\\\\leftrightharpoons$")
    ("×" . "$\\\\times$")
    ("°" . "$\\\\deg$")
    ("ş" . "{\\\\c{s}}")
    ("γ" . "$\\\\gamma$")
    ("ɣ" . "$\\\\gamma$")
    ("º" . "degc")
    ("η" . "$\\\\eta$")
    ("µ" . "$\\\\mu$")
    ("α" . "$\\\\alpha$")
    ("β" . "$\\\\beta$")
    ("ɛ" . "$\\\\epsilon$")
    ("ⅵ" . "\textrm{vi}")
    ("ⅲ" . "\textrm{iii}")
    ("ⅴ" . "\textrm{v}")
    ("λ" . "$\\\\lambda$")
    ("π" . "$\\\\pi$")
    ("∞" . "$\\\\infty$")
    ("χ" . "$\\\\chi$")
    ("∼" . "\\\\textasciitilde{}")
    ("‑" . "\\\\textemdash{}")
    (" " . " ")
    ("…" . "...")
    ("•" . "\\\\textbullet ")
    (" " . " ")
    (" " . " ")
    (" " . " ")
    ("–" . "-")
    ("−" . "-")
    ("–" . "-")
    ("—" . "-")
    ("‒" . "\\\\textemdash{}")
    ("‘" . "'")
    ("’" . "'")
    ("’" . "'")
    ("“" . "\"")
    ("’" . "'")
    ("”" . "\""))
  "Replace non-ascii characters with escaped ones for latex rendering.
The characters here directly borrowed from `org-ref'.
See `org-ref-nonascii-latex-replacements'")

(defsubst cdass (elem alist)
  "Short for (cdr (assoc ELEM) list).
Argument ALIST association list."
  (when (assoc elem alist)
    (cdr (assoc elem alist))))

(defun url-join (&rest elements)
  "Join ELEMENTS with a single \"/\", like a url."
  (string-join (-remove #'string-empty-p
                        (mapcar (lambda (x)
                                  (string-remove-prefix "/" (string-remove-suffix "/" x)))
                                elements)) "/"))

(defun path-join (&rest elements)
  "Join ELEMENTS as a path, expects full paths."
  (concat "/" (mapconcat (lambda (x)
                           (string-remove-prefix "/" (string-remove-suffix "/" x)))
                         elements "/")))

(defun dir-equal-p (dir-a dir-b)
  "Return non-nil if full paths for DIR-A and DIR-B are equal.
They need not exist."
  (string= (string-remove-suffix "/" (expand-file-name dir-a))
           (string-remove-suffix "/" (expand-file-name dir-b))))

;; TODO: Document this
(defun max-ind (seq)
  (let* ((max-ind--max-val 0) (max-ind--temp-ind -1) (max-ind--max 0))
    (cl-loop for x in seq
          do
          (progn
            (setq max-ind--temp-ind (+ 1 max-ind--temp-ind))
            (if x (if (> x max-ind--max-val)
                      (progn (setq max-ind--max-val x)
                             (setq max-ind--max max-ind--temp-ind))))))
    max-ind--max))

(defun find-open-port (init)
  "Find the next open port from INIT in case it's being used by another process."
  (cl-loop for port from init to 65531
        when (string-match-p
              "refused" (shell-command-to-string
                         (format "nc -z -v localhost %s" port)))
        return port))

(defun replace-in-string (in what with)
  "Hackey replace in string.
Replace IN string WITH WHAT."
  (replace-regexp-in-string
   (regexp-quote what) with in nil 'literal))
;; (make-obsolete 'replace-in-string 'replace-regexp-in-string "")

(defun sort-symbols (reverse beg end)
  "Sort symbols in region alphabetically, in REVERSE if negative.
BEG and END are region markers.  See `sort-words'."
  (interactive "*P\nr")
  (sort-regexp-fields reverse "\\(\\sw\\|\\s_\\)+" "\\&" beg end))

;; TODO: Maybe rename this to a more generic name
(defun ref-man--get-or-create-window-on-side ()
  "This is a copy of the function in util.el."
  (let* ((orig-win (selected-window))
         (win (cond ((window-in-direction 'right orig-win)
                     (window-in-direction 'right orig-win))
                    ((window-in-direction 'left orig-win)
                     (window-in-direction 'left orig-win))
                    (t (split-window-horizontally)))))
    win))

;; CHECK: Should we keep non-ascii?
(defun ref-man--remove-punc (x &optional keep-spaces)
  "Return only alphanumeric characters for string X.
With optional KEEP-SPACES non-nil, don't remove the spaces."
  (if keep-spaces
      (replace-regexp-in-string "[^0-9a-z ]" "" x)
    (replace-regexp-in-string "[^0-9a-z]" "" x)))

(defun ref-man-remove-bookmarks-from-pandoc-tex ()
  "Remove bookmarks from tex generated by pandoc.
The strings are of the form
\"{[}\\\\protect\\\\hyperlink{ref-author2020title}{20}{]}\".
They are replaced with [num], e.g., previous one is replaced with
[20]."
  (interactive)
  (save-excursion
    (save-restriction
      (when (region-active-p)
        (narrow-to-region (region-beginning) (region-end))
        (goto-char (point-min)))
      (while (re-search-forward "{\\[}.*?{\\([0-9]+\\)}{\\]}" nil t nil)
        (replace-match "[\\1]")))))

(defun ref-man--trim-whitespace (str &optional remove-quotes)
  "Trim the input string STR.
Remove newlines and multiple spaces with a single one.  With
optional REMOVE-QUOTES remove all quotes \" from the string
also."
  (let ((str (replace-regexp-in-string "[ \t]+" " "
                                       (replace-regexp-in-string "\n" "" (string-trim str)))))
    (if remove-quotes
        (replace-regexp-in-string "\"" "" str)
      str)))

(defun ref-man--fix-curly (str)
  "Gets text between parentheses for {STR}."
  (string-remove-suffix "}" (string-remove-prefix "{" str)))

(defun ref-man--bibtex-key-p (item)
  "ITEM is a bibtex key."
  (string= (car item) "=key="))

(defun ref-man--stop-word-p (x)
  "X is a stop word."
  (member x ref-man-stop-words))

;; CHECK: Might be faster with a pcase
(defun ref-man--transcribe (str change-list)
  "Transcribe non-ascii characters in STR to ASCII lookalikes.
Argument CHANGE-LIST is an alist of regexps, `(A . B)' changes
from A to B."
  (let ((content str)
        (change-list (or change-list bibtex-autokey-transcriptions)))
    (dolist (pattern change-list)
      (setq content (replace-regexp-in-string (car pattern)
                                              (cdr pattern)
                                              content t)))
    content))


(defun ref-man--replace-non-ascii (str)
  "Replace non-ascii characters in STR with escape codes.

Uses `ref-man-bibtex-ascii-replacement-strings' for replacements.
If `org-ref-nonascii-latex-replacements' exists, then the
replacements are a union of both above alists."
  (ref-man--transcribe str (or (and (boundp 'org-ref-nonascii-latex-replacements)
                                    (-union ref-man-bibtex-ascii-replacement-strings
                                            org-ref-nonascii-latex-replacements))
                               ref-man-bibtex-ascii-replacement-strings)))

(provide 'ref-man-util)
