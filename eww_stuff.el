;; eww
;; Wrote a full different function as I didn't want to
;; mess with the global variables
(require 'eww)
(require 'shr)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; BEGIN gscholar functions ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(if (string-match-p "droid" (system-name))
    (setq my/pubs-directory "/home/joe/pubs/")
  (setq my/pubs-directory "/home/joe/PhD/Pubs/pubs/"))
(setq org-links-file-path (expand-file-name "~/.temp-org-links.org"))
;; (setq temp-bib-file-path (expand-file-name "~/lib/docprocess/all.bib"))
(setq temp-bib-file-path (expand-file-name "~/.temp.bib"))

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

(defun my/eww-check-bibtex-buffer-from-scholar ()
  (let* ((buf (get-buffer " *scholar-entry*"))
         (buf-string (if buf (with-current-buffer buf (buffer-string))
                       (progn (message "[pdf-refs] Could not create buffer for scholar entry") nil))))
    (if buf-string (cond ((string-match-p "systems have detected unusual" buf-string)
                          (progn (message "[pdf-refs] Scholar is detecting a robot") nil))
                         ((string-match-p "client does not have permission" buf-string)
                          (progn (message "[pdf-refs] Scholar doesn't like EWW") nil))
                         (t buf-string))
                      (progn (message "[pdf-refs] Empty reply from scholar") nil))))

;; called from an org buffer
;; TODO: Should be callable from any buffer with some modification
(defun org-search-heading-on-gscholar-with-eww ()
  "Searches for the current heading in google scholar in eww"
  (interactive)
  ;; TODO: This code is repetitive. Dislike
  (if (eq major-mode 'org-mode)
      (progn
        (setq my/org-heading-gscholar-launch-point (point))
        (setq my/org-heading-gscholar-launch-buffer (current-buffer))nnnn
        (save-excursion
          (let ((query-string (org-get-heading t t)))
            (my/eww-gscholar query-string))))
    (message "[pdf-refs] Not in org-mode")))

;; Called from M-x
(defun my/eww-gscholar (url)
  "Fetch URL and render the page.
If the input doesn't look like an URL or a domain name."
  (interactive
   (let* ((uris (eww-suggested-uris))
	  (prompt (concat "Enter URL or keywords"
			  (if uris (format " (default %s)" (car uris)) "")
			  ": ")))
     (list (read-string prompt nil nil uris))))
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
  (if (string-match-p "scholar\.google\.com\?q=" url)
      (let ((buf (generate-new-buffer " *scholar*")))
        (with-current-buffer buf (insert (gscholar-bibtex-google-scholar-search-results
                                          query-string)))
        (shr-render-buffer buf)
        (if (get-buffer "*google-scholar*")
            (kill-buffer (get-buffer "*google-scholar*")))
        (pop-to-buffer-same-window "*html*")
        (rename-buffer "*google-scholar*")
        (with-current-buffer (get-buffer "*google-scholar*")
          (local-set-key (kbd "RET") 'eww-follow-link)
          (local-set-key (kbd "b") 'my/eww-get-bibtex-from-scholar nil)
          (local-set-key (kbd "d") (lambda () (interactive)
                                     (my/eww-download-pdf-from-scholar t)))
          (local-set-key (kbd "i") 'my/import-link-to-org-buffer)
          (local-set-key (kbd "q") 'quit-window)
          (local-set-key (kbd "v") (lambda () (interactive)
                                     (my/eww-view-and-download-if-required-pdf-from-scholar t)))
          (read-only-mode))
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

;; ONLY called from my/eww-browse-url
(defun my/eww-render (status url buf org)
  (eww-render status url nil buf)
  (let ((buf-string (my/eww-check-bibtex-buffer-from-scholar)))
    (if buf-string
        (if org ;; Sanitize org entry and insert into org buffer
            (progn
              (my/sanitize-org-entry)
              (let ((bib-assoc (with-current-buffer buf (goto-char (point-min)) (bibtex-parse-entry)))
                    (current-key (with-current-buffer my/org-heading-gscholar-launch-buffer
                                   (org-entry-get (point) "CUSTOM_ID"))))
                (cond (((string-match-p "na_" current-key)
                        (my/org-bibtex-convert-bib-to-property bib-assoc my/org-heading-gscholar-launch-buffer))
                       ((y-or-n-p "Authoritative entry. Really replace?")
                        (my/org-bibtex-convert-bib-to-property bib-assoc my/org-heading-gscholar-launch-buffer))))))
          (let* ((temp-bib-file-name (file-name-nondirectory temp-bib-file-path))
                 (buf (if (get-buffer temp-bib-file-name) (get-buffer temp-bib-file-name)
                        (find-file-noselect temp-bib-file-path))))
            (with-current-buffer buf (goto-char (point-min))
                                      (insert buf-string))))
      (message "[pdf-refs] Could not get entry from scholar"))
    (if buf (kill-buffer buf))))

;; ONLY called from my/eww-get-bibtex-from-scholar
(defun my/eww-browse-url (url org)
  (let ((buf (if (get-buffer " *scholar-entry*") (get-buffer " *scholar-entry*")
               (generate-new-buffer " *scholar-entry*"))))
    (with-current-buffer buf (eww-setup-buffer)
                         (plist-put eww-data :url url)
                         (plist-put eww-data :title "")
                         (eww-update-header-line-format)
                         (let ((inhibit-read-only t))
                           (goto-char (point-min)))
                         (url-retrieve url 'my/eww-render
                                       ;; (list url nil (current-buffer)) t))))
                                       (list url (current-buffer) org)))))

;; Keypress b in google-scholar buffer
(defun my/eww-get-bibtex-from-scholar (&optional to-org)
  "Extracts the NEXT bibtex entry from a web page rendered with eww
and stores it to my/bibtex-entry"
  (interactive)
  (save-excursion
    (let
        ((bib-url (progn (search-forward "import into bibtex")
                         (backward-char)
                         (car (eww-links-at-point)))))
      (my/eww-browse-url bib-url to-org))))

;; Currently no TODO attribute is set on the org entry and no
;; timestamp is marked. I should fix that.
;;
;; Perhaps author and publication information can also be added as a
;; stopgap to see which paper to read first.
;;
;; The keybindings also have to be added.
;;
;; Now it's datetree format
;; Not used
;; (defun my/org-store-link-without-prompt ()
;;   (push (list link "link") org-stored-links))
;; Changed (org-insert-link nil link "link") to simple string insert
;; Much easier
(defun my/import-link-to-org-buffer ()
"Generates a temporary buffer (currently ~/temp-org-links)
and from the current eww buffer, copies the link there with an
org heading as title. Perhaps I can include author also there,
but that's too much work for now."
  (interactive)
  (save-excursion
    (let* ((eww-buf (current-buffer))
           (org-links-file-name (file-name-nondirectory org-links-file-path))
           (buf (if (get-buffer org-links-file-name) (get-buffer org-links-file-name)
                  (find-file-noselect org-links-file-path)))
           (link (get-text-property (point) 'shr-url))
           (link-text-begin (if link (progn
                                       (while (equal link (get-text-property (point) 'shr-url))
                                         (backward-char))
                                       (forward-char) (point)) nil))
           (link-text-end (if link (progn
                                     (while (equal link (get-text-property (point) 'shr-url))
                                       (forward-char)) (point)) nil))
           (metadata (if link (progn (forward-line 2)
                                     (with-current-buffer eww-buf
                                       (buffer-substring-no-properties (point-at-bol) (point-at-eol))))))
           )
      (if (and link-text-begin link-text-end)
          (with-current-buffer buf
            (org-mode)
            (org-datetree-find-date-create (org-date-to-gregorian (org-read-date t nil "now")))
            (goto-char (point-at-eol))
            (org-insert-subheading nil)
            (insert (concat (with-current-buffer eww-buf
                      (buffer-substring-no-properties link-text-begin link-text-end)) "\n"))
            (org-indent-line) (insert (concat metadata "\n"))
            (org-indent-line) (insert (concat "[[" link "][link]]"))
            (message (concat "[pdf-refs] " "Imported entry " (with-current-buffer eww-buf
                                                 (buffer-substring-no-properties link-text-begin link-text-end))
                             " into buffer " org-links-file-path))
            )
        (message "[pdf-refs] No link under point"))
      )))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; END gscholar functions ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;
;; BEGIN links handling ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Much cleaner code now
;; Although, I think I'll remove the debug code later
(defun my/eww-get-all-links (&optional frombegin substring)
  (interactive)
  (save-excursion
    (if frombegin (goto-char (point-min)))
    (setq my/eww-buffer-links nil)
    (setq my/prev-url (get-text-property (point) 'shr-url)) ;; was my/current-url
    (setq my/current-url nil)
    (setq my/url-text-start (point))
    (setq my/url-text-end (point))
    (while (not (eobp))
      ;; (debug)    
      ;; Debug info
      ;; (message (concat (format "%s" my/url-text-start) ", " (format "%s" my/url-text-end)))
      ;; (message (format "%s" (string-match-p substring
      ;;                                       (buffer-substring-no-properties my/url-text-start my/url-text-end))))
      (if my/prev-url
          (if substring
              ;; (if (string-match-p substring my/prev-url)
              ;; (buffer-substring-no-properties my/url-text-start my/url-text-end))
              ;; More Debug code
              ;;   (message (concat substring " matches " (buffer-substring-no-properties my/url-text-start my/url-text-end)))
              ;;   (message (concat "URL was set at " (format "%s" my/url-text-end)))
              ;; (if
              ;; was my/current-url
              ;; (progn
              ;;   (message (concat "Copying " my/current-url))
              ;; (setq my/eww-buffer-links (nconc my/eww-buffer-links (list my/current-url)))
              ;; 
              ;; (let ((url (get-text-property (- my/url-text-end 1) 'shr-url)))
              ;;   (if url (setq my/eww-buffer-links (nconc my/eww-buffer-links (list url)))
              ;;     (setq my/eww-buffer-links
              ;;           (nconc my/eww-buffer-links (list (get-text-property my/url-text-end 'shr-url)))
              ;;   )))
              ;; (progn
              ;;   (message (concat "Copying " my/prev-url))
              (if (string-match-p substring my/prev-url)
                  (setq my/eww-buffer-links (nconc my/eww-buffer-links (list my/prev-url)))
                )
            ;; (if my/prev-url
            (setq my/eww-buffer-links (nconc my/eww-buffer-links (list my/prev-url)))
            ))
    ;; (setq my/eww-buffer-links (nconc my/eww-buffer-links
    ;;                                  (list (get-text-property (- my/url-text-end 1) 'shr-url))))))
    (setq my/prev-url my/current-url)
    (setq my/current-url nil)
    (setq my/url-text-start (+ 1 (point)))
    (setq my/url-text-end (+ 1 (point)))
    (while (and (not (eobp))
                (equal (get-text-property (point) 'shr-url) my/prev-url))
      (forward-char 1))               ;; not next link (same link)
    (setq my/url-text-end (point))
    (setq my/current-url (get-text-property (point) 'shr-url))
    )
  my/eww-buffer-links))

(defun my/eww-view-and-download-if-required-pdf-from-scholar (frombegin)
  "View the pdf if it exists in the download directory and download if required"
  (interactive)
  (let* ((url (car (my/eww-get-all-links frombegin "pdf")))
            (obj (url-generic-parse-url url))
            (path (car (url-path-and-query obj)))
            (file (concat my/pubs-directory (file-name-nondirectory path))))
    (if (file-exists-p file)
        (find-file-other-window file)
      (my/eww-download-pdf-from-scholar frombegin t url))))

(defun my/eww-download-callback (status url)
  (unless (plist-get status :error)
    (let* ((obj (url-generic-parse-url url))
           (path (car (url-path-and-query obj)))
           (file (concat my/pubs-directory (file-name-nondirectory path))))
      (if (file-exists-p file)
          (if (y-or-n-p "File exists. Replace?")
              (progn (goto-char (point-min))
                     (re-search-forward "\r?\n\r?\n")
                     (write-region (point) (point-max) file)
                     (message "[pdf-refs] Saved %s" file)))
        (goto-char (point-min))
        (re-search-forward "\r?\n\r?\n")
        (write-region (point) (point-max) file)
        (message "[pdf-refs] Saved %s" file))
      )))

;; DONE: Maybe the file should be stored in Pubs somewhere, or some
;; other variable and the filename should be automatically inserted as
;; a link into the org file.
;; So instead of scholar searching I can simply view it from org.
;;
;; TODO: When a pdf is stored for a link, automatically extract
;; abstract from science-parse and add it to the abstract field.
;;
;; TODO: Fetch and parse pdfs in the background? That's too much
;; 
;; TODO: Should be async with wait from callback
(defun my/eww-download-pdf-from-scholar (frombegin &optional view url)
  "Downloads the pdf file and optionally view it"
  (interactive)
  (let ((url (if url url (car (my/eww-get-all-links frombegin "pdf")))))
    (if (not view)
        (url-retrieve url 'my/eww-download-callback (list url))
      (let* ((buf (url-retrieve-synchronously url t))
             (buf-string (with-current-buffer buf (buffer-string))))
        (if buf-string
            (let* ((obj (url-generic-parse-url url))
                   (path (car (url-path-and-query obj)))
                   (file (concat my/pubs-directory (file-name-nondirectory path))))
              ;; (eww-make-unique-file-name
              ;;  (eww-decode-url-file-name (file-name-nondirectory path))
              ;;  my/pubs-directory)
              (if (file-exists-p file)
                  (if (y-or-n-p "File exists. Replace?")
                      (with-current-buffer buf
                        (goto-char (point-min))
                        (re-search-forward "\r?\n\r?\n")
                        (write-region (point) (point-max) file)))
                (with-current-buffer buf
                  (goto-char (point-min))
                  (re-search-forward "\r?\n\r?\n")
                  (write-region (point) (point-max) file)))
              (find-file-other-window file)
              ))
        (kill-buffer buf))
      )))

;;;;;;;;;;;;;;;;;;;;;;;;
;; END links handling ;;
;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; BEGIN eww navigation and keymap ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; use gscholar specific bindings only on google scholar pages
(defun my/eww-previous ()
  (interactive)
  (let ((url (with-current-buffer (get-buffer "*eww*") (plist-get eww-data :url))))
    (if (string-match-p "scholar\.google\.com" url)
        (catch 'retval
          (save-excursion
            (goto-char (point-min))
            (while (search-forward "Previous")
              (let ((url (get-text-property (- (point) 1) 'shr-url)))
                (if (string-match-p "scholar.*start=" url)
                    (progn (eww-browse-url url) (message "Going to Previous Page")
                           (throw 'retval t)))
                ))))
      (eww-previous-url))))

(defun my/eww-next ()
  (interactive)
  (let ((url (with-current-buffer (get-buffer "*eww*") (plist-get eww-data :url))))
    (if (string-match-p "scholar\.google\.com" url)
        (catch 'retval
          (save-excursion
            (goto-char (point-min))
            (while (search-forward "Next")
              (let ((url (get-text-property (- (point) 1) 'shr-url)))
                (if (and url (string-match-p "scholar.*start=" url))
                    (progn (eww-browse-url url) (message "Going to Next Page")
                           (throw 'retval t)))
                ))))
      (eww-next-url))))

(defun my/eww-keypress-i ()
  (interactive)
  (let ((url (with-current-buffer (get-buffer "*eww*") (plist-get eww-data :url))))
    (if (string-match-p "scholar\.google\.com" url)
        (my/import-link-to-org-buffer)
      (eww-view-source))))

;; Only this function would be unusable with eww
;; (defun my/eww-keypress-v ()
;;   (interactive)
;;   (my/eww-view-and-download-if-required-pdf-from-scholar nil)
;;   )

(defun my/eww-keypress-v ()
  (interactive)
  (let ((url (with-current-buffer (get-buffer "*eww*") (plist-get eww-data :url))))
    (if (string-match-p "scholar\.google\.com" url)
        (my/eww-view-and-download-if-required-pdf-from-scholar nil)
      (eww-view-source))))

(defun my/eww-keypress-b ()
  (interactive)
  (let ((url (with-current-buffer (get-buffer "*eww*") (plist-get eww-data :url))))
    (if (string-match-p "scholar\.google\.com" url)
        (my/eww-get-bibtex-from-scholar nil)
      (eww-add-bookmark))))

(defun my/eww-keypress-d ()
  (interactive)
  (let ((url (with-current-buffer (get-buffer "*eww*") (plist-get eww-data :url))))
    (if (string-match-p "scholar\.google\.com" url)
        (my/eww-download-pdf-from-scholar nil)
      (eww-download))))

;;;;;;;;;;;;;;;;;;;;;;;;
;; END eww navigation ;;
;;;;;;;;;;;;;;;;;;;;;;;;

(defun my/eww-mode-hook ()
  (bind-key "b" 'my/eww-keypress-b eww-mode-map)
  (bind-key "b" 'my/eww-keypress-b eww-link-keymap)  
  (bind-key "i" 'my/eww-keypress-i eww-mode-map)
  (bind-key "i" 'my/eww-keypress-i eww-link-keymap)
  (bind-key "n" 'next-line eww-mode-map)
  (bind-key "p" 'previous-line eww-mode-map)
  (bind-key "v" 'my/eww-keypress-v eww-mode-map)
  (bind-key "v" 'my/eww-keypress-v eww-link-keymap)
  (bind-key "]" 'my/eww-next eww-mode-map)
  (bind-key "[" 'my/eww-previous eww-mode-map)
  ;; (bind-key "i" 'my/import-link-to-org-buffer shr-map)
  ;; (local-set-key (kbd "i") nil)
  ;; (local-set-key (kbd "i") 'my/import-link-to-org-buffer)
  ;; (define-key eww-mode-map (kbd "d") nil)
  ;; (define-key eww-mode-map (kbd "d d") 'eww-download)
  ;; (define-key eww-mode-map (kbd "d p") 'my/eww-download-paper)
  )

(add-hook 'eww-mode-hook 'my/eww-mode-hook)

(global-set-key (kbd "C-c e e") 'eww)
(global-set-key (kbd "C-c e g") 'my/eww-gscholar)
(setq browse-url-browser-function 'eww-browse-url)

;; (setq url-user-agent "User-Agent: Mozilla/5.0 (X11; Linux x86_64; rv:46.0) Gecko/20100101 Firefox/46.0")

;; DEPRECATED
;; (defun my/eww-get-bibtex-from-scholar (org)
;;   "Extracts the NEXT bibtex entry from a web page rendered with eww
;; and stores it to my/bibtex-entry"
;;   (interactive)
;;   (save-excursion
;;     (let
;;         ((bib-url (progn (search-forward "import into bibtex")
;;                          (backward-char)
;;                          (car (eww-links-at-point)))))
;;       (my/eww-browse-url bib-url org))))
;; (defun my/eww-gscholar-old (url)
;;   "Fetch URL and render the page.
;; If the input doesn't look like an URL or a domain name."
;;   (interactive
;;    (let* ((uris (eww-suggested-uris))
;; 	  (prompt (concat "Enter URL or keywords"
;; 			  (if uris (format " (default %s)" (car uris)) "")
;; 			  ": ")))
;;      (list (read-string prompt nil nil uris))))
;;   (setq url (string-trim url))
;;   (cond ((string-match-p "\\`file:/" url))
;; 	;; Don't mangle file: URLs at all.
;;         ((string-match-p "\\`ftp://" url)
;;          (user-error "FTP is not supported"))
;;         (t
;; 	 ;; Anything that starts with something that vaguely looks
;; 	 ;; like a protocol designator is interpreted as a full URL.
;;          (if (or (string-match "\\`[A-Za-z]+:" url)
;; 		 ;; Also try to match "naked" URLs like
;; 		 ;; en.wikipedia.org/wiki/Free software
;; 		 (string-match "\\`[A-Za-z_]+\\.[A-Za-z._]+/" url)
;; 		 (and (= (length (split-string url)) 1)
;; 		      (or (and (not (string-match-p "\\`[\"'].*[\"']\\'" url))
;; 			       (> (length (split-string url "[.:]")) 1))
;; 			  (string-match eww-local-regex url))))
;;              (progn
;;                (unless (string-match-p "\\`[a-zA-Z][-a-zA-Z0-9+.]*://" url)
;;                  (setq url (concat "http://" url)))
;;                ;; Some sites do not redirect final /
;;                (when (string= (url-filename (url-generic-parse-url url)) "")
;;                  (setq url (concat url "/"))))
;;            (setq url (concat "https://scholar.google.com/scholar?q="
;;                              (replace-regexp-in-string " " "+" url))))))
;;   (pop-to-buffer-same-window
;;    (if (eq major-mode 'eww-mode)
;;        (current-buffer)
;;      (get-buffer-create "*eww*")))
;;   (eww-setup-buffer)
;;   (plist-put eww-data :url url)
;;   (plist-put eww-data :title "")
;;   (eww-update-header-line-format)
;;   (let ((inhibit-read-only t))
;;     (insert (format "Loading %s..." url))
;;     (goto-char (point-min)))
;;   (url-retrieve url 'eww-render
;; 		(list url nil (current-buffer))))
;; ;; called from an org buffer
;; ;; TODO: Should be callable from any buffer with some modification
;; (defun org-search-heading-on-gscholar-with-eww ()
;;   "Searches for the current heading in google scholar in eww"
;;   (interactive)
;;   ;; TODO: This code is repetitive. Dislike
;;   (if (eq major-mode 'org-mode)
;;       (progn
;;         (setq my/org-heading-gscholar-launch-point (point))
;;         (setq my/org-heading-gscholar-launch-buffer (current-buffer))nnnn
;;         (save-excursion
;;           (let ((buf (generate-new-buffer " *scholar*"))
;;                 (query-string (org-get-heading t t)))
;;             (with-current-buffer buf (insert (gscholar-bibtex-google-scholar-search-results
;;                                               query-string)))
;;             (shr-render-buffer buf)
;;             (if (get-buffer "*google-scholar*")
;;                 (kill-buffer (get-buffer "*google-scholar*")))
;;             (pop-to-buffer-same-window "*html*")
;;             (rename-buffer "*google-scholar*")
;;             (with-current-buffer (get-buffer "*google-scholar*")
;;               (local-set-key (kbd "RET") 'eww-follow-link)
;;               (local-set-key (kbd "b") 'my/eww-get-bibtex-from-scholar)
;;               (local-set-key (kbd "d") (lambda () (interactive)
;;                                          (my/eww-download-pdf-from-scholar t)))
;;               (local-set-key (kbd "i") 'my/import-link-to-org-buffer)
;;               (local-set-key (kbd "q") 'quit-window)
;;               (local-set-key (kbd "v") (lambda () (interactive)
;;                                          (my/eww-view-and-download-if-required-pdf-from-scholar t)))
;;               (read-only-mode))
;;             (kill-buffer buf))))
;;     (message "[pdf-refs] Not in org-mode"))
;;   )
