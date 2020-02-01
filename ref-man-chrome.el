(add-to-list 'load-path ".")
(load "ref-man.el")
(require 'websocket)


(defcustom ref-man-chrome-user-data-dir
  (concat (getenv "HOME") "/.config/chromium")
  "Chromium profile directory"
  :type 'directory
  :group 'ref-man)

(defcustom ref-man-chrome-port-number-start-from
  9222
  "Where to start port numbers for the debugger"
  :type 'interger
  :group 'ref-man)

(setq ref-man-chrome--chromium-port nil)
(setq ref-man-chrome--history-list nil)
(setq ref-man-chrome--rpc-id 0)
(setq ref-man-chrome--rpc-callbacks nil)
(setq ref-man-chrome--sockets nil)
(setq ref-man-chrome--tabs nil)
(setq ref-man-chrome--initialized-p nil)

;; I'm calling it ref-man-chrome- sub package
;; A free open port can be chosen randomly and chromium or chromium-browser can start
;; Check for "google-chrome", "chromium", "chromium-browser"
;; "which [chrome]" doesn't always correspond to the file in the running process
(defun ref-man-chrome--which-chromium ()
  (setq ref-man-chrome-command
        (replace-regexp-in-string
         "\n" "" 
         (cond ((not (string-equal "" (shell-command-to-string "which chromium-browser")))
                (shell-command-to-string "which chromium-browser"))
               ((not (string-equal "" (shell-command-to-string "which chromium")))
                (shell-command-to-string "which chromium"))
               ((not (string-equal "" (shell-command-to-string "which google-chrome")))
                (shell-command-to-string "which google-chrome"))
               (t nil)))))

(defun ref-man-chrome--chromium-running ()
  (let* ((chrome (file-name-nondirectory (ref-man-chrome--which-chromium)))
         (grep-string (concat "ps -ef | grep " chrome))
         (text (replace-regexp-in-string ".*grep.*" ""
                                         (shell-command-to-string grep-string))))
    (string-match-p chrome text)))

(defun ref-man-chrome--find-open-port ()
  "Finds the next open port from
`ref-man-chrome-port-number-start-from' in case
`ref-man-chrome-port-number-start-from' is being used by another
process"
  (loop for port from ref-man-chrome-port-number-start-from to 65531
        when (string-match-p
              "refused" (shell-command-to-string
                         (format "nc -z -v localhost %s" port)))
        return port))

(defun ref-man-chrome--process-helper (headless data-dir port)
  (message "[ref-man] Starting Chromium process")
  (if headless
      (start-process "chromium" "*chromium*" (ref-man-chrome--which-chromium)
                     "--headless" (concat "--user-data-dir=" data-dir) (format "--remote-debugging-port=%s" port))
    (start-process "chromium" "*chromium*" (ref-man-chrome--which-chromium)
                   (concat "--user-data-dir=" data-dir) (format "--remote-debugging-port=%s" port)))
  (sleep-for 2))

;; TODO: Fix for named process
(defun ref-man-chrome--kill-chrome-process ()
  "Kill the chrome process"
  (shell-command
   (concat "killall " (file-name-nondirectory (ref-man-chrome--which-chromium)))))

(defun ref-man-chrome-start-process (headless)
  "Start a new chromium process."
  (interactive)
  (let ((command (ref-man-chrome--which-chromium))
        (port (ref-man-chrome--find-open-port))
        (data-dir ref-man-chrome-user-data-dir))
    (setq ref-man-chrome--chromium-port port)
    (if (ref-man-chrome--chromium-running)
        (if (y-or-n-p "Existing chrome process found. Kill it and start headless?")
            (progn
              (ref-man-chrome--kill-chrome-process)
              (ref-man-chrome--process-helper headless data-dir port))
          (message "[ref-man] Not starting chromium"))
      (ref-man-chrome--process-helper headless data-dir port))))

(defun ref-man-chrome--check-port (port)
  (let* ((url (url-parse-make-urlobj
               "http" nil nil "localhost" port "/"))
         (url-request-method "GET")
         (url-http-attempt-keepalives nil)
         (buf (url-retrieve-synchronously url nil nil 1)))
    (with-current-buffer buf
      (when (string-match-p "headless remote" (buffer-string))
        t))))

(defun ref-man-chrome-get-json ()
  (let* ((url (url-parse-make-urlobj
               "http" nil nil "localhost" ref-man-chrome--chromium-port "/json"))
         (url-request-method "GET")
         (url-http-attempt-keepalives nil)
         (json-array-type 'list)        ; changes to list instead of vector
         (json-object-type 'plist))     ; Not sure how this effects
    ;; (buf (url-retrieve-synchronously url nil nil 1)))
    (with-current-buffer (url-retrieve-synchronously url)
      (if (not (eq 200 (url-http-parse-response)))
          (error "Unable to connect to host.")
        (goto-char (+ 1 url-http-end-of-headers))
        (json-read)))))

(defun ref-man-chrome-get-tabs ()
  (let* ((json-string (ref-man-chrome-get-json))
         (tabs (-filter (lambda (tab)
                          (and (plist-get tab :webSocketDebuggerUrl)
                               (string-equal (plist-get tab :type) "page")))
                        json-string)))
    tabs))

(defun ref-man-chrome-reset ()
  (setq ref-man-chrome--chromium-port nil)
  (setq ref-man-chrome--rpc-id 0)
  (setq ref-man-chrome--rpc-callbacks nil)
  (setq ref-man-chrome--sockets nil)
  (setq ref-man-chrome--initialized-p nil)
  (setq ref-man-chrome--tabs nil))

(defun ref-man-chrome--encode (data)
  (let ((json-array-type 'list)
        (json-object-type 'plist))
    (json-encode data)))

(defun ref-man-chrome--decode (data)
  (let ((json-array-type 'list)
        (json-object-type 'plist))
    (json-read-from-string data)))

(defun ref-man-chrome--next-rpc-id ()
  (setq ref-man-chrome--rpc-id (+ 1 ref-man-chrome--rpc-id))
  ref-man-chrome--rpc-id)

(defun ref-man-chrome--register-callback (id fn)
  (let ((hook (intern (number-to-string id) ref-man-chrome--rpc-callbacks)))
    (add-hook hook fn t)))

(defun ref-man-chrome--dispatch-callback (id data)
  (let ((hook (intern (number-to-string id) ref-man-chrome--rpc-callbacks)))
    (when hook
      (run-hook-with-args hook data)
      (unintern hook ref-man-chrome--rpc-callbacks))))

;; It's called method but it's actually THE CALL
(defun ref-man-chrome-call-rpc (method socket &optional params callback)
  (let ((id (ref-man-chrome--next-rpc-id)))
    (when callback
      (message (concat "[ref-man-chrome]: " "ID: "(format "%s" id) "CALLBACK: " (format "%s" callback)))
      (ref-man-chrome--register-callback id callback))
    (websocket-send-text
     socket
     (ref-man-chrome--encode (list :id id
                                   :method method
                                   :params params)))))

(defun ref-man-chrome--on-open (socket)
  (message "[ref-man-chrome]: Opened new websocket"))

(defun ref-man-chrome--on-close (socket)
  (message "[ref-man-chrome]: closed websocket"))
  ;; (message "[ref-man: Closed connection to " (format "%s") tab))

(defun ref-man-chrome--on-message-added (data)
  (let* ((-message (plist-get data :message))
         (url (plist-get message :url))
         (column (plist-get message :column))
         (line (plist-get message :line))
         (type (plist-get message :type))
         (level (plist-get message :level))
         (text (plist-get message :text)))
    (message (format "[ref-man-chrome]: %s" -message))))

;; (defun kite-mini-on-script-parsed (data)
;;   (let ((extension? (plist-get data :isContentScript))
;;         (url (plist-get data :url))
;;         (id (plist-get data :scriptId)))
;;     (when (and (eq extension? :json-false) (not (string-equal "" url)))
;;       (add-to-list 'kite-mini-rpc-scripts (list :id id :url url)))))

;; (defun kite-mini-on-script-failed-to-parse (data)
;;   (kite-mini-console-append (format "%s" data)))

(defun ref-man-chrome--on-message (socket data)
  (let* ((data (ref-man-chrome--decode (websocket-frame-payload data)))
         (method (plist-get data :method))
         (params (plist-get data :params)))
    (pcase method
      ;; ("Debugger.scriptParsed" (ref-man-chrome--on-script-parsed params))
      ;; we are getting an error in Console.messageAdded
      ;; ("Debugger.scriptFailedToParse" (kite-mini-on-script-failed-to-parse params))
      ("Console.messageAdded" (ref-man-chrome--on-message-added params))
      ;; ;; TODO: do something usefull here, possibly great for REPL
      ("Console.messageRepeatCountUpdated")
      ;; nil -> These are return messages from RPC calls, not notification
      (_ (if method
             (message "[ref-man-chrome]: %s" data) ; Generic fallback, only used in development
           (ref-man-chrome--dispatch-callback (plist-get data :id)
                                              (plist-get data :result)))))))

(defun ref-man-chrome--open-socket (url)
  (message "[ref-man-chrome]: Opening socket for" url)
  (websocket-open url
                  :on-open #'ref-man-chrome--on-open
                  :on-message #'ref-man-chrome--on-message
                  :on-close #'ref-man-chrome--on-close))

(defun ref-man-chrome--get-socket-url-for-tab (id)
  (let* ((tabs (ref-man-chrome-get-tabs))
         (tab-ids (mapcar (lambda (x) (plist-get x :id)) tabs))
         (sockets (mapcar (lambda (x)
                            (cons (plist-get x :id) (plist-get x :webSocketDebuggerUrl)))
                          tabs))
         (indx (if ref-man-chrome--tabs
                   (+ 1 (apply 'max (mapcar 'cdr ref-man-chrome--tabs))) 0)))
    (loop for x in tab-ids
          do (progn (when (not (assoc x ref-man-chrome--tabs))
                      (push (cons x indx) ref-man-chrome--tabs)
                      (incf indx))))
    (cdr (assoc (car (rassoc id ref-man-chrome--tabs)) sockets))))

;; TODO: Check for already connected
(defun ref-man-chrome-connect (id)
  (let* ((socket-url (ref-man-chrome--get-socket-url-for-tab id))
         (socket (ref-man-chrome--open-socket socket-url)))
    (setq ref-man-chrome--sockets (plist-put ref-man-chrome--sockets id socket))
    ;; (setq ref-man-chrome--sockets (nconc ref-man-chrome--sockets (list socket)))
    (ref-man-chrome-call-rpc "Console.enable" socket)
    (ref-man-chrome-call-rpc "Debugger.enable" socket)
    (ref-man-chrome-call-rpc "Network.setCacheDisabled" socket
                        '(:cacheDisabled t))))

(defun ref-man-chrome-eval (code id &optional callback)
  "Evaluate expression for either tab 0 or 1"
  (ref-man-chrome-call-rpc
   "Runtime.evaluate"
   (plist-get ref-man-chrome--sockets id)
   (list :expression code
         :returnByValue t)
   callback))

(defun ref-man-chrome-shutdown (&optional restart)
  "Kill the async process and reset"
  (ref-man-chrome--kill-chrome-process)
  (ref-man-chrome-reset))

(defun ref-man-chrome-init ()
  (if (not ref-man-chrome--initialized-p)
      (progn
        (ref-man-chrome-start-process t)
        (setq ref-man--test-var nil)
        (setq my/test-loop-iter 0)
        (while (not ref-man--test-var)        
          (setq my/test-loop-iter (+ 1 my/test-loop-iter))
          (condition-case ref-man--test-var
              (progn (ref-man-chrome-connect 0)
                     (setq ref-man--test-var t))
            (error nil)))
        (ref-man-chrome-eval "location.href = \"https://scholar.google.com\"" 0
                             (lambda (result) (message (concat "[ref-man] Set URL of tab 0 to "
                                                               (plist-get (plist-get result :result) :value)))))
        (ref-man-chrome-eval "window.open();" 0)
        (sleep-for 1)
        (ref-man-chrome-connect 1)
        (setq ref-man-chrome--initialized-p t)
        (message "[ref-man] Chromium plugin initialized"))
    (message "[ref-man] Chromium plugin was already initialized.")))


;; (defun ref-man--bibtex-generate-autokey (names year title)
;;   (let ((autokey (concat bibtex-autokey-prefix-string
;;                          names
;;                          (unless (or (equal names "")
;;                                      (equal year ""))
;;                            bibtex-autokey-name-year-separator)
;;                          year
;;                          (unless (or (and (equal names "")
;;                                           (equal year ""))
;;                                      (equal title ""))
;;                            bibtex-autokey-year-title-separator)
;;                          title)))
;;     (if bibtex-autokey-before-presentation-function
;;         (funcall bibtex-autokey-before-presentation-function autokey)
;;       autokey)))
;; (insert (ref-man--bibtex-generate-autokey "li" "15" "diversity")) ; li15:_diversity

;; (ref-man-chrome-init)
;; (ref-man-chrome-shutdown)
;; (ref-man-chrome--check-port 9222)
;; (ref-man-chrome--get-socket-url-for-tab 0)
;; (setq ref-man-chrome--tabs nil)
;; (ref-man-chrome--get-socket-url-for-tab 1)
;; (mapcar (lambda (x) (plist-get x :id)) (ref-man-chrome-get-tabs))
;; (plist-get ref-man-chrome--sockets 0)
;; ;; TODO: Sometimes it just doesn't connect especially on the first try
;; (ref-man-chrome-connect 0)
;; ;; FIXME I'm 'cons-ing it. They'll be prepended not appended
;; (nth 0 ref-man-chrome--sockets)

;; NOTE: `ref-man-chrome-init' does everything. though there are too many messagess
(ref-man-chrome-init)

;; NOTE: Following two commands sets the URL and fetches the html. Rest of the
;;       stuff has to be done with eww like hooks
(ref-man-chrome-eval "location.href = \"https://scholar.google.com/scholar?q=image+captioning\"" 0
                     (lambda (result) (insert (plist-get (plist-get result :result) :value))))

(ref-man-chrome-eval 
 "document.documentElement.innerHTML" 0
 (lambda (result)
   (progn
     (with-current-buffer (get-buffer-create "*test*")
       (when buffer-read-only
         (setq-local buffer-read-only nil))
       (erase-buffer)
       (insert (plist-get (plist-get result :result) :value))
       (when (get-buffer "*html*")
         (with-current-buffer (get-buffer "*html*")
           (setq-local buffer-read-only nil)))
       (shr-render-buffer (get-buffer "*test*")))
     (with-current-buffer (get-buffer "*html*")
       (setq-local buffer-read-only t)
       (eww-mode))(pop-to-buffer (get-buffer "*html*")))))

;; NOTE: The thing with eww is, it keeps track of history w.r.t URLs it visits
;;       and that is integral to how it navigates the buffers. I don't have the
;;       concept of a URL there, rather it's just forward and backward states,
;;       though I could store the URLs as search terms. In the sense, some
;;       location is set there.

;; (defun ref-man-chrome--)

;; TODO: History hooks? How does eww do it?
(defun ref-man-chrome--fetch-html (tab-id)
  (ref-man-chrome-eval 
   "document.documentElement.innerHTML" tab-id
   (lambda (result)
     (progn
       (with-current-buffer (get-buffer-create "*test*")
         (when buffer-read-only
           (setq-local buffer-read-only nil))
         (erase-buffer)
         (insert (plist-get (plist-get result :result) :value))
         (when (get-buffer "*html*")
           (with-current-buffer (get-buffer "*html*")
             (setq-local buffer-read-only nil)))
         (shr-render-buffer (get-buffer "*test*")))
       (with-current-buffer (get-buffer "*html*")
         (setq-local buffer-read-only t)
         (eww-mode))
       (pop-to-buffer (get-buffer "*html*"))))))

;; CHECK: Another way could be to click the link on the chromium page
(defun ref-man-chrome--set-location-fetch-html (url tab-id)
  (add-to-list 'ref-man-chrome--history-list url)
  (ref-man-chrome-eval (format "location.href = \"%s\"" url) tab-id
                       (lambda (result)
                         (message (plist-get (plist-get result :result) :value))
                         (ref-man-chrome--fetch-html tab-id))))

(defun ref-man-chrome-follow-link-at-point ()
  "Follow the corresponding link in the chromium process or *eww*
buffer according to the type of link under point"
  (let ((url (get-text-property (point) 'shr-url)))
    (if (string-match-p "^http" (get-text-property (point) (quote shr-url)))
        (shr-browse-url url)
      ;; FIXME: tab-id has to be stored somewhere according to page
      (ref-man-chrome--set-location-fetch-html url tab-id))))


;; ;; NOTE: I would like two tabs, one for chrome and another for any bibtex entry
;; ;;       I would like to extract. The import to bibtex will open in another
;; ;;       buffer and I'll just extract the text from that
;; (defun ref-man-chrome-search-gscholar (search-string)
;;   ;; 1. Get chrome tabs
;;   ;; 2. Select the scholar tab (if there are two tabs, usually one would
;;   ;;    be gscholar, other bibtex if present)
;;   ;; 3. Do usual eww-hooks (have to figure them out):
;;   ;;    - Like store in history (for forward backward)
;;   ;;    - Handle keyboard and mouse events
;;   ;;      * filter by date
;;   ;;      * follow link (in new tab by default)
;;   ;;      * etc.
;;   ;;    - keypresses i,d,v,c etc. needn't be sent back to chrome as they don't change the page
;;   ;;    - TODO: Define clearly what should be sent back to chrome and what shouldn't
;;   ;;      * Make interface for the same
;;   ;;    - other stuff
;;   ;; 4. Change the URL to gscholar-url + search-string
;;   ;; 5. Fetch the html back on page-load
;;   )

;; (defun ref-man-chrome-close-tab (which-tab) )
;; (defun ref-man-chrome-forward ())
;; (defun ref-man-chrome-backward ())
;; (defun ref-man-chrome-gscholar-next ())
;; (defun ref-man-chrome-gscholar-previous ())
;; (defun ref-man-chrome-keypress-b ())
;; (defun ref-man-chrome-keypress-c ())    ; cycle between pdf links? I don't remember that
;; (defun ref-man-chrome-keypress-d ())
;; (defun ref-man-chrome-keypress-i ())
;; (defun ref-man-chrome-keypress-v ())

;; ;; This might be a bit complicated as the buffer will be rendered as eww-buffer
;; ;; anyway. Only the html parts of the page will be rendered and the rest will be
;; ;; left out. As such it becomes mostly a static page. Only the interactions
;; ;; which change the links on the page need to be mapped back to eww
;; ;;
;; ;; For now I'll just map the ones which pertain to my custom google scholar commands
;; (defvar ref-man-chrome-mode-map         ; eww mode map compatibility
;;   (let ((map (make-sparse-keymap)))
;;     (define-key map "g" 'eww-reload) ;FIXME: revert-buffer-function instead!
;;     (define-key map "G" 'eww)
;;     (define-key map [?\M-\r] 'eww-open-in-new-buffer)
;;     (define-key map [?\t] 'shr-next-link)
;;     (define-key map [?\M-\t] 'shr-previous-link)
;;     (define-key map [backtab] 'shr-previous-link)
;;     (define-key map [delete] 'scroll-down-command)
;;     (define-key map "l" 'eww-back-url)
;;     (define-key map "r" 'eww-forward-url)
;;     (define-key map "n" 'eww-next-url)
;;     (define-key map "p" 'eww-previous-url)
;;     (define-key map "u" 'eww-up-url)
;;     (define-key map "t" 'eww-top-url)
;;     (define-key map "&" 'eww-browse-with-external-browser)
;;     (define-key map "d" 'eww-download)
;;     (define-key map "w" 'eww-copy-page-url)
;;     (define-key map "C" 'url-cookie-list)
;;     (define-key map "v" 'eww-view-source)
;;     (define-key map "R" 'eww-readable)
;;     (define-key map "H" 'eww-list-histories)
;;     (define-key map "E" 'eww-set-character-encoding)
;;     (define-key map "s" 'eww-switch-to-buffer)
;;     (define-key map "S" 'eww-list-buffers)
;;     (define-key map "F" 'eww-toggle-fonts)
;;     (define-key map "D" 'eww-toggle-paragraph-direction)
;;     (define-key map [(meta C)] 'eww-toggle-colors)

;;     (define-key map "b" 'eww-add-bookmark)
;;     (define-key map "B" 'eww-list-bookmarks)
;;     (define-key map [(meta n)] 'eww-next-bookmark)
;;     (define-key map [(meta p)] 'eww-previous-bookmark)

;;     (easy-menu-define nil map ""
;;       '("Eww"
;; 	["Exit" quit-window t]
;; 	["Close browser" quit-window t]
;; 	["Reload" eww-reload t]
;; 	["Follow URL in new buffer" eww-open-in-new-buffer]
;; 	["Back to previous page" eww-back-url
;; 	 :active (not (zerop (length eww-history)))]
;; 	["Forward to next page" eww-forward-url
;; 	 :active (not (zerop eww-history-position))]
;; 	["Browse with external browser" eww-browse-with-external-browser t]
;; 	["Download" eww-download t]
;; 	["View page source" eww-view-source]
;; 	["Copy page URL" eww-copy-page-url t]
;; 	["List histories" eww-list-histories t]
;; 	["Switch to buffer" eww-switch-to-buffer t]
;; 	["List buffers" eww-list-buffers t]
;; 	["Add bookmark" eww-add-bookmark t]
;; 	["List bookmarks" eww-list-bookmarks t]
;; 	["List cookies" url-cookie-list t]
;; 	["Toggle fonts" eww-toggle-fonts t]
;; 	["Toggle colors" eww-toggle-colors t]
;;         ["Character Encoding" eww-set-character-encoding]
;;         ["Toggle Paragraph Direction" eww-toggle-paragraph-direction]))
;;     map))


(provide 'ref-man-chrome)
