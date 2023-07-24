;;; ref-man-py.el --- Module for managing the python process. ;;; -*- lexical-binding: t; -*-

;; Copyright (C) 2018,2019,2020,2021,2022,2023
;; Akshay Badola

;; Author:	Akshay Badola <akshay.badola.cs@gmail.com>
;; Maintainer:	Akshay Badola <akshay.badola.cs@gmail.com>
;; Time-stamp:	<Monday 24 July 2023 07:39:58 AM IST>
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
;; Functions for managing python virtualenv and processes for `ref-man'.  Has
;; functions to check if a virtualenv exists for a given path, if correct python
;; version is installed in the system and starting and stopping python
;; processes.

;;; Code:

(require 'python-venv)

(defcustom ref-man-py-executable nil
  "Path to python executable to use for `ref-man'.

Note that this corresponds to the python executable used to setup
the virtual environment and should be a valid python executable
with version > 3.7.0."
  :type 'file
  :group 'ref-man)

(defcustom ref-man-py-server-port-start 9999
  "Server port on which to communicate with python server."
  :type 'integer
  :group 'ref-man)

(defcustom ref-man-py-server-port 9999
  "Port on which to communicate with python server."
  :type 'integer
  :group 'ref-man)

(defcustom ref-man-py-data-dir (expand-file-name "~/.ref-man/data/")
  "Server port on which to communicate with python server."
  :type 'directory
  :group 'ref-man)

(defcustom ref-man-proxy-port nil
  "Whether to use http proxy for all python requests.
If this is non-nil then the all the requests by the python server
go through this http proxy at localhost, specified by this port."
  :type 'integer
  :group 'ref-man)

(defcustom ref-man-chrome-debug-script (path-join (f-dirname (f-this-file)) "debug_ss.js")
  "Path to the chrome debugger javascript file.
The file contains code to get the Semantic Scholar Search
params.  As they can change, we need to update them once the
server starts.  Requires some kind of chrome(ium) to be installed
on the system."
  :type 'file
  :group 'ref-man)

(defcustom ref-man-py-source-dir nil
  "Optional source directory of the python module."
  :type 'directory
  :group 'ref-man)

(defcustom ref-man-py-env-dir ".ref-man/env"
  "Directory of virtualenv of the python module."
  :type 'directory
  :group 'ref-man)

(defcustom ref-man-py-run-from-source nil
  "Should we run the python module directly from source?"
  :type 'boolean
  :group 'ref-man)

(defcustom ref-man-py-use-system-python nil
  "Should we not create venv and use system python?"
  :type 'boolean
  :group 'ref-man)

(defcustom ref-man-py-refs-cache-dir ".ref-man/s2_cache"
  "Directory of extracted citation cache from S2 data."
  :type 'directory
  :group 'ref-man)

(defcustom ref-man-py-log-dir ".ref-man/logs"
  "Directory for storing logs."
  :type 'directory
  :group 'ref-man)

;; NOTE: External variables
(defvar ref-man-pdf-proxy-port)         ; from `ref-man-url'
(defvar ref-man-documents-dir)          ; from `ref-man-files'
(defvar ref-man-remote-documents-dir)   ; from `ref-man-remote'
(defvar ref-man-public-links-cache-file) ; from `ref-man-remote'

;; NOTE: Internal variables
(defvar ref-man-py-external-process-pid nil)

(defsubst ref-man-py-source-dir-valid-p ()
  "Check if `ref-man-py-source-dir' exists."
  (and ref-man-py-source-dir (stringp ref-man-py-source-dir)
       (not (string-empty-p ref-man-py-source-dir))
       (f-exists? ref-man-py-source-dir)))

(defsubst ref-man-py-url (&optional path opts)
  "Return `ref-man-py' url for python process.

Optional PATH is the path after url to fetch.
Optional OPTS is an alist of additional HTTP args to send."
  (declare (pure t) (side-effect-free t))
  (format "http://localhost:%s/%s%s"
          ref-man-py-server-port
          (or path "")
          (or (and opts (concat "?"
                                (mapconcat
                                 (lambda (x) (format "%s=%s" (car x) (cdr x)))
                                 opts "&")))
              "")))

(defsubst ref-man-py-python ()
  "Get the appropriate python version.

If `ref-man-py-use-system-python' that's just system python, else
it's given by python in `ref-man-py-env-dir'."
  (if ref-man-py-use-system-python
      (python-venv-get-system-python "/usr/bin/python3")
    (and ref-man-py-env-dir (path-join ref-man-py-env-dir "bin" "python"))))

(defun python-venv-py-version ()
  "Return version of python executable for appropriate python exectuable.

The executable is given by `ref-man-py-python'."
  (python-venv-get-python-version (ref-man-py-python)))

(defun ref-man-py-installed-mod-version ()
  "Return the version of `ref-man-py' python installed module."
  (let* ((python (ref-man-py-python))
         (output (shell-command-to-string (format "%s -m ref_man_py --version" python))))
    (-last-item (split-string output))))

(defun ref-man-py-file-mod-version ()
  "Return the version of `ref-man-py' python module in source dir.

Source dir is given by `ref-man-py-source-dir'."
  (when (ref-man-py-source-dir-valid-p)
    (let* ((init-file (path-join ref-man-py-source-dir "ref_man_py" "__init__.py"))
           (buf (find-file-noselect init-file)))
      (with-current-buffer buf
        (goto-char (point-min))
        (re-search-forward "__version__\\(.+?\\)=\\(.+?\\)\"\\(.+\\)\"")
        (substring-no-properties (match-string 3))))))

(defun ref-man-py-need-update-p (python)
  "Check if `ref-man-py' python module needs update.

PYTHON is the virtualenv environment or python executable."
  (let ((installed-version (python-venv-installed-mod-version-string python "ref-man-py"))
        (pypi-version (python-venv-get-pypi-version "ref-man-py")))
    (if (and installed-version pypi-version)
        (version< installed-version pypi-version)
      t)))

(defun ref-man-py-install-ref-man (&optional update)
  "Install the `ref-man-py' python module from PyPI.

If `ref-man-py-use-system-python' is non-nil then a python
virtualenv is created, otherwise the system python3 is used.  The
python virtualenv directory is given by `ref-man-py-env-dir'.

Optional non-nil EDITABLE does an editable install.

Optional non-nil REINSTALL reinstalls the entire virtualenv.

Optional non-nil UPDATE updates the `ref-man-py' python module.

This is for installing from PyPI only.  If
`ref-man-py-run-from-source' is given and a valid
`ref-man-py-source-dir' exists, then it's simply run from there
and nothing is installed.  Running from source assumes that all
the requirements are already installed in `ref-man-py-env-dir' or
system."
  (let* ((env ref-man-py-env-dir)
         (python (if ref-man-py-use-system-python
                     (python-venv-get-system-python)
                   (python-venv-or-env-python env)))
         (env-dir (if ref-man-py-use-system-python "user local dir" env))
         (installed-version (python-venv-installed-mod-version-string python "ref-man-py"))
         (env-has-no-ref-man (string-empty-p installed-version))
         (need-update (and (not ref-man-py-run-from-source)
                           (or update (ref-man-py-need-update-p python)))))
    (when ref-man-py-run-from-source
      (user-error "Can't run from source and install the module"))
    (cond (need-update
           (message "New version of ref-man. Updating existing ref-man-server in %s" env-dir)
           (python-venv-env-install-module-from-pypi python "ref-man-py" t))
          (env-has-no-ref-man
           (message "ref-man-server not found. Installing in %s" env-dir)
           (python-venv-env-install-module-from-pypi python "ref-man-py"))
          (t (if (y-or-n-p (format "Some error occured. Clean and reinstall for venv %s" env-dir))
                 (if ref-man-py-use-system-python
                     (progn (python-venv-env-uninstall-module python "ref-man-py")
                            (python-venv-env-install-module-from-pypi python "ref-man-py" t))
                   (python-venv-env-install-module-from-pypi python "ref-man-py" t)))))
    (let ((version (python-venv-installed-mod-version-string python "ref-man-py")))
      (if version
          (message "%s found in %s" version env-dir)
        (error "Could not install ref-man in %s" env-dir)))))

;; TODO: Requests to python server should be dynamic according to whether I want
;;       to use proxy or not at that point
;; CHECK: What's docs dir for and why's it not used?
(defun ref-man-py-process-helper (data-dir port &optional proxy-port)
  "Start the python server.
DATA-DIR is the server data directory.  PORT is the port to which
the server binds.

When called from `ref-man-py-start-server', DATA-DIR is set
to `ref-man-py-data-dir' and the port
`ref-man-py-server-port'.

Optional arguments PROXY-PORT, PROXY-EVERYTHING-PORT, DOCS-DIR
are specified for modularity and are set to `ref-man-proxy-port',
nil, `ref-man-py-data-dir' respectively by
`ref-man-py-start-server'."
  ;; NOTE: Hack so that process isn't returned
  (prog1
      (message (format "[ref-man] Starting python process on port: %s"
                       ref-man-py-server-port))
    (let ((python (ref-man-py-python))
          (proxy-port (or proxy-port ref-man-proxy-port))
          (default-directory (when (and ref-man-py-run-from-source
                                        (ref-man-py-source-dir-valid-p))
                               ref-man-py-source-dir)))
      (let ((args (-filter #'identity (list (format "--data-dir=%s" data-dir)
                                            (format "--port=%s" port)
                                            (and proxy-port "--proxy-everything")
                                            (and proxy-port
                                                 (format "--proxy-everything-port=%s" proxy-port))
                                            (and ref-man-pdf-proxy-port
                                                 (format "--proxy-port=%s"
                                                         ref-man-pdf-proxy-port))
                                            (and ref-man-chrome-debug-script
                                                 (format "--chrome-debugger-path=%s"
                                                         ref-man-chrome-debug-script))
                                            (and ref-man-documents-dir
                                                 (format "--local-pdfs-dir=%s"
                                                         ref-man-documents-dir))
                                            (and ref-man-remote-documents-dir
                                                 (format "--remote-pdfs-dir=%s"
                                                         ref-man-remote-documents-dir))
                                            (and ref-man-public-links-cache-file
                                                 (format "--remote-links-cache=%s"
                                                         ref-man-public-links-cache-file))
                                            (and ref-man-py-refs-cache-dir
                                                 (format "--refs-cache-dir=%s"
                                                         ref-man-py-refs-cache-dir))
                                            "--verbosity=debug"))))
        (unless (version-list-< (version-to-list (ref-man-py-installed-mod-version)) '(0 7 0))
          (push "--logfile=ref-man.log" args)
          (push (format "--logdir=%s" ref-man-py-log-dir) args)
          (push "--logfile-verbosity=debug" args))
        (message "Python process args are %s" args)
        (apply #'start-process "ref-man-server" "*ref-man-server*"
               python "-m" "ref_man_py" args)))))

(defun ref-man-py-stop-server ()
  "Stop the python server by sending a shutdown command.
This is sent via http and lets the server exit gracefully."
  (interactive)
  (let ((buf (url-retrieve-synchronously (ref-man-py-url "shutdown"))))
    (message (util/url-buffer-string buf))))

(defun ref-man-py-kill-internal-process ()
  "Kill the internal python process process by sending SIGKILL."
  (signal-process (get-buffer "*ref-man-server*") 15))

(defun ref-man-py-kill-external-process ()
  "Kill the external python process process by sending SIGKILL."
  (signal-process ref-man-py-external-process-pid 15))

(defun ref-man-py-server-reachable-p ()
  "Check if python server is reachable."
  (condition-case nil
      (let ((buf (url-retrieve-synchronously (ref-man-py-url "version") t)))
        (when buf
          (string-match-p "ref-man python server"
                          (with-current-buffer buf (buffer-string)))))
    (error nil)))

(defun ref-man-py-process-running ()
  "Check if python server is running.
Returns \\='external or \\='internal according to where the process is
running if it's running else nil."
  (cond ((get-buffer-process "*ref-man-server*")
         (setq ref-man-py-external-process-pid nil)
         'internal)
        ((ref-man-py-external-process-p)
         'external)
        (t nil)))

(defun ref-man-py-server-running ()
  "Check if python server is already running."
  (let ((python-process (ref-man-py-process-running)))
    (when python-process
      (if (ref-man-py-server-reachable-p)
          python-process
        (if (eq python-process 'internal)
            'internal-error 'external-error)))))

(defun ref-man-py-external-process-p ()
  "Check for `server.py' python processes outside Emacs.
In case a process is found, `ref-man-py-server-port' is set
to the port of that process and
`ref-man-py-external-process-pid' is set to its pid."
  (let ((python-strings
         (split-string (shell-command-to-string "ps -ef | grep python | grep server") "\n")))
    (cl-loop for x in python-strings
             do
             (when (and (string-match-p "port" x) (string-match-p "data-dir" x))
               (setq ref-man-py-server-port
                     (string-to-number
                      (cadr (split-string
                             (car (split-string
                                   (substring x (string-match "port" x))))
                             "="))))
               (setq ref-man-py-external-process-pid (string-to-number (nth 1 (split-string x))))
               (cl-return t)))))

(defun ref-man-py-start-server ()
  "Start the python server, unless already running.

The server can be running outside Emacs also in which case
`ref-man-py-server-port' is set to port.

See accompanying `server.py' for the server details.  The API and
methods are still evolving but as of now it supports DBLP and
ArXiv.  The process if started opens a local port and can fetch
data in multiple threads from supported APIs before preprocessing
and consolidating.  It also maintains a local datastore."
  (interactive)
  ;; FIXME: for 'internal-error and 'external-error
  (if (ref-man-py-server-running)
      (message (format "Found existing process running on port: %s"
                       ref-man-py-server-port))
    (message "No existing python process found")
    (let ((port (find-open-port ref-man-py-server-port-start))
          (data-dir ref-man-py-data-dir))
      (setq ref-man-py-server-port port)
      (ref-man-py-process-helper data-dir port))))

(defun ref-man-py-restart-server ()
  "Restart the python server."
  (interactive)
  (cond ((or (eq 'internal (ref-man-py-server-running))
             (eq 'external (ref-man-py-server-running)))
         (ref-man-py-stop-server))
        ((eq 'internal-error (ref-man-py-server-running))
         (ref-man-py-kill-internal-process))
        ((eq 'external-error (ref-man-py-server-running))
         (ref-man-py-kill-external-process)))
  ;; FIXME: This runs before server shuts down
  (ref-man-py-start-server))

(provide 'ref-man-py)

;;; ref-man-py.el ends here
