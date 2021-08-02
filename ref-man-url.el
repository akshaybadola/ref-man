;;; ref-man-url.el --- url utilities and functions for `ref-man'. ;;; -*- lexical-binding: t; -*-

;; Copyright (C) 2018,2019,2020,2021
;; Akshay Badola

;; Author:	Akshay Badola <akshay.badola.cs@gmail.com>
;; Maintainer:	Akshay Badola <akshay.badola.cs@gmail.com>
;; Time-stamp:	<Monday 02 August 2021 15:13:05 PM IST>
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
;; Url functions, checks for various properties, conversion to and from proxy,
;; etc.

;;; Code:

(unless (featurep 'ref-man-util)
  (require 'ref-man-util))

(defcustom ref-man-pdf-proxy-port nil
  "Fetch PDFs over a proxy server if non-nil.
If this is non-nil then the all the pdf downloads go through the
python server which additionally fetches it from an http proxy at
localhost specified by this port."
  :type 'boolean
  :group 'ref-man)

;; NOTE: External variables
(defvar ref-man-python-server-port)     ; from `ref-man-py'
(defvar ref-man-documents-dir)          ; from `ref-man-files'

(defvar ref-man-url-supported-sites
  '(acl arxiv neurips mlr aaai acm doi-cvpr cvf cvf-old openreview ss)
  "List of supported sites for fetching pdf.")

;; CHECK: What does this do?
(defun ref-man-url-to-arxiv-id (url)
  "Get arxivid from URL."
  (let ((suffix (car (last (split-string url "/")))))
    (if (string-match-p "pdf" suffix)
        (replace-in-string suffix ".pdf" "")
      suffix)))

(defun ref-man-url-from-arxiv-id ()
  "Get url from arxivid extracted from org property drawer at point."
  (interactive)
  (if (eq major-mode 'org-mode)
      (let ((arxiv-id (pcase (or (org-entry-get (point) "ARXIVID")
                                 (org-entry-get (point) "EPRINT"))
                        ((and val) val))
                      ;; NOTE: Replaced with pcase
                      ;; (cond ((org-entry-get (point) "ARXIVID")
                      ;;        (org-entry-get (point) "ARXIVID"))
                      ;;       ((org-entry-get (point) "EPRINT")
                      ;;        (org-entry-get (point) "EPRINT"))
                      ;;       (t nil))
                      ))
        (when arxiv-id
          (if (called-interactively-p 'any)
              (progn
                (when current-prefix-arg
                  (org-entry-put (point) "URL" (format "https://arxiv.org/abs/%s" arxiv-id)))
                (kill-new (format "https://arxiv.org/abs/%s" arxiv-id)))
            (format "https://arxiv.org/abs/%s" arxiv-id))))
    (message "[ref-man] Not in org-mode") nil))

(defun ref-man-url-non-gscholar-url-p (url)
  "Check if URL is not a Google Scholar url."
  (or (string-match-p "semanticscholar.org" url)
      (and (not (string-match-p "javascript" url))
           (not (string-match-p "scholar" url))
           (not (string-match-p "google" url))
           (not (string-prefix-p "/" url)))))

(defun ref-man-url-filter-non-gscholar (url-list)
  "Remove all non Google Scholar urls from URL-LIST."
  (-remove (lambda (x) (not (ref-man-url-non-gscholar-url-p x)))
             url-list))


(defun ref-man-url-matches-filename-p (url filename)
  "Check if the FILENAME is contained in URL."
  (let* ((obj (url-generic-parse-url url))
         (path (car (url-path-and-query obj))))
         (string-match-p filename path)))

(defun ref-man-url-maybe-proxy (url)
  "Return proxy URL if `ref-man-pdf-proxy-port' is non-nil, else same URL."
  (let ((prefix (format "http://localhost:%s/fetch_proxy?url="
                        ref-man-python-server-port)))
    (if (and ref-man-pdf-proxy-port (not (string-prefix-p "http://localhost" url)))
        (concat prefix url)
      url)))

;; CHECK: Do I require something similar in case some other URL is proxied?
(defun ref-man-url-maybe-unproxy (url)
  "Remove proxy prefix and return URL if `ref-man-pdf-proxy-port' is non-nil, else same URL."
  (let ((prefix (format "http://localhost:%s/fetch_proxy?url="
                        ref-man-python-server-port)))
    (if (and ref-man-pdf-proxy-port (string-prefix-p prefix url))
        (string-remove-prefix prefix url)
      url)))

(defsubst ref-man-url-relative-p (url)
  "Check if URL is a relative url."
  (or (string-prefix-p "/" url) (string-prefix-p "./" url)))

(defun ref-man-url-parseable-link (url)
  "Check if the URL is suitable for information extraction."
  (cond ((string-match-p "arxiv.org" url)
         ; Either arxiv has bibtex link or get from API
         )
        ((string-match-p "aclweb.org" url))
        ((string-match-p "papers.nips.cc" url))
        ((string-match-p "mlr.press" url))
        ((string-match-p "openaccess.thecvf.com" url))
        ((string-match-p "cv-foundation.org" url))
        ((string-match-p "aaai.org" url))
        ((string-match-p "dl.acm.org" url))
        ((string-match-p "openreview.net" url))))

(defun ref-man-url-has-bib-url-p (url)
  "Does the given URL contain a downloadable or parseable bibtex entry."
  (and (ref-man-url-non-gscholar-url-p url)
       (or (string-match-p "arxiv.org" url)
           (string-match-p "aclweb.org" url)
           (string-match-p "papers.nips.cc" url)
           (string-match-p "mlr.press" url)
           (string-match-p "openaccess.thecvf.com" url)
           (string-match-p "cv-foundation.org" url)
           (string-match-p "aaai.org" url)
           (string-match-p "dl.acm.org" url)
           (string-match-p "openreview.net" url))))

(defun ref-man-url-may-have-bib-p (url)
  "Might the given URL contain a downloadable or parseable bibtex entry."
  (and (ref-man-url-non-gscholar-url-p url)
       (or (string-match-p "arxiv.org" url)
           (string-match-p "aclweb.org" url)
           (string-match-p "papers.nips.cc" url)
           (string-match-p "mlr.press" url)
           (string-match-p "openaccess.thecvf.com" url)
           (string-match-p "cv-foundation.org" url)
           (string-match-p "aaai.org" url)
           (string-match-p "dl.acm.org" url)
           (string-match-p "openreview.net" url))))

(defun ref-man-url-downloadable-pdf-url-p (url)
  "Does the given URL contain a pdf to download."
  (and (ref-man-url-non-gscholar-url-p url)
       (cond ((string-match-p "arxiv.org" url)
              (string-match-p "/pdf/" url))
             ((string-match-p "aaai.org" url)
              (or (string-match-p "/download/" url)
                  (string-match-p "ojs.aaai.org.+article/view/[0-9]+/[0-9]+" url)))
             ((string-match-p "openreview.net" url)
              (string-match-p "pdf" url))
             ((string-match-p "dl.acm.org" url)
              (or (string-match-p "doi/pdf" url) (string-match-p "gateway.cfm" url)))
             (t (string-match-p "\\.pdf$" url)))))

(defun ref-man-url-get-bibtex-link-from-nips-url  (url))
(defun ref-man-url-get-bibtex-link-from-cvf-url  (url))
(defun ref-man-url-get-bibtex-link-from-aaai-url  (url))
(defun ref-man-url-get-bibtex-link-from-acm-url  (url))
(defun ref-man-url-get-bibtex-link-from-openreview-url  (url))
(defun ref-man-url-get-supplementary-url-from-doi  (url))
(defun ref-man-url-get-supplementary-url-from-arxiv  (url))
(defun ref-man-url-get-supplementary-url-from-acl  (url))
(defun ref-man-url-get-supplementary-url-from-nips-url  (url))
(defun ref-man-url-get-supplementary-url-from-cvf-url  (url))
(defun ref-man-url-get-supplementary-url-from-aaai-url  (url))
(defun ref-man-url-get-supplementary-url-from-acm-url  (url))
(defun ref-man-url-get-supplementary-url-from-openreview-url (url))

(defmacro with-temp-shr-buffer (buf &rest body)
  "Construct a temp `shr' buffer from url buffer BUF and execute BODY."
  (or (fboundp 'libxml-parse-html-region)
      (error "This function requires Emacs to be compiled with libxml2"))
  `(let ((temp-buf (get-buffer-create " *temp-shr-buffer*")))
     (with-current-buffer temp-buf
       (erase-buffer)
       (shr-insert-document
        (with-current-buffer ,buf
          (goto-char (point-min))
          (forward-paragraph)
          (libxml-parse-html-region (point) (point-max))))
       (goto-char (point-min))
       ,@body)))

(defmacro with-named-shr-buffer (buf name &rest body)
  "Similar to `with-temp-shr-buffer' but created a named buffer with NAME.
Useful when you want the buffer to persist afterwards."
  (or (fboundp 'libxml-parse-html-region)
    (error "This function requires Emacs to be compiled with libxml2"))
  `(let ((temp-buf (get-buffer-create ,name)))
     (with-current-buffer temp-buf
       (erase-buffer)
       (shr-insert-document
        (with-current-buffer ,buf
          (goto-char (point-min))
          (forward-paragraph)
          (libxml-parse-html-region (point-min) (point-max))))
       (goto-char (point-min))
       ,@body)))

(defun ref-man-url-get-first-pdf-link-from-html-buffer (buf &optional predicate)
  "Extract first pdf link from an html response buffer BUF.
The buffer is first rendered with `shr' and then searched for
links.  With optional PREDICATE, return first link from the
output of `-filter' with predicate."
  (with-temp-shr-buffer buf
                        (let ((predicate (or predicate #'identity)))
                          (-first predicate (ref-man-web-get-all-links
                                             (current-buffer) t nil "pdf")))))

(defun ref-man-url-get-last-pdf-link-from-html-buffer (buf)
  "Extract last pdf link from an html response buffer BUF.
The buffer is first rendered with `shr' and then searched for
links."
  (with-temp-shr-buffer buf
                        (-last-item (ref-man-web-get-all-links
                                     (current-buffer) nil nil "pdf"))))

(defun ref-man-url-get-first-link-from-html-buffer (buf)
  "Extract first link from an html response buffer BUF.
The buffer is first rendered with `shr' and then searched for
links."
  (with-temp-shr-buffer buf
                        (car (ref-man-web-get-all-links (current-buffer)))))

(defun ref-man-url-get-last-link-from-html-buffer (buf)
  "Extract last link from an html response buffer BUF.
The buffer is first rendered with `shr' and then searched for
links."
  (with-temp-shr-buffer buf
                        (-last-item (ref-man-web-get-all-links
                                     (current-buffer) nil nil))))

(defun ref-man-url-domain (url)
  "Get the domain name with protocol for URL."
  (string-join
   (-take (1+ (-find-index (lambda (x) (string-match-p "\\." x))
                           (split-string url "/")))
          (split-string url "/")) "/"))

(defun ref-man-url-get-absolute-path (url link)
  "Get absolute path for a LINK and parent URL."
  (let ((dom (ref-man-url-domain url))
        (lsplits (split-string link "/"))
        (usplits (split-string url "/")))
    (cond ((string-match-p "^http:\\|^https:" link)
           link)
          ((or (equal (car lsplits) ".")
               (string-match-p "[a-zA-Z0-9]+" (car lsplits)))
           (url-join url link))
          ((string-prefix-p "/" link)
           (url-join dom link))
          (t (let ((count (-count (lambda (x) (equal x "..")) lsplits)))
               (when (string-suffix-p "/" url)
                 (setq count (+ 1 count)))
               (string-join (-concat (-take (- (length usplits) count) usplits)
                                     (-drop count lsplits)) "/"))))))

;; NOTE: This is equivalent to run-hook-with-args-until-success
;; TODO: This should be a pcase for brevity
;; The list of `site's should be separate
(defun ref-man-url-get-pdf-link-helper (site url buf &optional status)
  "Helper function to get pdf link from URL given type of SITE.
BUF is the html buffer retrieved from URL.  Optional status is a
plist which contains the http status.

SITE is a symbol according to which the helper function is
returned. Possible values for SITE are:

'acl            denotes an `aclweb.org' type website
'arxiv          `arxiv.org'
'doi            a DOI redirect
'neurips        either a `nips.cc' or `neurips.cc' type site
'mlr            `mlr.org' proceedings
'aaai           `aaai.org'
'acm            `acm.org'
'cvf            `cv-foundation.org'
'cvf-old        Older version of cvf website
'openreview     `openreview.org'
'ss             `semanticscholar.org'

In case none of them matches, then the default is to retrieve the
first pdf link from the buffer."
  (cond ((eq site 'acl)
         (ref-man-url-acl-pdf-link-helper url))
        ((eq site 'arxiv)
         (ref-man-url-arxiv-pdf-link-helper url))
        ((eq site 'doi)
         (let* ((url (plist-get status :redirect))
                (site (ref-man-url-get-site-from-url url)))
           ;; NOTE: Call self again with new URL
           (ref-man-url-get-pdf-link-helper site url buf)))
        ((eq site 'neurips)
         (let ((link (ref-man-url-get-first-pdf-link-from-html-buffer buf)))
           (when link
             (cond ((string-match-p "^http:\\|^https:" link) link)
                   ((string-match-p "^/paper/" link)
                    (url-join (string-join (-take 3 (split-string url "/")) "/") link))
                   (t nil)))))
        ((eq site 'mlr)
         (let ((link (ref-man-url-get-first-pdf-link-from-html-buffer buf)))
           (when (and link (string-match-p "^http:\\|^https:" link)) link)))
        ((eq site 'aaai)
         (let* ((buf (if (string-match-p "This page requires frames."
                                         (with-current-buffer buf (buffer-string)))
                         (url-retrieve-synchronously (ref-man-url-get-last-link-from-html-buffer buf) t)
                       buf)))
           (replace-regexp-in-string "/paper/view/" "/paper/viewFile/"
                                     (ref-man-url-get-last-link-from-html-buffer buf))))
        ((eq site 'acm)
         (if (string-match-p "/doi/" url)
             (replace-regexp-in-string "/doi/" "/doi/pdf/" url)
           (let ((link (with-temp-shr-buffer
                        buf
                        (or (car (ref-man-web-get-all-links (current-buffer) nil nil "gateway"))
                            (car (ref-man-web-get-all-links (current-buffer) nil nil "doi/pdf"))))))
             (when link
               (if (string-prefix-p "https://dl.acm.org/" link)
                   link
                 (url-join "https://dl.acm.org/" link))))))
        ((eq site 'cvf)
         (let ((link (ref-man-url-get-first-pdf-link-from-html-buffer buf)))
           (when link
             (cond ((string-match-p "^http:\\|^https:" link) link)
                   ((string-match-p "\\(\\.\\./\\)+\\(content.*\\)" link)
                    (url-join (string-join (-take 3 (split-string url "/")) "/")
                              (replace-regexp-in-string "\\(\\.\\./\\)+\\(content.*\\)" "/\\2" link)))
                   ((string-match-p "^../../content_.*" link)
                    (url-join (string-join (-take 3 (split-string url "/")) "/")
                              (string-join (-drop 2 (split-string link "/")) "/")))
                   (t nil)))))
        ;; CHECK: Not sure if this is correct
        ((eq site 'cvf-old)
         (let ((link (ref-man-url-get-first-pdf-link-from-html-buffer buf)))
           (when link
             (cond ((string-match-p "^http:\\|^https:" link) link)
                   ((string-match-p "^../../content_.*" link)
                    (url-join (string-join (-take 4 (split-string url "/")) "/")
                              (string-join (-drop 2 (split-string link "/")) "/")))
                   (t nil)))))
        ((eq site 'openreview)
         (let ((link (ref-man-url-get-first-pdf-link-from-html-buffer buf)))
           (when link
             (if (string-match-p "^[http|https]" link)
                 link
               (url-join "https://openreview.net" link)))))
        ((eq site 'ss)
         (let* ((links (with-temp-shr-buffer buf
                                            (ref-man-web-get-all-links (current-buffer) t)))
                (site-url (ref-man-url-get-best-pdf-url links))
                (url (cond ((eq (car site-url) 'pdf)
                            (cdr site-url))
                           (site-url
                            (condition-case nil
                                (ref-man-url-get-pdf-link-helper (car site-url) (cdr site-url) nil)
                                (error (with-current-buffer
                                           (url-retrieve-synchronously (cdr site-url))
                                         (ref-man-url-get-pdf-link-helper
                                          (car site-url)
                                          (cdr site-url)
                                          (current-buffer)))))))))
           url))
        (t (let ((link (ref-man-url-get-first-pdf-link-from-html-buffer buf)))
             (when link
               (if (string-match-p "^http:\\|^https:" link)
                   link
                 (ref-man-url-get-absolute-path url link)))))))

;; (defun ref-man-url-get-pdf-link (site url &optional callback cbargs)
;;   "Helper function to get a pdf link from URL and website SITE.
;; SITE can be one of a few sites from which it is feasible to
;; downlod the pdf.  If optional CALLBACK is given then the URL is
;; fetched async and the callback is called with the pdf link and
;; args CBARGS.  Otherwise, the URL is fetched synchronously."
;;   (if callback
;;       (url-retrieve url (lambda (status url)
;;                           (if (plist-get status :error)
;;                               (message (format "[ref-man] Error occured while fetching %s" url))
;;                             (let* ((link (ref-man-url-get-pdf-link-helper site url (current-buffer))))
;;                               (funcall callback link cbargs)))))
;;     (let ((buf (url-retrieve-synchronously url t)))
;;       (ref-man-url-get-pdf-link-helper site url buf))))

(defun ref-man-url-arxiv-pdf-link-helper (url &rest args)
  "Get PDF URL for an arxiv url.
ARGS is for compatibility and not used."
  (concat (replace-regexp-in-string "/abs/" "/pdf/" url) ".pdf"))

(defun ref-man-url-acl-pdf-link-helper (url &rest args)
  "PDF link helper for an ACL URL.
ARGS is for compatibility and not used."
  (if (string-match-p "aclweb.org" url)
      (concat (replace-regexp-in-string "/$" "" url) ".pdf")
    (concat "https://www.aclweb.org/anthology/"
            (upcase (car (last (split-string url "/")))) ".pdf")))

(defun ref-man-url-get-site-from-url (url)
  "Helper function to determine site from URL."
  (cond ((string-match-p "arxiv.org" url) 'arxiv)
      ((string-match-p "aclanthology.info\\|aclweb.org" url) 'acl)
      ((and (string-match-p "doi.org" url)
            (string-match-p "cvpr" (-last-item (split-string url "/" t))))
       'doi-cvpr)
      ((string-match-p "papers.nips.cc\\|proceedings.neurips.cc" url) 'neurips)
      ((string-match-p "mlr.press" url) 'mlr)
      ((string-match-p "openaccess.thecvf.com" url) 'cvf)
      ((string-match-p "cv-foundation.org" url) 'old-cvf)
      ((string-match-p "aaai.org" url) 'aaai)
      ((string-match-p "acm.org" url) 'acm)
      ((string-match-p "openreview.net" url) 'openreview)
      ((string-match-p "semanticscholar.org/paper" url) 'ss)
      ((string-match-p "doi.org" url) 'doi)
      (t nil)))

(defun ref-man-url--sort-best-subroutine (x)
  (pcase (car x)
    ('acm (or (let ((link (car (-filter (lambda (y) (string-match-p "pdf" y)) (cdr x)))))
                (and link (cons 'pdf link)))
              (let ((link (car (-filter (lambda (y) (string-match-p "/doi/" y)) (cdr x)))))
                (and link (cons 'pdf (replace-regexp-in-string "/doi/" "/doi/pdf/" link))))))
    (_ (cadr x))))

(defun ref-man-url-get-best-pdf-url (urls)
  "Get the best url from a list of URLS according to ease of downloading.
Return a '(site url) pair."
  (let ((pdf-urls (-filter #'ref-man-url-downloadable-pdf-url-p urls)))
    (if pdf-urls
        (cons 'pdf (car pdf-urls))
      (let* ((links (-filter #'identity
                             (-map (lambda (x) (when (ref-man-url-get-site-from-url x)
                                                 (cons (ref-man-url-get-site-from-url x) x)))
                                   urls)))
             (links (-sort (lambda (x y)
                             (< (cl-position (car x) '(arxiv acl neurips mlr aaai acm
                                                             cvf cvf-old openreview ss doi))
                                (cl-position (car y) '(arxiv acl neurips mlr aaai acm cvf
                                                             cvf-old openreview ss doi))))
                           (ref-man-pairs-to-alist links)))
             (best (car links)))
        (cond ((and (symbolp (car best))
                    (listp (cdr best)))
               (cons (car best)
                     (cadr best)))
              ((and (symbolp (car best))
                    (stringp (cdr best)))
               best)
              (t (ref-man-url--sort-best-subroutine best)))))))

(defun ref-man-url-get-pdf-url-according-to-source (url &optional callback cbargs)
  "Fetch a PDF url according to te source URL.
When optional CALLBACK and CBARGS is non nil, CALLBACK is called
with PDF link and CBARGS.

The function can be used to:
    a. get a PDF URL from a site
    b. after obtaining the URL download the PDF file.

Since a URL may redirect or multiple URLs can point to same site,
which would have similar mechanisms for obtaining the URL, a SITE
is determined first with `ref-man-url-get-site-from-url'.

Post which a HELPER function is determined which can obtain the
PDF URL from that particular SITE (see `ref-man-url-get-pdf-link-helper').

After which if an optional CALLBACK (with optional CBARGS) is
given is it called on the PDF URL."
  (when url
    (let* ((site (ref-man-url-get-site-from-url url))
           (helper (pcase site
                     ('arxiv #'ref-man-url-arxiv-pdf-link-helper)
                     ('acl #'ref-man-url-acl-pdf-link-helper)
                     ('doi-cvpr #'ref-man-url-cvpr-pdf-link-helper)
                     (_ (-partial #'ref-man-url-get-pdf-link-helper site)))))
      (cond ((and helper (symbolp helper) callback)
             (let ((link (funcall helper url cbargs)))
               (funcall callback link cbargs)))
            ((and helper (symbolp helper))
             (funcall helper url cbargs))
            ((and helper callback)
             (url-retrieve url (lambda (status url helper callback cbargs)
                                 (if (plist-get status :error)
                                     (message (format "[ref-man] Error occured while fetching %s" url))
                                   (let* ((link (funcall helper url (current-buffer) status)))
                                     (funcall callback link cbargs))))
                           (list url helper callback cbargs)))
            (helper
             (let ((buf (url-retrieve-synchronously url t)))
               (funcall helper url buf)))
            (t nil)))))

;; NOTE: This function currently is only used by `ref-man-try-fetch-and-store-pdf-in-org-entry'
(defun ref-man-url-get-bib-according-to-source (url)
  "Return a bibtex entry according to URL."
  (when url
    ;; NOTE: This how it could be if a cons of (url . buf-or-name) is given
    ;; (when (and (listp buf-url) (ref-man--downloadable-bib-url-p (cdr url)))
    ;;   (ref-man--get-bib-from-url-buf url)
    ;;   )
    (cond ((string-match-p "doi.org" url)
           (ref-man-url-get-bibtex-link-from-doi url))
          ((string-match-p "arxiv.org" url)
           (ref-man-url-get-bibtex-link-from-arxiv url))
          ((string-match-p "aclweb.org" url)
           (concat (replace-regexp-in-string "/$" "" url) ".bib"))
          ((string-match-p "aclanthology.info" url)
           (cons 'url (concat "https://www.aclweb.org/anthology/"
                              (upcase (car (last (split-string url "/")))) ".bib")))
          ((string-match-p "papers.nips.cc\\|proceedings.neurips.cc" url)
           (ref-man-url-get-bibtex-link-from-nips-url url))
          ((string-match-p "openaccess.thecvf.com" url)
           (ref-man-url-get-bibtex-link-from-cvf-url url))
          ((string-match-p "aaai.org" url)
           (ref-man-url-get-bibtex-link-from-aaai-url url))
          ((string-match-p "dl.acm.org" url)
           (ref-man-url-get-bibtex-link-from-acm-url url))
          ((string-match-p "openreview.net" url)
           (ref-man-url-get-bibtex-link-from-openreview-url url))
          (t url))))

(defun ref-man--shr-render-buffer-quiet (buffer buffer-name)
  "Display the HTML rendering of the current buffer."
  (interactive (list (current-buffer)))
  (or (fboundp 'libxml-parse-html-region)
      (error "This function requires Emacs to be compiled with libxml2"))
  (with-current-buffer (get-buffer-create buffer-name)
    (erase-buffer)
    (shr-insert-document
     (with-current-buffer buffer
       (libxml-parse-html-region (point-min) (point-max))))
    (goto-char (point-min))))
(make-obsolete 'ref-man--shr-render-buffer-quiet 'with-temp-shr-buffer "ref-man 0.3.0")

(defun ref-man-url-get-bibtex-link-from-arxiv (url)
  "Extract bibtex if it exists from an arxiv URL."
  (interactive)
  (save-excursion
    (let ((buf (url-retrieve-synchronously url)))
      (with-temp-shr-buffer
       buf
       (let* ((match (search-forward "bibtex" nil t))
              (bib-url (when match
                         (backward-char)
                         (car (eww-links-at-point)))))
         (when bib-url
           (replace-regexp-in-string "/bibtex/" "/bib2/" (concat bib-url ".bib"))))))))

(defun ref-man-url-get-supplementary-url-according-to-source (url)
  "Try and get url for supplementary material for given URL."
  (when url
    (cond ((string-match-p "doi.org" url)
           (ref-man-url-get-supplementary-url-from-doi url))
          ((string-match-p "arxiv.org" url)
           (ref-man-url-get-supplementary-url-from-arxiv url))
          ((string-match-p "aclweb.org" url)
           (ref-man-url-get-supplementary-url-from-acl url))
          ((string-match-p "papers.nips.cc\\|proceedings.neurips.cc" url)
           (ref-man-url-get-supplementary-url-from-nips-url url))
          ((string-match-p "openaccess.thecvf.com" url)
           (ref-man-url-get-supplementary-url-from-cvf-url url))
          ((string-match-p "aaai.org" url)
           (ref-man-url-get-supplementary-url-from-aaai-url url))
          ((string-match-p "dl.acm.org" url)
           (ref-man-url-get-supplementary-url-from-acm-url url))
          ((string-match-p "openreview.net" url)
           (ref-man-url-get-supplementary-url-from-openreview-url url))
          (t url))))

(defun ref-man-url-cvpr-pdf-link-helper (url &rest args)
  "Get PDF URL for an arxiv url.
ARGS is a plist with keywords :heading and optional :url and :year."
  (when (and (plist-get args :heading)
             (not (string-empty-p (plist-get args :heading))))
    ;; TODO: Otherwise get title from DOI
    (ref-man-python-get-cvpr-url (plist-get args :heading) url)))

(defun ref-man-python-get-cvpr-url (title &optional url year)
  "Get the cvpr url from python server for given TITLE if possible.
Optional YEAR if not specified but can be extracted from a DOI
URL.  If neither are given, then the pdf url for the longest regexp
match for TITLE's first three words will be returned."
  (let* ((title (ref-man--remove-punc title t))
         (year (or year (and url (nth 1 (split-string (-last-item (split-string url "/" t)) "\\." t)))))
         (buf (url-retrieve-synchronously (format "http://localhost:%s/get_cvpr_url?title=%s&year=%s"
                                          ref-man-python-server-port title year)))
         (buf-string (with-current-buffer buf
                       (goto-char (point-min))
                       (re-search-forward "\r?\n\r?\n")
                       (buffer-substring-no-properties (point) (point-max))))
         (splits (split-string buf-string "\n" t)))
    (when (and (= (length splits) 1) (car splits)
               (not (string-match-p "^error" (car splits))))
      (cadr (split-string (car splits) ";" t)))))

(defun ref-man-url-get-pdf-link-from-doi (url)
  "Fetch pdf url after DOI redirect for URL."
  (doi-utils-get-pdf-url url))

(defun ref-man-url-get-bibtex-link-from-doi (url)
  "Buffer redirects correctly to IEEE (or some other site), but I
  can't really download from there"
  ;; If link is cvpr or iccv, then find the cvf link and go to that site
  (message "[ref-man] Not Implemented yet") nil)

;; (defvar ref-man--ieee-parse)
(defun ref-man--parse-ieee-page (url)
  "Extract some information from an IEEE URL."
  (let* ((buf (url-retrieve-synchronously url t))
         (ieee-xml-parse (with-current-buffer buf
                           (libxml-parse-html-region (point-min) (point-max))))
         ref-man--ieee-parse)
    (seq-do
     (lambda (x) (when (string-match-p "global.document.metadata" x)
                   (setq ref-man--ieee-parse
                         (json-read-from-string
                          (car (split-string (nth 1 (split-string x "global.document.metadata=")) "\n"))))))
     (dom-strings ieee-xml-parse))
    ref-man--ieee-parse))


(provide 'ref-man-url)

;;; ref-man-url.el ends here
