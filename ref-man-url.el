;;; ref-man-url.el --- url utilities and functions for `ref-man'. ;;; -*- lexical-binding: t; -*-

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
;; Url functions, checks for various properties, conversion to and from proxy,
;; etc.

;;; Code:

(unless (featurep 'ref-man-util)
  (require 'ref-man-util))
(require 'doi-utils)

(defcustom ref-man-pdf-proxy-port nil
  "Fetch PDFs over a proxy server if non-nil.

If this is non-nil then the all the pdf downloads go through the
python server which additionally fetches it from an http proxy at
localhost specified by this port."
  :type 'boolean
  :group 'ref-man)

(defcustom ref-man-pdf-proxy-ignore-sites nil
  "Don't use the proxy for these sites while fetching PDF.

See `ref-man-pdf-proxy-port'."
  :type '(repeat symbol)
  :group 'ref-man)

;; NOTE: External variables
(defvar ref-man-py-server-port)     ; from `ref-man-py'
(defvar ref-man-documents-dir)          ; from `ref-man-files'

(defvar ref-man-url-supported-sites
  '(acl arxiv neurips mlr aaai acm doi-cvpr cvf cvf-old openreview ss)
  "List of supported sites for fetching pdf.")

(defun ref-man-url-meta-url (url)
  "Return if the URL is a meta url.
Meta url is from one of the three sites:
1. arxiv.org
2. semanticscholar.org
3. doi.org."
  (cond ((string-match-p "arxiv.org" url) 'arxiv)
        ((string-match-p "semanticscholar.org/paper" url) 'ss)
        ((string-match-p "^https?://dx.doi.org/.+$" url) 'doi)
        (t nil)))

(defvar ref-man-url-types '(("^https?://arxiv.org/.+" . arxiv)
                            ("^https?://semanticscholar.org/.+" . ss)
                            ("^https?://dx.doi.org/.+" . doi)
                            ("^https?://github.com/.+". github))
  "Alist of recognized url regex and types.
The keys of the alist are regexps and the values are the types of URL.")

(defvar ref-man-url-org-prop-types
  '("URL" "DOI_URL" "PDF_URL" "ARXIV_URL" "ALT_URL")
  "Keys of org property considered as a URL.")

(defvar ref-man-url-maybe-pdf-url-prop-types
  '("URL" "PDF_URL" "ARXIV_URL" "ALT_URL")
  "Keys of org property considered as a URL.")

(defun ref-man-url-to-arxiv-id (url)
  "Get arxivid from an arxiv.org URL."
  (when (eq (ref-man-url-get-site-from-url url) 'arxiv)
    (replace-regexp-in-string "\\(.+?\\)\\(v[0-9]+\\)?\\(\\.pdf\\)?" "\\1"
                              (-last-item (split-string url "/")))))

;; TODO: Unify arxiv functions url into a single one
(defun ref-man-url-from-arxiv-id ()
  "Get url from arxivid extracted from org property drawer at point."
  (interactive)
  (if (eq major-mode 'org-mode)
      (let ((arxiv-id (pcase (or (org-entry-get (point) "ARXIVID")
                                 (org-entry-get (point) "EPRINT"))
                        ((and val) val))))
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
  "Return proxy URL if present.

See `ref-man-pdf-proxy-port' for details."
  (let ((prefix (format "http://localhost:%s/fetch_proxy?url="
                        ref-man-py-server-port)))
    (if (and ref-man-pdf-proxy-port (not (string-prefix-p "http://localhost" url)))
        (concat prefix url)
      url)))

;; CHECK: Do I require something similar in case some other URL is proxied?
(defun ref-man-url-maybe-unproxy (url)
  "Remove proxy prefix if present and return URL.

See `ref-man-pdf-proxy-port' for details."
  (let ((prefix (format "http://localhost:%s/fetch_proxy?url="
                        ref-man-py-server-port)))
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

(defun ref-man-url-downloadable-pdf-url-p (url &optional transform)
  "Does the given URL contain a pdf to download.

If optional TRANSFORM is non-nil then change the URL according to its type to a
downloadable one."
  (and (ref-man-url-non-gscholar-url-p url)
       (cond ((string-match-p "arxiv.org" url)
              (if (string-match-p "/pdf/" url) url
                (if transform (ref-man-url-arxiv-pdf-link-helper url) url)))
             ((string-match-p "aaai.org" url)
              (when (or (string-match-p "/download/" url)
                        (string-match-p "ojs.aaai.org.+article/view/[0-9]+/[0-9]+" url))
                (if transform
                    (replace-regexp-in-string "/article/view/" "/article/download/" url)
                  url)))
             ((string-match-p "openreview.net" url)
              (if transform
                  (replace-regexp-in-string "forum\\?id=" "pdf?id=" url)
                url))
             ((string-match-p "dl.acm.org" url)
              (when (string-match-p "doi/pdf" url) url))
             (t (when (string-match-p "\\.pdf$" url) url)))))

;; (defun ref-man-url-get-bibtex-link-from-nips-url  (url))
;; (defun ref-man-url-get-bibtex-link-from-cvf-url  (url))
;; (defun ref-man-url-get-bibtex-link-from-aaai-url  (url))
;; (defun ref-man-url-get-bibtex-link-from-acm-url  (url))
;; (defun ref-man-url-get-bibtex-link-from-openreview-url  (url))
;; (defun ref-man-url-get-supplementary-url-from-doi  (url))
;; (defun ref-man-url-get-supplementary-url-from-arxiv  (url))
;; (defun ref-man-url-get-supplementary-url-from-acl  (url))
;; (defun ref-man-url-get-supplementary-url-from-nips-url  (url))
;; (defun ref-man-url-get-supplementary-url-from-cvf-url  (url))
;; (defun ref-man-url-get-supplementary-url-from-aaai-url  (url))
;; (defun ref-man-url-get-supplementary-url-from-acm-url  (url))
;; (defun ref-man-url-get-supplementary-url-from-openreview-url (url))

(defmacro with-temp-shr-buffer (buf &rest body)
  "Construct a temp `shr' buffer from url buffer BUF and execute BODY."
  (declare (indent 1) (debug t))
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
Arguments BUF and BODY are same as in `with-temp-shr-buffer'.

Useful when you want the buffer to persist afterwards."
  (declare (indent 1) (debug t))
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

(defun ref-man-url-get-all-links-from-html-buffer (buf &optional predicate)
  "Extract all links from an html response buffer BUF.
The buffer is first rendered with `shr' and then searched for
links.  With optional PREDICATE, return links that satisfies
predicate."
  (with-temp-shr-buffer buf
    (let ((predicate (or predicate #'identity)))
      (-filter predicate (ref-man-web-get-all-links
                          (current-buffer) t)))))

(defun ref-man-url-get-first-pdf-link-from-html-buffer (buf &optional predicate)
  "Extract first pdf link from an html response buffer BUF.
The buffer is first rendered with `shr' and then searched for
links.  With optional PREDICATE, return first link that satisfies
predicate."
  (with-temp-shr-buffer buf
    (let ((predicate (or predicate #'identity)))
      (-first predicate (ref-man-web-get-all-links
                         (current-buffer) t nil "pdf")))))

(defun ref-man-url-get-last-pdf-link-from-html-buffer (buf &optional predicate)
  "Extract last pdf link from an html response buffer BUF.
The buffer is first rendered with `shr' and then searched for
links.  With optional PREDICATE, return first link that satisfies
predicate."
  (with-temp-shr-buffer buf
    (let ((predicate (or predicate #'identity)))
      (-last predicate (ref-man-web-get-all-links
                        (current-buffer) t nil "pdf")))))

(defun ref-man-url-get-first-link-from-html-buffer (buf &optional predicate)
  "Extract first link from an html response buffer BUF.
The buffer is first rendered with `shr' and then searched for
links.  With optional PREDICATE, return first link that satisfies
predicate."
  (with-temp-shr-buffer buf
    (let ((predicate (or predicate #'identity)))
      (-first predicate (ref-man-web-get-all-links
                         (current-buffer) t)))))

(defun ref-man-url-get-last-link-from-html-buffer (buf &optional predicate)
  "Extract last link from an html response buffer BUF.
The buffer is first rendered with `shr' and then searched for
links.  With optional PREDICATE, return first link that satisfies
predicate."
  (with-temp-shr-buffer buf
    (let ((predicate (or predicate #'identity)))
      (-last predicate (ref-man-web-get-all-links
                        (current-buffer) t)))))

(defun ref-man-url-domain (url)
  "Get the domain name with protocol for URL."
  (string-join
   (-take (1+ (-find-index (lambda (x) (string-match-p "\\." x))
                           (split-string url "/")))
          (split-string url "/"))
   "/"))

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
                                     (-drop count lsplits))
                            "/"))))))

;; NOTE: This is equivalent to run-hook-with-args-until-success
;;       BUT there is recursion here for redirects
(defun ref-man-url-get-pdf-link-helper (site url buf &optional status)
  "Helper function to get pdf link from URL given type of SITE.
BUF is the html buffer retrieved from URL.  Optional status is a
plist which contains the http status.

SITE is a symbol according to which the helper function is
returned.  Possible values for SITE are:

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
  (pcase site
    ('acl (ref-man-url-acl-pdf-link-helper url))
    ('arxiv (ref-man-url-arxiv-pdf-link-helper url))
    ('doi (let* ((url (plist-get status :redirect))
                 (site (ref-man-url-get-site-from-url url)))
            ;; Call self again with new URL
            (ref-man-url-get-pdf-link-helper site url buf)))
    ('neurips (let ((link (ref-man-url-get-first-pdf-link-from-html-buffer buf)))
                (when link
                  (cond ((string-match-p "^http:\\|^https:" link) link)
                        ((string-match-p "^/paper/" link)
                         (url-join (string-join (-take 3 (split-string url "/")) "/") link))
                        (t nil)))))
    ('mlr (let ((link (ref-man-url-get-first-pdf-link-from-html-buffer buf)))
            (when (and link (string-match-p "^http:\\|^https:" link)) link)))
    ('aaai (let ((link (pcase (ref-man-url-get-all-links-from-html-buffer
                               buf
                               (lambda (x) (string-match-p (rx "http"
                                                               (+? any)
                                                               (or "article" "paper")
                                                               "/" "view" (+ any))
                                                           x)))
                         (`(,x) x)
                         (_ nil))))
             (or link
                 (let ((buf (if (string-match-p "This page requires frames."
                                                (with-current-buffer buf (buffer-string)))
                                (url-retrieve-synchronously (ref-man-url-get-last-link-from-html-buffer buf) t)
                              buf)))
                   (replace-regexp-in-string "/paper/view/" "/paper/viewFile/"
                                             (ref-man-url-get-last-link-from-html-buffer buf))))))
    ('acm (let ((link (ref-man-url-acm-pdf-link-helper-subr url)))
            (if (string-match-p "/pdf/" link)
                link
              (ref-man-url-acm-pdf-link-helper url))))
    ('cvf (let ((link (ref-man-url-get-first-pdf-link-from-html-buffer buf)))
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
    ('cvf-old (let ((link (ref-man-url-get-first-pdf-link-from-html-buffer buf)))
                (when link
                  (cond ((string-match-p "^http:\\|^https:" link) link)
                        ((string-match-p "^../../content_.*" link)
                         (url-join (string-join (-take 4 (split-string url "/")) "/")
                                   (string-join (-drop 2 (split-string link "/")) "/")))
                        (t nil)))))
    ('openreview (let ((link (ref-man-url-get-first-pdf-link-from-html-buffer buf)))
                   (when link
                     (if (string-match-p "^[http|https]" link)
                         link
                       (url-join "https://openreview.net" link)))))
    ('ss (let* ((links (with-temp-shr-buffer buf
                                             (ref-man-web-get-all-links (current-buffer) t)))
                (site-url (ref-man-url-get-best-pdf-url links))
                (url (when site-url
                       (cond ((eq (car site-url) 'pdf)
                              (cdr site-url))
                             ((car site-url)
                              (condition-case nil
                                  (ref-man-url-get-pdf-link-helper (car site-url) (cdr site-url) nil)
                                (error (with-current-buffer
                                           (url-retrieve-synchronously (cdr site-url))
                                         (ref-man-url-get-pdf-link-helper
                                          (car site-url)
                                          (cdr site-url)
                                          (current-buffer))))))))))
           url))
    (_ (let ((link (ref-man-url-get-first-pdf-link-from-html-buffer buf)))
         (when link
           (if (string-match-p "^http:\\|^https:" link)
               link
             (ref-man-url-get-absolute-path url link)))))))

(defun ref-man-url-acm-transform (link)
  "Transform an acm url LINK."
  (when link
    (if (string-prefix-p "https://dl.acm.org/" link)
        link
      (url-join "https://dl.acm.org/" link))))

(defun ref-man-url-acm-pdf-link-helper-subr (url)
  "ACM PDF URL link helper subroutine."
  (pcase url
    ((pred (string-match-p "/doi/abs/"))
     (replace-regexp-in-string "/doi/abs/" "/doi/pdf/" url))
    ((and (pred (string-match-p "/doi/")) (pred (not (string-match-p "/doi/abs/"))))
     (replace-regexp-in-string "/doi/" "/doi/pdf/" url))
    (_ url)))

(defun ref-man-url-acm-pdf-link-helper-redirect (url buf &rest args)
  "Redirect helper for ACM pdf URL.
BUF is the html buffer received."
  (let ((link (ref-man-url-acm-pdf-link-helper-subr
               (with-temp-shr-buffer
                buf
                (or (car (ref-man-web-get-all-links (current-buffer) nil nil "gateway"))
                    (car (ref-man-web-get-all-links (current-buffer) nil nil "doi/pdf"))
                    (car (ref-man-web-get-all-links (current-buffer) nil nil "doi/abs")))))))
    link))

(defun ref-man-url-acm-pdf-link-helper (url &rest args)
  "ACM PDF URL link helper."
  (ref-man-url-acm-transform
   (or (ref-man-url-downloadable-pdf-url-p (ref-man-url-acm-pdf-link-helper-subr url))
       (ref-man-url-acm-pdf-link-helper-redirect url))))

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
        ((string-match-p "semanticscholar.org/paper" url) 'ss)
        ((string-match-p "^https?://\\(dx.\\)?doi.org/.+$" url) 'doi)
        ((string-match-p "aclanthology.info\\|aclweb.org" url) 'acl)
        ((and (string-match-p "doi.org" url)
              (string-match-p "cvpr" (-last-item (split-string url "/" t))))
         'doi-cvpr)
        ((string-match-p "papers.nips.cc\\|proceedings.neurips.cc" url) 'neurips)
        ((string-match-p "mlr.press" url) 'mlr)
        ((string-match-p "openaccess.thecvf.com" url) 'cvf)
        ((string-match-p "ieeexplore.ieee.org" url) 'ieee)
        ((string-match-p "cv-foundation.org" url) 'old-cvf)
        ((string-match-p "aaai.org" url) 'aaai)
        ((string-match-p "acm.org" url) 'acm)
        ((string-match-p "openreview.net" url) 'openreview)
        (t nil)))

(defun ref-man-url--sort-best-subroutine (x)
  "Sort best subroutines for a given '(site . url) X."
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
        (and best
             (cond ((and (symbolp (car best))
                         (listp (cdr best)))
                    (cons (car best)
                          (cadr best)))
                   ((and (symbolp (car best))
                         (stringp (cdr best)))
                    best)
                   (t (ref-man-url--sort-best-subroutine best))))))))

;; FIXME: This fetches a url FIRST AND THEN tries to obtain if
;;        `ref-man-url-get-pdf-link-helper' is used.
;;
;;        This is incorrect as for example a simple transform can sometimes
;;        determine the PDF url.
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
given, it is called on the PDF URL."
  (when url
    (let* ((site (ref-man-url-get-site-from-url url))
           (helper (pcase site
                     ('arxiv #'ref-man-url-arxiv-pdf-link-helper)
                     ('acl #'ref-man-url-acl-pdf-link-helper)
                     ('doi-cvpr #'ref-man-url-cvf-pdf-link-helper)
                     ('ieee #'ref-man-url-cvf-pdf-link-helper)
                     ('acm #'ref-man-url-acm-pdf-link-helper)
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

;; ;; NOTE: This function currently is only used by `ref-man-try-fetch-and-store-pdf-in-org-entry'
;; (defun ref-man-url-get-bib-according-to-source (url)
;;   "Return a bibtex entry according to URL."
;;   (when url
;;     ;; NOTE: This how it could be if a cons of (url . buf-or-name) is given
;;     ;; (when (and (listp buf-url) (ref-man--downloadable-bib-url-p (cdr url)))
;;     ;;   (ref-man--get-bib-from-url-buf url)
;;     ;;   )
;;     (cond ((string-match-p "doi.org" url)
;;            (ref-man-url-get-bibtex-link-from-doi url))
;;           ((string-match-p "arxiv.org" url)
;;            (ref-man-url-get-bibtex-link-from-arxiv url))
;;           ((string-match-p "aclweb.org" url)
;;            (concat (replace-regexp-in-string "/$" "" url) ".bib"))
;;           ((string-match-p "aclanthology.info" url)
;;            (cons 'url (concat "https://www.aclweb.org/anthology/"
;;                               (upcase (car (last (split-string url "/")))) ".bib")))
;;           ((string-match-p "papers.nips.cc\\|proceedings.neurips.cc" url)
;;            (ref-man-url-get-bibtex-link-from-nips-url url))
;;           ((string-match-p "openaccess.thecvf.com" url)
;;            (ref-man-url-get-bibtex-link-from-cvf-url url))
;;           ((string-match-p "aaai.org" url)
;;            (ref-man-url-get-bibtex-link-from-aaai-url url))
;;           ((string-match-p "dl.acm.org" url)
;;            (ref-man-url-get-bibtex-link-from-acm-url url))
;;           ((string-match-p "openreview.net" url)
;;            (ref-man-url-get-bibtex-link-from-openreview-url url))
;;           (t url))))

(defun ref-man--shr-render-buffer-quiet (buffer buffer-name)
  "Display the HTML rendering of the BUFFER.

The rendered buffer is a named buffer BUFFER-NAME."
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

;; (defun ref-man-url-get-supplementary-url-according-to-source (url)
;;   "Try and get url for supplementary material for given URL."
;;   (when url
;;     (cond ((string-match-p "doi.org" url)
;;            (ref-man-url-get-supplementary-url-from-doi url))
;;           ((string-match-p "arxiv.org" url)
;;            (ref-man-url-get-supplementary-url-from-arxiv url))
;;           ((string-match-p "aclweb.org" url)
;;            (ref-man-url-get-supplementary-url-from-acl url))
;;           ((string-match-p "papers.nips.cc\\|proceedings.neurips.cc" url)
;;            (ref-man-url-get-supplementary-url-from-nips-url url))
;;           ((string-match-p "openaccess.thecvf.com" url)
;;            (ref-man-url-get-supplementary-url-from-cvf-url url))
;;           ((string-match-p "aaai.org" url)
;;            (ref-man-url-get-supplementary-url-from-aaai-url url))
;;           ((string-match-p "dl.acm.org" url)
;;            (ref-man-url-get-supplementary-url-from-acm-url url))
;;           ((string-match-p "openreview.net" url)
;;            (ref-man-url-get-supplementary-url-from-openreview-url url))
;;           (t url))))

(defun ref-man-url-parse-cvf-venue (doi venue)
  "Get the correct venue from a CVF DOI and VENUE.

DOI is checked first and if not present VENUE is checked."
  (cond ((and doi (let ((case-fold-search nil))
                    (string-match ".*\\(ICCV\\|CVPR\\|WACV\\).*" doi)))
         (match-string 1 doi))
        (venue
         (let ((case-fold-search t))
           (cond ((string-match ".*\\(ICCV\\|CVPR\\|WACV\\).*" venue)
                  (match-string 1 venue))
                 ((string-match-p ".*computer.*vision.*pattern.*recognition.*" venue)
                  "CVPR")
                 ((string-match-p ".*international.*conference.*computer.*vision.*" venue)
                  "ICCV")
                 ((string-match-p "winter.+applications.*computer.*vision" venue)
                  "WACV")
                 (t nil))))))

(defun ref-man-url-cvf-pdf-link-helper (url &optional args)
  "Get PDF URL for a CVF url.
ARGS is a plist with keywords :heading :point :buffer"
  (let* ((buf (plist-get args :buffer))
         (heading (plist-get args :heading))
         (pt (plist-get args :point))
         (doi (with-current-buffer buf (org-entry-get pt "DOI")))
         (venue (ref-man-url-parse-cvf-venue
                 doi
                 (with-current-buffer buf (org-entry-get pt "VENUE"))))
         (year (with-current-buffer buf (org-entry-get pt "YEAR"))))
    (when (and heading (not (string-empty-p heading)))
      (when (string-match-p "ieeexplore.ieee.org" url)
        (unless venue
          (setq venue (read-from-minibuffer
                       "Could not determine CVF venue. Enter: ")))
        (if (string-empty-p venue)
            (user-error "No venue given")
          (ref-man-py-get-cvf-url heading venue url year))))))

(defun ref-man-url-get-cvf-url (title venue &optional url year)
  "Get the cvpr url from python server for given TITLE and VENUE.

Optional YEAR if not specified but can be extracted from a DOI
URL.  If neither are given, then the pdf url for the longest regexp
match for TITLE's first three words will be returned."
  (let* ((title (replace-regexp-in-string "[^0-9a-z ]" " " title))
         (year (or year (and url (nth 1 (split-string (-last-item (split-string url "/" t)) "\\." t)))))
         (buf (url-retrieve-synchronously
               (ref-man-py-url "get_cvf_url" `(("title" . ,title)
                                               ("venue" . ,venue)
                                               ("year" . ,year)))))
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
  "Get a bibtex from a DOI URL."
  ;; FIXME: Buffer redirects correctly to IEEE (or some other site), but I can't
  ;; really download from there

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
