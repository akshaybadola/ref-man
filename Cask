(source gnu)
(source melpa)

(package-file "ref-man.el")

(depends-on "a" "0.1.1")
;; NOTE: Removed util as cask git install dependency seems buggy
(depends-on "util"
	    :git "https://github.com/akshaybadola/emacs-util"
            :branch "master")

(development
 (depends-on "f")
 (depends-on "ecukes")
 (depends-on "ert-runner")
 (depends-on "el-mock"))
