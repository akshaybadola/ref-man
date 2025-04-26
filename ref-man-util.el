;;; ref-man-util.el --- Utility variables and functions for `ref-man'. ;;; -*- lexical-binding: t; -*-

;; Copyright (C) 2018,2019,2020,2021,2022,2023,2025
;; Akshay Badola

;; Author:	Akshay Badola <akshay.badola.cs@gmail.com>
;; Maintainer:	Akshay Badola <akshay.badola.cs@gmail.com>
;; Time-stamp:	<Saturday 26 April 2025 07:52:08 AM IST>
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
;; A bunch of utility functions and some common variables for `ref-man'.

;;; Code:

(require 'bibtex)

(defconst ref-man-stop-words
  '("a" "about" "above" "after" "again" "against"
    "all" "an" "and" "any" "are" "as" "at" "because"
    "before" "below" "between" "both" "by" "can" "did"
    "do" "does" "don" "down" "during" "each" "few"
    "for" "from" "further" "had" "has" "have" "having"
    "here" "how" "i" "in" "into" "is" "it" "its"
    "just" "more" "most" "no" "nor" "not" "now"
    "of" "off" "on" "once" "only" "other" "out"
    "over" "own" "same" "should" "so" "some" "such"
    "t" "than" "that" "the" "then" "there" "these"
    "through" "to" "too" "under" "up" "very" "was" "were"
    "what" "when" "where" "which" "who" "why" "will" "with"))

(defconst ref-man-alphabet
  '("a" "b" "c" "d" "e" "f" "g" "h" "i" "j" "k" "l" "m" "n" "o" "p" "q" "r" "s" "t" "u" "v" "w" "x" "y" "z" "A" "B" "C" "D" "E" "F" "G" "H" "I" "J" "K" "L" "M" "N" "O" "P" "Q" "R" "S" "T" "U" "V" "W" "X" "Y" "Z")
  "Just a list of a-z A-Z.")

(defconst ref-man--num-to-months
  '((1 . "Jan") (2 . "Feb") (3 . "Mar") (4 . "Apr")
    (5 . "May") (6 . "Jun") (7 . "Jul") (8 . "Aug")
    (9 . "Sep") (10 . "Oct") (11 . "Nov") (12 . "Dec")))

;; (eval-and-compile
;;   (defconst ref-man-char-table
;;     (let* ((buf (find-file-noselect (concat (file-name-directory (buffer-file-name)) "/" "charlist.json")))
;;            (charlist (with-current-buffer buf
;;                        (goto-char (point-min))
;;                        (json-read))))
;;       (prog1 charlist
;;         (kill-buffer buf)))
;;     "Char table constructed from w3.org."))

(eval-and-compile
  (defconst ref-man-char-table
    (json-read-from-string "{
  \"32\": {
    \"char\": \" \",
    \"latex\": \"\\\\space \",
    \"math\": null
  },
  \"33\": {
    \"char\": \"!\",
    \"latex\": \"!\",
    \"math\": null
  },
  \"34\": {
    \"char\": \"\\\"\",
    \"latex\": \"\\\"\",
    \"math\": null
  },
  \"35\": {
    \"char\": \"#\",
    \"latex\": \"\\\\#\",
    \"math\": null
  },
  \"36\": {
    \"char\": \"$\",
    \"latex\": \"\\\\textdollar \",
    \"math\": \"\\\\$\"
  },
  \"37\": {
    \"char\": \"%\",
    \"latex\": \"\\\\%\",
    \"math\": null
  },
  \"38\": {
    \"char\": \"&\",
    \"latex\": \"\\\\&\",
    \"math\": null
  },
  \"39\": {
    \"char\": \"'\",
    \"latex\": \"\\\\textquotesingle \",
    \"math\": null
  },
  \"40\": {
    \"char\": \"(\",
    \"latex\": \"(\",
    \"math\": null
  },
  \"41\": {
    \"char\": \")\",
    \"latex\": \")\",
    \"math\": null
  },
  \"42\": {
    \"char\": \"*\",
    \"latex\": \"\\\\ast \",
    \"math\": null
  },
  \"43\": {
    \"char\": \"+\",
    \"latex\": \"+\",
    \"math\": null
  },
  \"44\": {
    \"char\": \",\",
    \"latex\": \",\",
    \"math\": null
  },
  \"45\": {
    \"char\": \"-\",
    \"latex\": \"-\",
    \"math\": null
  },
  \"46\": {
    \"char\": \".\",
    \"latex\": \".\",
    \"math\": null
  },
  \"47\": {
    \"char\": \"/\",
    \"latex\": \"/\",
    \"math\": null
  },
  \"48\": {
    \"char\": \"0\",
    \"latex\": \"0\",
    \"math\": null
  },
  \"49\": {
    \"char\": \"1\",
    \"latex\": \"1\",
    \"math\": null
  },
  \"50\": {
    \"char\": \"2\",
    \"latex\": \"2\",
    \"math\": null
  },
  \"51\": {
    \"char\": \"3\",
    \"latex\": \"3\",
    \"math\": null
  },
  \"52\": {
    \"char\": \"4\",
    \"latex\": \"4\",
    \"math\": null
  },
  \"53\": {
    \"char\": \"5\",
    \"latex\": \"5\",
    \"math\": null
  },
  \"54\": {
    \"char\": \"6\",
    \"latex\": \"6\",
    \"math\": null
  },
  \"55\": {
    \"char\": \"7\",
    \"latex\": \"7\",
    \"math\": null
  },
  \"56\": {
    \"char\": \"8\",
    \"latex\": \"8\",
    \"math\": null
  },
  \"57\": {
    \"char\": \"9\",
    \"latex\": \"9\",
    \"math\": null
  },
  \"58\": {
    \"char\": \":\",
    \"latex\": \":\",
    \"math\": null
  },
  \"59\": {
    \"char\": \";\",
    \"latex\": \";\",
    \"math\": null
  },
  \"60\": {
    \"char\": \"<\",
    \"latex\": \"<\",
    \"math\": null
  },
  \"61\": {
    \"char\": \"=\",
    \"latex\": \"=\",
    \"math\": null
  },
  \"62\": {
    \"char\": \">\",
    \"latex\": \">\",
    \"math\": null
  },
  \"63\": {
    \"char\": \"?\",
    \"latex\": \"?\",
    \"math\": null
  },
  \"64\": {
    \"char\": \"@\",
    \"latex\": \"@\",
    \"math\": null
  },
  \"65\": {
    \"char\": \"A\",
    \"latex\": \"A\",
    \"math\": null
  },
  \"66\": {
    \"char\": \"B\",
    \"latex\": \"B\",
    \"math\": null
  },
  \"67\": {
    \"char\": \"C\",
    \"latex\": \"C\",
    \"math\": null
  },
  \"68\": {
    \"char\": \"D\",
    \"latex\": \"D\",
    \"math\": null
  },
  \"69\": {
    \"char\": \"E\",
    \"latex\": \"E\",
    \"math\": null
  },
  \"70\": {
    \"char\": \"F\",
    \"latex\": \"F\",
    \"math\": null
  },
  \"71\": {
    \"char\": \"G\",
    \"latex\": \"G\",
    \"math\": null
  },
  \"72\": {
    \"char\": \"H\",
    \"latex\": \"H\",
    \"math\": null
  },
  \"73\": {
    \"char\": \"I\",
    \"latex\": \"I\",
    \"math\": null
  },
  \"74\": {
    \"char\": \"J\",
    \"latex\": \"J\",
    \"math\": null
  },
  \"75\": {
    \"char\": \"K\",
    \"latex\": \"K\",
    \"math\": null
  },
  \"76\": {
    \"char\": \"L\",
    \"latex\": \"L\",
    \"math\": null
  },
  \"77\": {
    \"char\": \"M\",
    \"latex\": \"M\",
    \"math\": null
  },
  \"78\": {
    \"char\": \"N\",
    \"latex\": \"N\",
    \"math\": null
  },
  \"79\": {
    \"char\": \"O\",
    \"latex\": \"O\",
    \"math\": null
  },
  \"80\": {
    \"char\": \"P\",
    \"latex\": \"P\",
    \"math\": null
  },
  \"81\": {
    \"char\": \"Q\",
    \"latex\": \"Q\",
    \"math\": null
  },
  \"82\": {
    \"char\": \"R\",
    \"latex\": \"R\",
    \"math\": null
  },
  \"83\": {
    \"char\": \"S\",
    \"latex\": \"S\",
    \"math\": null
  },
  \"84\": {
    \"char\": \"T\",
    \"latex\": \"T\",
    \"math\": null
  },
  \"85\": {
    \"char\": \"U\",
    \"latex\": \"U\",
    \"math\": null
  },
  \"86\": {
    \"char\": \"V\",
    \"latex\": \"V\",
    \"math\": null
  },
  \"87\": {
    \"char\": \"W\",
    \"latex\": \"W\",
    \"math\": null
  },
  \"88\": {
    \"char\": \"X\",
    \"latex\": \"X\",
    \"math\": null
  },
  \"89\": {
    \"char\": \"Y\",
    \"latex\": \"Y\",
    \"math\": null
  },
  \"90\": {
    \"char\": \"Z\",
    \"latex\": \"Z\",
    \"math\": null
  },
  \"91\": {
    \"char\": \"[\",
    \"latex\": \"[\",
    \"math\": null
  },
  \"92\": {
    \"char\": \"\\\\\",
    \"latex\": \"\\\\textbackslash \",
    \"math\": \"\\\\backslash \"
  },
  \"93\": {
    \"char\": \"]\",
    \"latex\": \"]\",
    \"math\": null
  },
  \"94\": {
    \"char\": \"^\",
    \"latex\": \"\\\\^{}\",
    \"math\": null
  },
  \"95\": {
    \"char\": \"_\",
    \"latex\": \"\\\\_\",
    \"math\": null
  },
  \"96\": {
    \"char\": \"`\",
    \"latex\": \"\\\\textasciigrave \",
    \"math\": null
  },
  \"97\": {
    \"char\": \"a\",
    \"latex\": \"a\",
    \"math\": null
  },
  \"98\": {
    \"char\": \"b\",
    \"latex\": \"b\",
    \"math\": null
  },
  \"99\": {
    \"char\": \"c\",
    \"latex\": \"c\",
    \"math\": null
  },
  \"100\": {
    \"char\": \"d\",
    \"latex\": \"d\",
    \"math\": null
  },
  \"101\": {
    \"char\": \"e\",
    \"latex\": \"e\",
    \"math\": null
  },
  \"102\": {
    \"char\": \"f\",
    \"latex\": \"f\",
    \"math\": null
  },
  \"103\": {
    \"char\": \"g\",
    \"latex\": \"g\",
    \"math\": null
  },
  \"104\": {
    \"char\": \"h\",
    \"latex\": \"h\",
    \"math\": null
  },
  \"105\": {
    \"char\": \"i\",
    \"latex\": \"i\",
    \"math\": null
  },
  \"106\": {
    \"char\": \"j\",
    \"latex\": \"j\",
    \"math\": null
  },
  \"107\": {
    \"char\": \"k\",
    \"latex\": \"k\",
    \"math\": null
  },
  \"108\": {
    \"char\": \"l\",
    \"latex\": \"l\",
    \"math\": null
  },
  \"109\": {
    \"char\": \"m\",
    \"latex\": \"m\",
    \"math\": null
  },
  \"110\": {
    \"char\": \"n\",
    \"latex\": \"n\",
    \"math\": null
  },
  \"111\": {
    \"char\": \"o\",
    \"latex\": \"o\",
    \"math\": null
  },
  \"112\": {
    \"char\": \"p\",
    \"latex\": \"p\",
    \"math\": null
  },
  \"113\": {
    \"char\": \"q\",
    \"latex\": \"q\",
    \"math\": null
  },
  \"114\": {
    \"char\": \"r\",
    \"latex\": \"r\",
    \"math\": null
  },
  \"115\": {
    \"char\": \"s\",
    \"latex\": \"s\",
    \"math\": null
  },
  \"116\": {
    \"char\": \"t\",
    \"latex\": \"t\",
    \"math\": null
  },
  \"117\": {
    \"char\": \"u\",
    \"latex\": \"u\",
    \"math\": null
  },
  \"118\": {
    \"char\": \"v\",
    \"latex\": \"v\",
    \"math\": null
  },
  \"119\": {
    \"char\": \"w\",
    \"latex\": \"w\",
    \"math\": null
  },
  \"120\": {
    \"char\": \"x\",
    \"latex\": \"x\",
    \"math\": null
  },
  \"121\": {
    \"char\": \"y\",
    \"latex\": \"y\",
    \"math\": null
  },
  \"122\": {
    \"char\": \"z\",
    \"latex\": \"z\",
    \"math\": null
  },
  \"123\": {
    \"char\": \"{\",
    \"latex\": \"\\\\lbrace \",
    \"math\": null
  },
  \"124\": {
    \"char\": \"|\",
    \"latex\": \"\\\\vert \",
    \"math\": null
  },
  \"125\": {
    \"char\": \"}\",
    \"latex\": \"\\\\rbrace \",
    \"math\": null
  },
  \"126\": {
    \"char\": \"~\",
    \"latex\": \"\\\\textasciitilde \",
    \"math\": null
  },
  \"160\": {
    \"char\": \" \",
    \"latex\": \"~\",
    \"math\": null
  },
  \"161\": {
    \"char\": \"¡\",
    \"latex\": \"\\\\textexclamdown \",
    \"math\": null
  },
  \"162\": {
    \"char\": \"¢\",
    \"latex\": \"\\\\textcent \",
    \"math\": \"\\\\mbox{\\\\textcent}\"
  },
  \"163\": {
    \"char\": \"£\",
    \"latex\": \"\\\\textsterling \",
    \"math\": \"\\\\pounds \"
  },
  \"164\": {
    \"char\": \"¤\",
    \"latex\": \"\\\\textcurrency \",
    \"math\": \"\\\\mbox{\\\\textcurrency}\"
  },
  \"165\": {
    \"char\": \"¥\",
    \"latex\": \"\\\\textyen \",
    \"math\": \"\\\\yen \"
  },
  \"166\": {
    \"char\": \"¦\",
    \"latex\": \"\\\\textbrokenbar \",
    \"math\": null
  },
  \"167\": {
    \"char\": \"§\",
    \"latex\": \"\\\\textsection \",
    \"math\": \"\\\\S \"
  },
  \"168\": {
    \"char\": \"¨\",
    \"latex\": \"\\\\textasciidieresis \",
    \"math\": null
  },
  \"169\": {
    \"char\": \"©\",
    \"latex\": \"\\\\textcopyright \",
    \"math\": \"\\\\copyright \"
  },
  \"170\": {
    \"char\": \"ª\",
    \"latex\": \"\\\\textordfeminine \",
    \"math\": null
  },
  \"171\": {
    \"char\": \"«\",
    \"latex\": \"\\\\guillemotleft \",
    \"math\": \"\\\\mbox{\\\\guillemotleft}\"
  },
  \"172\": {
    \"char\": \"¬\",
    \"latex\": \"\\\\lnot \",
    \"math\": null
  },
  \"173\": {
    \"char\": \"­\",
    \"latex\": \"\\\\-\",
    \"math\": null
  },
  \"174\": {
    \"char\": \"®\",
    \"latex\": \"\\\\textregistered \",
    \"math\": \"\\\\circledR \"
  },
  \"175\": {
    \"char\": \"¯\",
    \"latex\": \"\\\\textasciimacron \",
    \"math\": null
  },
  \"176\": {
    \"char\": \"°\",
    \"latex\": \"\\\\textdegree \",
    \"math\": \"\\\\mbox{\\\\textdegree}\"
  },
  \"177\": {
    \"char\": \"±\",
    \"latex\": \"\\\\pm \",
    \"math\": null
  },
  \"178\": {
    \"char\": \"²\",
    \"latex\": \"{^2}\",
    \"math\": null
  },
  \"179\": {
    \"char\": \"³\",
    \"latex\": \"{^3}\",
    \"math\": null
  },
  \"180\": {
    \"char\": \"´\",
    \"latex\": \"\\\\textasciiacute \",
    \"math\": null
  },
  \"181\": {
    \"char\": \"µ\",
    \"latex\": \"\\\\mathrm{\\\\mu}\",
    \"math\": null
  },
  \"182\": {
    \"char\": \"¶\",
    \"latex\": \"\\\\textparagraph \",
    \"math\": \"\\\\P \"
  },
  \"183\": {
    \"char\": \"·\",
    \"latex\": \"\\\\cdot \",
    \"math\": null
  },
  \"184\": {
    \"char\": \"¸\",
    \"latex\": \"\\\\c{}\",
    \"math\": \"\\\\mbox{\\\\c{}}\"
  },
  \"185\": {
    \"char\": \"¹\",
    \"latex\": \"{^1}\",
    \"math\": null
  },
  \"186\": {
    \"char\": \"º\",
    \"latex\": \"\\\\textordmasculine \",
    \"math\": null
  },
  \"187\": {
    \"char\": \"»\",
    \"latex\": \"\\\\guillemotright \",
    \"math\": \"\\\\mbox{\\\\guillemotright}\"
  },
  \"188\": {
    \"char\": \"¼\",
    \"latex\": \"\\\\textonequarter \",
    \"math\": null
  },
  \"189\": {
    \"char\": \"½\",
    \"latex\": \"\\\\textonehalf \",
    \"math\": null
  },
  \"190\": {
    \"char\": \"¾\",
    \"latex\": \"\\\\textthreequarters \",
    \"math\": null
  },
  \"191\": {
    \"char\": \"¿\",
    \"latex\": \"\\\\textquestiondown \",
    \"math\": null
  },
  \"192\": {
    \"char\": \"À\",
    \"latex\": \"\\\\`{A}\",
    \"math\": \"\\\\grave{A}\"
  },
  \"193\": {
    \"char\": \"Á\",
    \"latex\": \"\\\\'{A}\",
    \"math\": \"\\\\acute{A}\"
  },
  \"194\": {
    \"char\": \"Â\",
    \"latex\": \"\\\\^{A}\",
    \"math\": \"\\\\hat{A}\"
  },
  \"195\": {
    \"char\": \"Ã\",
    \"latex\": \"\\\\~{A}\",
    \"math\": \"\\\\tilde{A}\"
  },
  \"196\": {
    \"char\": \"Ä\",
    \"latex\": \"\\\\\\\"{A}\",
    \"math\": \"\\\\ddot{A}\"
  },
  \"197\": {
    \"char\": \"Å\",
    \"latex\": \"\\\\AA \",
    \"math\": null
  },
  \"198\": {
    \"char\": \"Æ\",
    \"latex\": \"\\\\AE \",
    \"math\": null
  },
  \"199\": {
    \"char\": \"Ç\",
    \"latex\": \"\\\\c{C}\",
    \"math\": \"\\\\mbox{\\\\c{C}}\"
  },
  \"200\": {
    \"char\": \"È\",
    \"latex\": \"\\\\`{E}\",
    \"math\": \"\\\\grave{E}\"
  },
  \"201\": {
    \"char\": \"É\",
    \"latex\": \"\\\\'{E}\",
    \"math\": \"\\\\acute{E}\"
  },
  \"202\": {
    \"char\": \"Ê\",
    \"latex\": \"\\\\^{E}\",
    \"math\": \"\\\\hat{E}\"
  },
  \"203\": {
    \"char\": \"Ë\",
    \"latex\": \"\\\\\\\"{E}\",
    \"math\": \"\\\\ddot{E}\"
  },
  \"204\": {
    \"char\": \"Ì\",
    \"latex\": \"\\\\`{I}\",
    \"math\": \"\\\\grave{I}\"
  },
  \"205\": {
    \"char\": \"Í\",
    \"latex\": \"\\\\'{I}\",
    \"math\": \"\\\\acute{I}\"
  },
  \"206\": {
    \"char\": \"Î\",
    \"latex\": \"\\\\^{I}\",
    \"math\": \"\\\\hat{I}\"
  },
  \"207\": {
    \"char\": \"Ï\",
    \"latex\": \"\\\\\\\"{I}\",
    \"math\": \"\\\\ddot{I}\"
  },
  \"208\": {
    \"char\": \"Ð\",
    \"latex\": \"\\\\DH \",
    \"math\": null
  },
  \"209\": {
    \"char\": \"Ñ\",
    \"latex\": \"\\\\~{N}\",
    \"math\": \"\\\\tilde{N}\"
  },
  \"210\": {
    \"char\": \"Ò\",
    \"latex\": \"\\\\`{O}\",
    \"math\": \"\\\\grave{O}\"
  },
  \"211\": {
    \"char\": \"Ó\",
    \"latex\": \"\\\\'{O}\",
    \"math\": \"\\\\acute{O}\"
  },
  \"212\": {
    \"char\": \"Ô\",
    \"latex\": \"\\\\^{O}\",
    \"math\": \"\\\\hat{O}\"
  },
  \"213\": {
    \"char\": \"Õ\",
    \"latex\": \"\\\\~{O}\",
    \"math\": \"\\\\tilde{O}\"
  },
  \"214\": {
    \"char\": \"Ö\",
    \"latex\": \"\\\\\\\"{O}\",
    \"math\": \"\\\\ddot{O}\"
  },
  \"215\": {
    \"char\": \"×\",
    \"latex\": \"\\\\texttimes \",
    \"math\": \"\\\\times \"
  },
  \"216\": {
    \"char\": \"Ø\",
    \"latex\": \"\\\\O \",
    \"math\": null
  },
  \"217\": {
    \"char\": \"Ù\",
    \"latex\": \"\\\\`{U}\",
    \"math\": \"\\\\grave{U}\"
  },
  \"218\": {
    \"char\": \"Ú\",
    \"latex\": \"\\\\'{U}\",
    \"math\": \"\\\\acute{U}\"
  },
  \"219\": {
    \"char\": \"Û\",
    \"latex\": \"\\\\^{U}\",
    \"math\": \"\\\\hat{U}\"
  },
  \"220\": {
    \"char\": \"Ü\",
    \"latex\": \"\\\\\\\"{U}\",
    \"math\": \"\\\\ddot{U}\"
  },
  \"221\": {
    \"char\": \"Ý\",
    \"latex\": \"\\\\'{Y}\",
    \"math\": \"\\\\acute{Y}\"
  },
  \"222\": {
    \"char\": \"Þ\",
    \"latex\": \"\\\\TH \",
    \"math\": null
  },
  \"223\": {
    \"char\": \"ß\",
    \"latex\": \"\\\\ss \",
    \"math\": null
  },
  \"224\": {
    \"char\": \"à\",
    \"latex\": \"\\\\`{a}\",
    \"math\": \"\\\\grave{a}\"
  },
  \"225\": {
    \"char\": \"á\",
    \"latex\": \"\\\\'{a}\",
    \"math\": \"\\\\acute{a}\"
  },
  \"226\": {
    \"char\": \"â\",
    \"latex\": \"\\\\^{a}\",
    \"math\": \"\\\\hat{a}\"
  },
  \"227\": {
    \"char\": \"ã\",
    \"latex\": \"\\\\~{a}\",
    \"math\": \"\\\\tilde{a}\"
  },
  \"228\": {
    \"char\": \"ä\",
    \"latex\": \"\\\\\\\"{a}\",
    \"math\": \"\\\\ddot{a}\"
  },
  \"229\": {
    \"char\": \"å\",
    \"latex\": \"\\\\aa \",
    \"math\": null
  },
  \"230\": {
    \"char\": \"æ\",
    \"latex\": \"\\\\ae \",
    \"math\": null
  },
  \"231\": {
    \"char\": \"ç\",
    \"latex\": \"\\\\c{c}\",
    \"math\": \"\\\\mbox{\\\\c{c}}\"
  },
  \"232\": {
    \"char\": \"è\",
    \"latex\": \"\\\\`{e}\",
    \"math\": \"\\\\grave{e}\"
  },
  \"233\": {
    \"char\": \"é\",
    \"latex\": \"\\\\'{e}\",
    \"math\": \"\\\\acute{e}\"
  },
  \"234\": {
    \"char\": \"ê\",
    \"latex\": \"\\\\^{e}\",
    \"math\": \"\\\\hat{e}\"
  },
  \"235\": {
    \"char\": \"ë\",
    \"latex\": \"\\\\\\\"{e}\",
    \"math\": \"\\\\ddot{e}\"
  },
  \"236\": {
    \"char\": \"ì\",
    \"latex\": \"\\\\`{\\\\i}\",
    \"math\": \"\\\\grave{\\\\imath}\"
  },
  \"237\": {
    \"char\": \"í\",
    \"latex\": \"\\\\'{\\\\i}\",
    \"math\": \"\\\\acute{\\\\imath}\"
  },
  \"238\": {
    \"char\": \"î\",
    \"latex\": \"\\^{\\i}\",
    \"math\": \"\\\\hat{\\\\imath}\"
  },
  \"239\": {
    \"char\": \"ï\",
    \"latex\": \"\\\\\\\"{\\\\i}\",
    \"math\": \"\\\\ddot{\\\\imath}\"
  },
  \"240\": {
    \"char\": \"ð\",
    \"latex\": \"\\\\dh \",
    \"math\": null
  },
  \"241\": {
    \"char\": \"ñ\",
    \"latex\": \"\\\\~{n}\",
    \"math\": \"\\\\tilde{n}\"
  },
  \"242\": {
    \"char\": \"ò\",
    \"latex\": \"\\\\`{o}\",
    \"math\": \"\\\\grave{o}\"
  },
  \"243\": {
    \"char\": \"ó\",
    \"latex\": \"\\\\'{o}\",
    \"math\": \"\\\\acute{o}\"
  },
  \"244\": {
    \"char\": \"ô\",
    \"latex\": \"\\\\^{o}\",
    \"math\": \"\\\\hat{o}\"
  },
  \"245\": {
    \"char\": \"õ\",
    \"latex\": \"\\\\~{o}\",
    \"math\": \"\\\\tilde{o}\"
  },
  \"246\": {
    \"char\": \"ö\",
    \"latex\": \"\\\\\\\"{o}\",
    \"math\": \"\\\\ddot{o}\"
  },
  \"247\": {
    \"char\": \"÷\",
    \"latex\": \"\\\\div \",
    \"math\": null
  },
  \"248\": {
    \"char\": \"ø\",
    \"latex\": \"\\\\o \",
    \"math\": null
  },
  \"249\": {
    \"char\": \"ù\",
    \"latex\": \"\\\\`{u}\",
    \"math\": \"\\\\grave{u}\"
  },
  \"250\": {
    \"char\": \"ú\",
    \"latex\": \"\\\\'{u}\",
    \"math\": \"\\\\acute{u}\"
  },
  \"251\": {
    \"char\": \"û\",
    \"latex\": \"\\\\^{u}\",
    \"math\": \"\\\\hat{u}\"
  },
  \"252\": {
    \"char\": \"ü\",
    \"latex\": \"\\\\\\\"{u}\",
    \"math\": \"\\\\ddot{u}\"
  },
  \"253\": {
    \"char\": \"ý\",
    \"latex\": \"\\\\'{y}\",
    \"math\": \"\\\\acute{y}\"
  },
  \"254\": {
    \"char\": \"þ\",
    \"latex\": \"\\\\th \",
    \"math\": null
  },
  \"255\": {
    \"char\": \"ÿ\",
    \"latex\": \"\\\\\\\"{y}\",
    \"math\": \"\\\\ddot{y}\"
  },
  \"256\": {
    \"char\": \"Ā\",
    \"latex\": \"\\\\={A}\",
    \"math\": \"\\\\bar{A}\"
  },
  \"257\": {
    \"char\": \"ā\",
    \"latex\": \"\\\\={a}\",
    \"math\": \"\\\\bar{a}\"
  },
  \"258\": {
    \"char\": \"Ă\",
    \"latex\": \"\\\\u{A}\",
    \"math\": \"\\\\breve{A}\"
  },
  \"259\": {
    \"char\": \"ă\",
    \"latex\": \"\\\\u{a}\",
    \"math\": \"\\\\u{a}\"
  },
  \"260\": {
    \"char\": \"Ą\",
    \"latex\": \"\\\\k{A}\",
    \"math\": null
  },
  \"261\": {
    \"char\": \"ą\",
    \"latex\": \"\\\\k{a}\",
    \"math\": null
  },
  \"262\": {
    \"char\": \"Ć\",
    \"latex\": \"\\\\'{C}\",
    \"math\": \"\\\\acute{C}\"
  },
  \"263\": {
    \"char\": \"ć\",
    \"latex\": \"\\\\'{c}\",
    \"math\": \"\\\\acute{c}\"
  },
  \"264\": {
    \"char\": \"Ĉ\",
    \"latex\": \"\\\\^{C}\",
    \"math\": \"\\\\hat{C}\"
  },
  \"265\": {
    \"char\": \"ĉ\",
    \"latex\": \"\\\\^{c}\",
    \"math\": \"\\\\hat{c}\"
  },
  \"266\": {
    \"char\": \"Ċ\",
    \"latex\": \"\\\\.{C}\",
    \"math\": \"\\\\dot{C}\"
  },
  \"267\": {
    \"char\": \"ċ\",
    \"latex\": \"\\\\.{c}\",
    \"math\": \"\\\\dot{c}\"
  },
  \"268\": {
    \"char\": \"Č\",
    \"latex\": \"\\\\v{C}\",
    \"math\": \"\\\\check{C}\"
  },
  \"269\": {
    \"char\": \"č\",
    \"latex\": \"\\\\v{c}\",
    \"math\": \"\\\\check{c}\"
  },
  \"270\": {
    \"char\": \"Ď\",
    \"latex\": \"\\\\v{D}\",
    \"math\": \"\\\\check{D}\"
  },
  \"271\": {
    \"char\": \"ď\",
    \"latex\": \"\\\\v{d}\",
    \"math\": \"\\\\check{d}\"
  },
  \"272\": {
    \"char\": \"Đ\",
    \"latex\": \"\\\\DJ \",
    \"math\": null
  },
  \"273\": {
    \"char\": \"đ\",
    \"latex\": \"\\\\dj \",
    \"math\": null
  },
  \"274\": {
    \"char\": \"Ē\",
    \"latex\": \"\\\\={E}\",
    \"math\": \"\\\\bar{E}\"
  },
  \"275\": {
    \"char\": \"ē\",
    \"latex\": \"\\\\={e}\",
    \"math\": \"\\\\bar{e}\"
  },
  \"276\": {
    \"char\": \"Ĕ\",
    \"latex\": \"\\\\u{E}\",
    \"math\": \"\\\\breve{E}\"
  },
  \"277\": {
    \"char\": \"ĕ\",
    \"latex\": \"\\\\u{e}\",
    \"math\": \"\\\\breve{e}\"
  },
  \"278\": {
    \"char\": \"Ė\",
    \"latex\": \"\\\\.{E}\",
    \"math\": \"\\\\dot{E}\"
  },
  \"279\": {
    \"char\": \"ė\",
    \"latex\": \"\\\\.{e}\",
    \"math\": \"\\\\dot{e}\"
  },
  \"280\": {
    \"char\": \"Ę\",
    \"latex\": \"\\\\k{E}\",
    \"math\": \"\\\\k{E}\"
  },
  \"281\": {
    \"char\": \"ę\",
    \"latex\": \"\\\\k{e}\",
    \"math\": null
  },
  \"282\": {
    \"char\": \"Ě\",
    \"latex\": \"\\\\v{E}\",
    \"math\": \"\\\\check{E}\"
  },
  \"283\": {
    \"char\": \"ě\",
    \"latex\": \"\\\\v{e}\",
    \"math\": \"\\\\check{e}\"
  },
  \"284\": {
    \"char\": \"Ĝ\",
    \"latex\": \"\\\\^{G}\",
    \"math\": \"\\\\hat{G}\"
  },
  \"285\": {
    \"char\": \"ĝ\",
    \"latex\": \"\\\\^{g}\",
    \"math\": \"\\\\hat{g}\"
  },
  \"286\": {
    \"char\": \"Ğ\",
    \"latex\": \"\\\\u{G}\",
    \"math\": \"\\\\breve{G}\"
  },
  \"287\": {
    \"char\": \"ğ\",
    \"latex\": \"\\\\u{g}\",
    \"math\": \"\\\\breve{g}\"
  },
  \"288\": {
    \"char\": \"Ġ\",
    \"latex\": \"\\\\.{G}\",
    \"math\": \"\\\\dot{G}\"
  },
  \"289\": {
    \"char\": \"ġ\",
    \"latex\": \"\\\\.{g}\",
    \"math\": \"\\\\dot{g}\"
  },
  \"290\": {
    \"char\": \"Ģ\",
    \"latex\": \"\\\\c{G}\",
    \"math\": \"\\\\mbox{\\\\c{G}}\"
  },
  \"291\": {
    \"char\": \"ģ\",
    \"latex\": \"\\\\c{g}\",
    \"math\": \"\\\\mbox{\\\\c{g}}\"
  },
  \"292\": {
    \"char\": \"Ĥ\",
    \"latex\": \"\\\\^{H}\",
    \"math\": \"\\\\hat{H}\"
  },
  \"293\": {
    \"char\": \"ĥ\",
    \"latex\": \"\\\\^{h}\",
    \"math\": \"\\\\hat{h}\"
  },
  \"294\": {
    \"char\": \"Ħ\",
    \"latex\": \"{\\\\fontencoding{LELA}\\\\selectfont\\\\char40}\",
    \"math\": null
  },
  \"295\": {
    \"char\": \"ħ\",
    \"latex\": \"\\\\Elzxh \",
    \"math\": null
  },
  \"296\": {
    \"char\": \"Ĩ\",
    \"latex\": \"\\\\~{I}\",
    \"math\": \"\\\\tilde{I}\"
  },
  \"297\": {
    \"char\": \"ĩ\",
    \"latex\": \"\\\\~{\\\\i}\",
    \"math\": \"\\\\tilde{\\\\imath}\"
  },
  \"298\": {
    \"char\": \"Ī\",
    \"latex\": \"\\\\={I}\",
    \"math\": \"\\\\bar{I}\"
  },
  \"299\": {
    \"char\": \"ī\",
    \"latex\": \"\\\\={\\\\i}\",
    \"math\": \"\\\\bar{\\\\imath}\"
  },
  \"300\": {
    \"char\": \"Ĭ\",
    \"latex\": \"\\\\u{I}\",
    \"math\": \"\\\\breve{I}\"
  },
  \"301\": {
    \"char\": \"ĭ\",
    \"latex\": \"\\\\u{\\\\i}\",
    \"math\": \"\\\\breve{\\\\imath}\"
  },
  \"302\": {
    \"char\": \"Į\",
    \"latex\": \"\\\\k{I}\",
    \"math\": null
  },
  \"303\": {
    \"char\": \"į\",
    \"latex\": \"\\\\k{i}\",
    \"math\": null
  },
  \"304\": {
    \"char\": \"İ\",
    \"latex\": \"\\\\.{I}\",
    \"math\": \"\\\\dot{I}\"
  },
  \"305\": {
    \"char\": \"ı\",
    \"latex\": \"\\\\i \",
    \"math\": \"\\\\imath \"
  },
  \"306\": {
    \"char\": \"Ĳ\",
    \"latex\": \"IJ\",
    \"math\": null
  },
  \"307\": {
    \"char\": \"ĳ\",
    \"latex\": \"ij\",
    \"math\": null
  },
  \"308\": {
    \"char\": \"Ĵ\",
    \"latex\": \"\\\\^{J}\",
    \"math\": \"\\\\hat{J}\"
  },
  \"309\": {
    \"char\": \"ĵ\",
    \"latex\": \"\\\\^{\\\\j}\",
    \"math\": \"\\\\hat{\\\\jmath}\"
  },
  \"310\": {
    \"char\": \"Ķ\",
    \"latex\": \"\\\\c{K}\",
    \"math\": \"\\\\mbox{\\\\c{K}}\"
  },
  \"311\": {
    \"char\": \"ķ\",
    \"latex\": \"\\\\c{k}\",
    \"math\": \"\\\\mbox{\\\\c{k}}\"
  },
  \"312\": {
    \"char\": \"ĸ\",
    \"latex\": \"{\\\\fontencoding{LELA}\\\\selectfont\\\\char91}\",
    \"math\": null
  },
  \"313\": {
    \"char\": \"Ĺ\",
    \"latex\": \"\\\\'{L}\",
    \"math\": \"\\\\acute{L}\"
  },
  \"314\": {
    \"char\": \"ĺ\",
    \"latex\": \"\\\\'{l}\",
    \"math\": \"\\\\acute{l}\"
  },
  \"315\": {
    \"char\": \"Ļ\",
    \"latex\": \"\\\\c{L}\",
    \"math\": \"\\\\mbox{\\\\c{L}}\"
  },
  \"316\": {
    \"char\": \"ļ\",
    \"latex\": \"\\\\c{l}\",
    \"math\": \"\\\\mbox{\\\\c{l}}\"
  },
  \"317\": {
    \"char\": \"Ľ\",
    \"latex\": \"\\\\v{L}\",
    \"math\": \"\\\\check{L}\"
  },
  \"318\": {
    \"char\": \"ľ\",
    \"latex\": \"\\\\v{l}\",
    \"math\": \"\\\\check{l}\"
  },
  \"319\": {
    \"char\": \"Ŀ\",
    \"latex\": \"{\\\\fontencoding{LELA}\\\\selectfont\\\\char201}\",
    \"math\": null
  },
  \"320\": {
    \"char\": \"ŀ\",
    \"latex\": \"{\\\\fontencoding{LELA}\\\\selectfont\\\\char202}\",
    \"math\": null
  },
  \"321\": {
    \"char\": \"Ł\",
    \"latex\": \"\\\\L \",
    \"math\": null
  },
  \"322\": {
    \"char\": \"ł\",
    \"latex\": \"\\\\l \",
    \"math\": null
  },
  \"323\": {
    \"char\": \"Ń\",
    \"latex\": \"\\\\'{N}\",
    \"math\": \"\\\\acute{N}\"
  },
  \"324\": {
    \"char\": \"ń\",
    \"latex\": \"\\\\'{n}\",
    \"math\": \"\\\\acute{n}\"
  },
  \"325\": {
    \"char\": \"Ņ\",
    \"latex\": \"\\\\c{N}\",
    \"math\": \"\\\\mbox{\\\\c{N}}\"
  },
  \"326\": {
    \"char\": \"ņ\",
    \"latex\": \"\\\\c{n}\",
    \"math\": \"\\\\mbox{\\\\c{n}}\"
  },
  \"327\": {
    \"char\": \"Ň\",
    \"latex\": \"\\\\v{N}\",
    \"math\": \"\\\\check{N}\"
  },
  \"328\": {
    \"char\": \"ň\",
    \"latex\": \"\\\\v{n}\",
    \"math\": \"\\\\check{n}\"
  },
  \"329\": {
    \"char\": \"ŉ\",
    \"latex\": \"'n\",
    \"math\": null
  },
  \"330\": {
    \"char\": \"Ŋ\",
    \"latex\": \"\\\\NG \",
    \"math\": null
  },
  \"331\": {
    \"char\": \"ŋ\",
    \"latex\": \"\\\\ng \",
    \"math\": null
  },
  \"332\": {
    \"char\": \"Ō\",
    \"latex\": \"\\\\={O}\",
    \"math\": \"\\\\bar{O}\"
  },
  \"333\": {
    \"char\": \"ō\",
    \"latex\": \"\\\\={o}\",
    \"math\": \"\\\\bar{o}\"
  },
  \"334\": {
    \"char\": \"Ŏ\",
    \"latex\": \"\\\\u{O}\",
    \"math\": \"\\\\breve{O}\"
  },
  \"335\": {
    \"char\": \"ŏ\",
    \"latex\": \"\\\\u{o}\",
    \"math\": \"\\\\breve{o}\"
  },
  \"336\": {
    \"char\": \"Ő\",
    \"latex\": \"\\\\H{O}\",
    \"math\": \"\\\\mbox{\\\\H{O}}\"
  },
  \"337\": {
    \"char\": \"ő\",
    \"latex\": \"\\\\H{o}\",
    \"math\": \"\\\\mbox{\\\\H{o}}\"
  },
  \"338\": {
    \"char\": \"Œ\",
    \"latex\": \"\\\\OE \",
    \"math\": null
  },
  \"339\": {
    \"char\": \"œ\",
    \"latex\": \"\\\\oe \",
    \"math\": null
  },
  \"340\": {
    \"char\": \"Ŕ\",
    \"latex\": \"\\\\'{R}\",
    \"math\": \"\\\\acute{R}\"
  },
  \"341\": {
    \"char\": \"ŕ\",
    \"latex\": \"\\\\'{r}\",
    \"math\": \"\\\\acute{r}\"
  },
  \"342\": {
    \"char\": \"Ŗ\",
    \"latex\": \"\\\\c{R}\",
    \"math\": \"\\\\mbox{\\\\c{R}}\"
  },
  \"343\": {
    \"char\": \"ŗ\",
    \"latex\": \"\\\\c{r}\",
    \"math\": \"\\\\mbox{\\\\c{r}}\"
  },
  \"344\": {
    \"char\": \"Ř\",
    \"latex\": \"\\\\v{R}\",
    \"math\": \"\\\\check{R}\"
  },
  \"345\": {
    \"char\": \"ř\",
    \"latex\": \"\\\\v{r}\",
    \"math\": \"\\\\check{r}\"
  },
  \"346\": {
    \"char\": \"Ś\",
    \"latex\": \"\\\\'{S}\",
    \"math\": \"\\\\acute{S}\"
  },
  \"347\": {
    \"char\": \"ś\",
    \"latex\": \"\\\\'{s}\",
    \"math\": \"\\\\acute{s}\"
  },
  \"348\": {
    \"char\": \"Ŝ\",
    \"latex\": \"\\\\^{S}\",
    \"math\": \"\\\\hat{S}\"
  },
  \"349\": {
    \"char\": \"ŝ\",
    \"latex\": \"\\\\^{s}\",
    \"math\": \"\\\\hat{s}\"
  },
  \"350\": {
    \"char\": \"Ş\",
    \"latex\": \"\\\\c{S}\",
    \"math\": \"\\\\mbox{\\\\c{S}}\"
  },
  \"351\": {
    \"char\": \"ş\",
    \"latex\": \"\\\\c{s}\",
    \"math\": \"\\\\mbox{\\\\c{s}}\"
  },
  \"352\": {
    \"char\": \"Š\",
    \"latex\": \"\\\\v{S}\",
    \"math\": \"\\\\check{S}\"
  },
  \"353\": {
    \"char\": \"š\",
    \"latex\": \"\\\\v{s}\",
    \"math\": \"\\\\check{s}\"
  },
  \"354\": {
    \"char\": \"Ţ\",
    \"latex\": \"\\\\c{T}\",
    \"math\": \"\\\\mbox{\\\\c{T}}\"
  },
  \"355\": {
    \"char\": \"ţ\",
    \"latex\": \"\\\\c{t}\",
    \"math\": \"\\\\mbox{\\\\c{t}}\"
  },
  \"356\": {
    \"char\": \"Ť\",
    \"latex\": \"\\\\v{T}\",
    \"math\": \"\\\\check{T}\"
  },
  \"357\": {
    \"char\": \"ť\",
    \"latex\": \"\\\\v{t}\",
    \"math\": \"\\\\check{t}\"
  },
  \"358\": {
    \"char\": \"Ŧ\",
    \"latex\": \"{\\\\fontencoding{LELA}\\\\selectfont\\\\char47}\",
    \"math\": null
  },
  \"359\": {
    \"char\": \"ŧ\",
    \"latex\": \"{\\\\fontencoding{LELA}\\\\selectfont\\\\char63}\",
    \"math\": null
  },
  \"360\": {
    \"char\": \"Ũ\",
    \"latex\": \"\\\\~{U}\",
    \"math\": \"\\\\tilde{U}\"
  },
  \"361\": {
    \"char\": \"ũ\",
    \"latex\": \"\\\\~{u}\",
    \"math\": \"\\\\tilde{u}\"
  },
  \"362\": {
    \"char\": \"Ū\",
    \"latex\": \"\\\\={U}\",
    \"math\": \"\\\\bar{U}\"
  },
  \"363\": {
    \"char\": \"ū\",
    \"latex\": \"\\\\={u}\",
    \"math\": \"\\\\bar{u}\"
  },
  \"364\": {
    \"char\": \"Ŭ\",
    \"latex\": \"\\\\u{U}\",
    \"math\": \"\\\\breve{U}\"
  },
  \"365\": {
    \"char\": \"ŭ\",
    \"latex\": \"\\\\u{u}\",
    \"math\": \"\\\\breve{u}\"
  },
  \"366\": {
    \"char\": \"Ů\",
    \"latex\": \"\\\\r{U}\",
    \"math\": \"\\\\mathring{U}\"
  },
  \"367\": {
    \"char\": \"ů\",
    \"latex\": \"\\\\r{u}\",
    \"math\": \"\\\\mathring{u}\"
  },
  \"368\": {
    \"char\": \"Ű\",
    \"latex\": \"\\\\H{U}\",
    \"math\": \"\\\\mbox{\\\\H{U}}\"
  },
  \"369\": {
    \"char\": \"ű\",
    \"latex\": \"\\\\H{u}\",
    \"math\": \"\\\\mbox{\\\\H{u}}\"
  },
  \"370\": {
    \"char\": \"Ų\",
    \"latex\": \"\\\\k{U}\",
    \"math\": \"\\\\k{U}\"
  },
  \"371\": {
    \"char\": \"ų\",
    \"latex\": \"\\\\k{u}\",
    \"math\": null
  },
  \"372\": {
    \"char\": \"Ŵ\",
    \"latex\": \"\\\\^{W}\",
    \"math\": \"\\\\hat{W}\"
  },
  \"373\": {
    \"char\": \"ŵ\",
    \"latex\": \"\\\\^{w}\",
    \"math\": \"\\\\hat{w}\"
  },
  \"374\": {
    \"char\": \"Ŷ\",
    \"latex\": \"\\\\^{Y}\",
    \"math\": \"\\\\hat{Y}\"
  },
  \"375\": {
    \"char\": \"ŷ\",
    \"latex\": \"\\\\^{y}\",
    \"math\": \"\\\\hat{y}\"
  },
  \"376\": {
    \"char\": \"Ÿ\",
    \"latex\": \"\\\\\\\"{Y}\",
    \"math\": \"\\\\ddot{Y}\"
  },
  \"377\": {
    \"char\": \"Ź\",
    \"latex\": \"\\\\'{Z}\",
    \"math\": \"\\\\acute{Z}\"
  },
  \"378\": {
    \"char\": \"ź\",
    \"latex\": \"\\\\'{z}\",
    \"math\": \"\\\\acute{z}\"
  },
  \"379\": {
    \"char\": \"Ż\",
    \"latex\": \"\\\\.{Z}\",
    \"math\": \"\\\\dot{Z}\"
  },
  \"380\": {
    \"char\": \"ż\",
    \"latex\": \"\\\\.{z}\",
    \"math\": \"\\\\dot{z}\"
  },
  \"381\": {
    \"char\": \"Ž\",
    \"latex\": \"\\\\v{Z}\",
    \"math\": \"\\\\check{Z}\"
  },
  \"382\": {
    \"char\": \"ž\",
    \"latex\": \"\\\\v{z}\",
    \"math\": \"\\\\check{z}\"
  },
  \"402\": {
    \"char\": \"ƒ\",
    \"latex\": \"f\",
    \"math\": null
  },
  \"405\": {
    \"char\": \"ƕ\",
    \"latex\": \"\\\\texthvlig \",
    \"math\": null
  },
  \"414\": {
    \"char\": \"ƞ\",
    \"latex\": \"\\\\textnrleg \",
    \"math\": null
  },
  \"426\": {
    \"char\": \"ƪ\",
    \"latex\": \"\\\\eth \",
    \"math\": null
  },
  \"442\": {
    \"char\": \"ƺ\",
    \"latex\": \"{\\\\fontencoding{LELA}\\\\selectfont\\\\char195}\",
    \"math\": null
  },
  \"450\": {
    \"char\": \"ǂ\",
    \"latex\": \"\\\\textdoublepipe \",
    \"math\": null
  },
  \"501\": {
    \"char\": \"ǵ\",
    \"latex\": \"\\\\'{g}\",
    \"math\": \"\\\\acute{g}\"
  },
  \"592\": {
    \"char\": \"ɐ\",
    \"latex\": \"\\\\Elztrna \",
    \"math\": null
  },
  \"594\": {
    \"char\": \"ɒ\",
    \"latex\": \"\\\\Elztrnsa \",
    \"math\": null
  },
  \"596\": {
    \"char\": \"ɔ\",
    \"latex\": \"\\\\Elzopeno \",
    \"math\": null
  },
  \"598\": {
    \"char\": \"ɖ\",
    \"latex\": \"\\\\Elzrtld \",
    \"math\": null
  },
  \"600\": {
    \"char\": \"ɘ\",
    \"latex\": \"{\\\\fontencoding{LEIP}\\\\selectfont\\\\char61}\",
    \"math\": null
  },
  \"601\": {
    \"char\": \"ə\",
    \"latex\": \"\\\\Elzschwa \",
    \"math\": null
  },
  \"603\": {
    \"char\": \"ɛ\",
    \"latex\": \"\\\\varepsilon \",
    \"math\": null
  },
  \"609\": {
    \"char\": \"ɡ\",
    \"latex\": \"g\",
    \"math\": null
  },
  \"611\": {
    \"char\": \"ɣ\",
    \"latex\": \"\\\\Elzpgamma \",
    \"math\": null
  },
  \"612\": {
    \"char\": \"ɤ\",
    \"latex\": \"\\\\Elzpbgam \",
    \"math\": null
  },
  \"613\": {
    \"char\": \"ɥ\",
    \"latex\": \"\\\\Elztrnh \",
    \"math\": null
  },
  \"620\": {
    \"char\": \"ɬ\",
    \"latex\": \"\\\\Elzbtdl \",
    \"math\": null
  },
  \"621\": {
    \"char\": \"ɭ\",
    \"latex\": \"\\\\Elzrtll \",
    \"math\": null
  },
  \"623\": {
    \"char\": \"ɯ\",
    \"latex\": \"\\\\Elztrnm \",
    \"math\": null
  },
  \"624\": {
    \"char\": \"ɰ\",
    \"latex\": \"\\\\Elztrnmlr \",
    \"math\": null
  },
  \"625\": {
    \"char\": \"ɱ\",
    \"latex\": \"\\\\Elzltlmr \",
    \"math\": null
  },
  \"626\": {
    \"char\": \"ɲ\",
    \"latex\": \"\\\\Elzltln \",
    \"math\": null
  },
  \"627\": {
    \"char\": \"ɳ\",
    \"latex\": \"\\\\Elzrtln \",
    \"math\": null
  },
  \"631\": {
    \"char\": \"ɷ\",
    \"latex\": \"\\\\Elzclomeg \",
    \"math\": null
  },
  \"632\": {
    \"char\": \"ɸ\",
    \"latex\": \"\\\\textphi \",
    \"math\": null
  },
  \"633\": {
    \"char\": \"ɹ\",
    \"latex\": \"\\\\Elztrnr \",
    \"math\": null
  },
  \"634\": {
    \"char\": \"ɺ\",
    \"latex\": \"\\\\Elztrnrl \",
    \"math\": null
  },
  \"635\": {
    \"char\": \"ɻ\",
    \"latex\": \"\\\\Elzrttrnr \",
    \"math\": null
  },
  \"636\": {
    \"char\": \"ɼ\",
    \"latex\": \"\\\\Elzrl \",
    \"math\": null
  },
  \"637\": {
    \"char\": \"ɽ\",
    \"latex\": \"\\\\Elzrtlr \",
    \"math\": null
  },
  \"638\": {
    \"char\": \"ɾ\",
    \"latex\": \"\\\\Elzfhr \",
    \"math\": null
  },
  \"639\": {
    \"char\": \"ɿ\",
    \"latex\": \"{\\\\fontencoding{LEIP}\\\\selectfont\\\\char202}\",
    \"math\": null
  },
  \"642\": {
    \"char\": \"ʂ\",
    \"latex\": \"\\\\Elzrtls \",
    \"math\": null
  },
  \"643\": {
    \"char\": \"ʃ\",
    \"latex\": \"\\\\Elzesh \",
    \"math\": null
  },
  \"647\": {
    \"char\": \"ʇ\",
    \"latex\": \"\\\\Elztrnt \",
    \"math\": null
  },
  \"648\": {
    \"char\": \"ʈ\",
    \"latex\": \"\\\\Elzrtlt \",
    \"math\": null
  },
  \"650\": {
    \"char\": \"ʊ\",
    \"latex\": \"\\\\Elzpupsil \",
    \"math\": null
  },
  \"651\": {
    \"char\": \"ʋ\",
    \"latex\": \"\\\\Elzpscrv \",
    \"math\": null
  },
  \"652\": {
    \"char\": \"ʌ\",
    \"latex\": \"\\\\Elzinvv \",
    \"math\": null
  },
  \"653\": {
    \"char\": \"ʍ\",
    \"latex\": \"\\\\Elzinvw \",
    \"math\": null
  },
  \"654\": {
    \"char\": \"ʎ\",
    \"latex\": \"\\\\Elztrny \",
    \"math\": null
  },
  \"656\": {
    \"char\": \"ʐ\",
    \"latex\": \"\\\\Elzrtlz \",
    \"math\": null
  },
  \"658\": {
    \"char\": \"ʒ\",
    \"latex\": \"\\\\Elzyogh \",
    \"math\": null
  },
  \"660\": {
    \"char\": \"ʔ\",
    \"latex\": \"\\\\Elzglst \",
    \"math\": null
  },
  \"661\": {
    \"char\": \"ʕ\",
    \"latex\": \"\\\\Elzreglst \",
    \"math\": null
  },
  \"662\": {
    \"char\": \"ʖ\",
    \"latex\": \"\\\\Elzinglst \",
    \"math\": null
  },
  \"670\": {
    \"char\": \"ʞ\",
    \"latex\": \"\\\\textturnk \",
    \"math\": null
  },
  \"676\": {
    \"char\": \"ʤ\",
    \"latex\": \"\\\\Elzdyogh \",
    \"math\": null
  },
  \"679\": {
    \"char\": \"ʧ\",
    \"latex\": \"\\\\Elztesh \",
    \"math\": null
  },
  \"700\": {
    \"char\": \"ʼ\",
    \"latex\": \"'\",
    \"math\": null
  },
  \"711\": {
    \"char\": \"ˇ\",
    \"latex\": \"\\\\textasciicaron \",
    \"math\": null
  },
  \"712\": {
    \"char\": \"ˈ\",
    \"latex\": \"\\\\Elzverts \",
    \"math\": null
  },
  \"716\": {
    \"char\": \"ˌ\",
    \"latex\": \"\\\\Elzverti \",
    \"math\": null
  },
  \"720\": {
    \"char\": \"ː\",
    \"latex\": \"\\\\Elzlmrk \",
    \"math\": null
  },
  \"721\": {
    \"char\": \"ˑ\",
    \"latex\": \"\\\\Elzhlmrk \",
    \"math\": null
  },
  \"722\": {
    \"char\": \"˒\",
    \"latex\": \"\\\\Elzsbrhr \",
    \"math\": null
  },
  \"723\": {
    \"char\": \"˓\",
    \"latex\": \"\\\\Elzsblhr \",
    \"math\": null
  },
  \"724\": {
    \"char\": \"˔\",
    \"latex\": \"\\\\Elzrais \",
    \"math\": null
  },
  \"725\": {
    \"char\": \"˕\",
    \"latex\": \"\\\\Elzlow \",
    \"math\": null
  },
  \"728\": {
    \"char\": \"˘\",
    \"latex\": \"\\\\textasciibreve \",
    \"math\": \"\\\\u\"
  },
  \"729\": {
    \"char\": \"˙\",
    \"latex\": \"\\\\textperiodcentered \",
    \"math\": \"\\\\dot{}\"
  },
  \"730\": {
    \"char\": \"˚\",
    \"latex\": \"\\\\r{}\",
    \"math\": \"\\\\mathring{}\"
  },
  \"731\": {
    \"char\": \"˛\",
    \"latex\": \"\\\\k{}\",
    \"math\": \"\\\\mbox{\\\\k{}}\"
  },
  \"732\": {
    \"char\": \"˜\",
    \"latex\": \"\\\\texttildelow \",
    \"math\": null
  },
  \"733\": {
    \"char\": \"˝\",
    \"latex\": \"\\\\H{}\",
    \"math\": \"\\\\mbox{\\\\H{}}\"
  },
  \"741\": {
    \"char\": \"˥\",
    \"latex\": \"\\\\tone{55}\",
    \"math\": null
  },
  \"742\": {
    \"char\": \"˦\",
    \"latex\": \"\\\\tone{44}\",
    \"math\": null
  },
  \"743\": {
    \"char\": \"˧\",
    \"latex\": \"\\\\tone{33}\",
    \"math\": null
  },
  \"744\": {
    \"char\": \"˨\",
    \"latex\": \"\\\\tone{22}\",
    \"math\": null
  },
  \"745\": {
    \"char\": \"˩\",
    \"latex\": \"\\\\tone{11}\",
    \"math\": null
  },
  \"768\": {
    \"char\": \"̀\",
    \"latex\": \"\\\\`\",
    \"math\": \"\\\\grave\"
  },
  \"769\": {
    \"char\": \"́\",
    \"latex\": \"\\\\'\",
    \"math\": \"\\\\acute\"
  },
  \"770\": {
    \"char\": \"̂\",
    \"latex\": \"\\\\^\",
    \"math\": \"\\\\hat\"
  },
  \"771\": {
    \"char\": \"̃\",
    \"latex\": \"\\\\~\",
    \"math\": \"\\\\tilde\"
  },
  \"772\": {
    \"char\": \"̄\",
    \"latex\": \"\\\\=\",
    \"math\": \"\\\\bar\"
  },
  \"774\": {
    \"char\": \"̆\",
    \"latex\": \"\\\\u\",
    \"math\": \"\\\\breve\"
  },
  \"775\": {
    \"char\": \"̇\",
    \"latex\": \"\\\\.\",
    \"math\": \"\\\\dot\"
  },
  \"776\": {
    \"char\": \"̈\",
    \"latex\": \"\\\\\\\"\",
    \"math\": \"\\\\ddot\"
  },
  \"778\": {
    \"char\": \"̊\",
    \"latex\": \"\\\\r\",
    \"math\": null
  },
  \"779\": {
    \"char\": \"̋\",
    \"latex\": \"\\\\H\",
    \"math\": null
  },
  \"780\": {
    \"char\": \"̌\",
    \"latex\": \"\\\\v\",
    \"math\": \"\\\\check\"
  },
  \"783\": {
    \"char\": \"̏\",
    \"latex\": \"\\\\cyrchar\\\\C\",
    \"math\": null
  },
  \"785\": {
    \"char\": \"̑\",
    \"latex\": \"{\\\\fontencoding{LECO}\\\\selectfont\\\\char177}\",
    \"math\": null
  },
  \"792\": {
    \"char\": \"̘\",
    \"latex\": \"{\\\\fontencoding{LECO}\\\\selectfont\\\\char184}\",
    \"math\": null
  },
  \"793\": {
    \"char\": \"̙\",
    \"latex\": \"{\\\\fontencoding{LECO}\\\\selectfont\\\\char185}\",
    \"math\": null
  },
  \"801\": {
    \"char\": \"̡\",
    \"latex\": \"\\\\Elzpalh \",
    \"math\": null
  },
  \"802\": {
    \"char\": \"̢\",
    \"latex\": \"\\\\Elzrh \",
    \"math\": null
  },
  \"807\": {
    \"char\": \"̧\",
    \"latex\": \"\\\\c\",
    \"math\": null
  },
  \"808\": {
    \"char\": \"̨\",
    \"latex\": \"\\\\k\",
    \"math\": null
  },
  \"810\": {
    \"char\": \"̪\",
    \"latex\": \"\\\\Elzsbbrg \",
    \"math\": null
  },
  \"811\": {
    \"char\": \"̫\",
    \"latex\": \"{\\\\fontencoding{LECO}\\\\selectfont\\\\char203}\",
    \"math\": null
  },
  \"815\": {
    \"char\": \"̯\",
    \"latex\": \"{\\\\fontencoding{LECO}\\\\selectfont\\\\char207}\",
    \"math\": null
  },
  \"821\": {
    \"char\": \"̵\",
    \"latex\": \"\\\\Elzxl \",
    \"math\": null
  },
  \"822\": {
    \"char\": \"̶\",
    \"latex\": \"\\\\Elzbar \",
    \"math\": null
  },
  \"823\": {
    \"char\": \"̷\",
    \"latex\": \"{\\\\fontencoding{LECO}\\\\selectfont\\\\char215}\",
    \"math\": null
  },
  \"824\": {
    \"char\": \"̸\",
    \"latex\": \"{\\\\fontencoding{LECO}\\\\selectfont\\\\char216}\",
    \"math\": null
  },
  \"826\": {
    \"char\": \"̺\",
    \"latex\": \"{\\\\fontencoding{LECO}\\\\selectfont\\\\char218}\",
    \"math\": null
  },
  \"827\": {
    \"char\": \"̻\",
    \"latex\": \"{\\\\fontencoding{LECO}\\\\selectfont\\\\char219}\",
    \"math\": null
  },
  \"828\": {
    \"char\": \"̼\",
    \"latex\": \"{\\\\fontencoding{LECO}\\\\selectfont\\\\char220}\",
    \"math\": null
  },
  \"829\": {
    \"char\": \"̽\",
    \"latex\": \"{\\\\fontencoding{LECO}\\\\selectfont\\\\char221}\",
    \"math\": null
  },
  \"865\": {
    \"char\": \"͡\",
    \"latex\": \"{\\\\fontencoding{LECO}\\\\selectfont\\\\char225}\",
    \"math\": null
  },
  \"902\": {
    \"char\": \"Ά\",
    \"latex\": \"\\\\'{A}\",
    \"math\": \"\\\\acute{A}\"
  },
  \"904\": {
    \"char\": \"Έ\",
    \"latex\": \"\\\\'{E}\",
    \"math\": \"\\\\acute{E}\"
  },
  \"905\": {
    \"char\": \"Ή\",
    \"latex\": \"\\\\'{H}\",
    \"math\": \"\\\\grave{H}\"
  },
  \"906\": {
    \"char\": \"Ί\",
    \"latex\": \"\\\\'{}{I}\",
    \"math\": \"\\\\mathrm{'I}\"
  },
  \"908\": {
    \"char\": \"Ό\",
    \"latex\": \"\\\\'{}O\",
    \"math\": \"\\\\mathrm{'O}\"
  },
  \"910\": {
    \"char\": \"Ύ\",
    \"latex\": \"\\\\mathrm{'Y}\",
    \"math\": null
  },
  \"911\": {
    \"char\": \"Ώ\",
    \"latex\": \"\\\\mathrm{'\\\\Omega}\",
    \"math\": null
  },
  \"912\": {
    \"char\": \"ΐ\",
    \"latex\": \"\\\\acute{\\\\ddot{\\\\iota}}\",
    \"math\": null
  },
  \"913\": {
    \"char\": \"Α\",
    \"latex\": \"\\\\Alpha \",
    \"math\": null
  },
  \"914\": {
    \"char\": \"Β\",
    \"latex\": \"\\\\Beta \",
    \"math\": null
  },
  \"915\": {
    \"char\": \"Γ\",
    \"latex\": \"\\\\Gamma \",
    \"math\": null
  },
  \"916\": {
    \"char\": \"Δ\",
    \"latex\": \"\\\\Delta \",
    \"math\": null
  },
  \"917\": {
    \"char\": \"Ε\",
    \"latex\": \"\\\\Epsilon \",
    \"math\": null
  },
  \"918\": {
    \"char\": \"Ζ\",
    \"latex\": \"\\\\Zeta \",
    \"math\": null
  },
  \"919\": {
    \"char\": \"Η\",
    \"latex\": \"\\\\Eta \",
    \"math\": null
  },
  \"920\": {
    \"char\": \"Θ\",
    \"latex\": \"\\\\Theta \",
    \"math\": null
  },
  \"921\": {
    \"char\": \"Ι\",
    \"latex\": \"\\\\Iota \",
    \"math\": null
  },
  \"922\": {
    \"char\": \"Κ\",
    \"latex\": \"\\\\Kappa \",
    \"math\": null
  },
  \"923\": {
    \"char\": \"Λ\",
    \"latex\": \"\\\\Lambda \",
    \"math\": null
  },
  \"924\": {
    \"char\": \"Μ\",
    \"latex\": \"M\",
    \"math\": null
  },
  \"925\": {
    \"char\": \"Ν\",
    \"latex\": \"N\",
    \"math\": null
  },
  \"926\": {
    \"char\": \"Ξ\",
    \"latex\": \"\\\\Xi \",
    \"math\": null
  },
  \"927\": {
    \"char\": \"Ο\",
    \"latex\": \"O\",
    \"math\": null
  },
  \"928\": {
    \"char\": \"Π\",
    \"latex\": \"\\\\Pi \",
    \"math\": null
  },
  \"929\": {
    \"char\": \"Ρ\",
    \"latex\": \"\\\\Rho \",
    \"math\": null
  },
  \"931\": {
    \"char\": \"Σ\",
    \"latex\": \"\\\\Sigma \",
    \"math\": null
  },
  \"932\": {
    \"char\": \"Τ\",
    \"latex\": \"\\\\Tau \",
    \"math\": null
  },
  \"933\": {
    \"char\": \"Υ\",
    \"latex\": \"\\\\Upsilon \",
    \"math\": null
  },
  \"934\": {
    \"char\": \"Φ\",
    \"latex\": \"\\\\Phi \",
    \"math\": null
  },
  \"935\": {
    \"char\": \"Χ\",
    \"latex\": \"\\\\Chi \",
    \"math\": null
  },
  \"936\": {
    \"char\": \"Ψ\",
    \"latex\": \"\\\\Psi \",
    \"math\": null
  },
  \"937\": {
    \"char\": \"Ω\",
    \"latex\": \"\\\\Omega \",
    \"math\": null
  },
  \"938\": {
    \"char\": \"Ϊ\",
    \"latex\": \"\\\\mathrm{\\\\ddot{I}}\",
    \"math\": null
  },
  \"939\": {
    \"char\": \"Ϋ\",
    \"latex\": \"\\\\mathrm{\\\\ddot{Y}}\",
    \"math\": null
  },
  \"940\": {
    \"char\": \"ά\",
    \"latex\": \"\\\\'{$\\\\alpha$}\",
    \"math\": \"\\\\acute{\\\\alpha}\"
  },
  \"941\": {
    \"char\": \"έ\",
    \"latex\": \"\\\\acute{\\\\epsilon}\",
    \"math\": null
  },
  \"942\": {
    \"char\": \"ή\",
    \"latex\": \"\\\\acute{\\\\eta}\",
    \"math\": null
  },
  \"943\": {
    \"char\": \"ί\",
    \"latex\": \"\\\\acute{\\\\iota}\",
    \"math\": null
  },
  \"944\": {
    \"char\": \"ΰ\",
    \"latex\": \"\\\\acute{\\\\ddot{\\\\upsilon}}\",
    \"math\": null
  },
  \"945\": {
    \"char\": \"α\",
    \"latex\": \"\\\\alpha \",
    \"math\": null
  },
  \"946\": {
    \"char\": \"β\",
    \"latex\": \"\\\\beta \",
    \"math\": null
  },
  \"947\": {
    \"char\": \"γ\",
    \"latex\": \"\\\\gamma \",
    \"math\": null
  },
  \"948\": {
    \"char\": \"δ\",
    \"latex\": \"\\\\delta \",
    \"math\": null
  },
  \"949\": {
    \"char\": \"ε\",
    \"latex\": \"\\\\epsilon \",
    \"math\": null
  },
  \"950\": {
    \"char\": \"ζ\",
    \"latex\": \"\\\\zeta \",
    \"math\": null
  },
  \"951\": {
    \"char\": \"η\",
    \"latex\": \"\\\\eta \",
    \"math\": null
  },
  \"952\": {
    \"char\": \"θ\",
    \"latex\": \"\\\\texttheta \",
    \"math\": \"\\\\theta \"
  },
  \"953\": {
    \"char\": \"ι\",
    \"latex\": \"\\\\iota \",
    \"math\": null
  },
  \"954\": {
    \"char\": \"κ\",
    \"latex\": \"\\\\kappa \",
    \"math\": null
  },
  \"955\": {
    \"char\": \"λ\",
    \"latex\": \"\\\\lambda \",
    \"math\": null
  },
  \"956\": {
    \"char\": \"μ\",
    \"latex\": \"\\\\mu \",
    \"math\": null
  },
  \"957\": {
    \"char\": \"ν\",
    \"latex\": \"\\\\nu \",
    \"math\": null
  },
  \"958\": {
    \"char\": \"ξ\",
    \"latex\": \"\\\\xi \",
    \"math\": null
  },
  \"959\": {
    \"char\": \"ο\",
    \"latex\": \"o\",
    \"math\": null
  },
  \"960\": {
    \"char\": \"π\",
    \"latex\": \"\\\\pi \",
    \"math\": null
  },
  \"961\": {
    \"char\": \"ρ\",
    \"latex\": \"\\\\rho \",
    \"math\": null
  },
  \"962\": {
    \"char\": \"ς\",
    \"latex\": \"\\\\varsigma \",
    \"math\": null
  },
  \"963\": {
    \"char\": \"σ\",
    \"latex\": \"\\\\sigma \",
    \"math\": null
  },
  \"964\": {
    \"char\": \"τ\",
    \"latex\": \"\\\\tau \",
    \"math\": null
  },
  \"965\": {
    \"char\": \"υ\",
    \"latex\": \"\\\\upsilon \",
    \"math\": null
  },
  \"966\": {
    \"char\": \"φ\",
    \"latex\": \"\\\\varphi \",
    \"math\": null
  },
  \"967\": {
    \"char\": \"χ\",
    \"latex\": \"\\\\chi \",
    \"math\": null
  },
  \"968\": {
    \"char\": \"ψ\",
    \"latex\": \"\\\\psi \",
    \"math\": null
  },
  \"969\": {
    \"char\": \"ω\",
    \"latex\": \"\\\\omega \",
    \"math\": null
  },
  \"970\": {
    \"char\": \"ϊ\",
    \"latex\": \"\\\\ddot{\\\\iota}\",
    \"math\": null
  },
  \"971\": {
    \"char\": \"ϋ\",
    \"latex\": \"\\\\ddot{\\\\upsilon}\",
    \"math\": null
  },
  \"972\": {
    \"char\": \"ό\",
    \"latex\": \"\\\\'{o}\",
    \"math\": \"\\\\acute{o}\"
  },
  \"973\": {
    \"char\": \"ύ\",
    \"latex\": \"\\\\acute{\\\\upsilon}\",
    \"math\": null
  },
  \"974\": {
    \"char\": \"ώ\",
    \"latex\": \"\\\\acute{\\\\omega}\",
    \"math\": null
  },
  \"976\": {
    \"char\": \"ϐ\",
    \"latex\": \"\\\\Pisymbol{ppi022}{87}\",
    \"math\": null
  },
  \"977\": {
    \"char\": \"ϑ\",
    \"latex\": \"\\\\textvartheta \",
    \"math\": \"\\\\vartheta \"
  },
  \"978\": {
    \"char\": \"ϒ\",
    \"latex\": \"\\\\Upsilon \",
    \"math\": null
  },
  \"981\": {
    \"char\": \"ϕ\",
    \"latex\": \"\\\\phi \",
    \"math\": null
  },
  \"982\": {
    \"char\": \"ϖ\",
    \"latex\": \"\\\\varpi \",
    \"math\": null
  },
  \"986\": {
    \"char\": \"Ϛ\",
    \"latex\": \"\\\\Stigma \",
    \"math\": null
  },
  \"988\": {
    \"char\": \"Ϝ\",
    \"latex\": \"\\\\Digamma \",
    \"math\": null
  },
  \"989\": {
    \"char\": \"ϝ\",
    \"latex\": \"\\\\digamma \",
    \"math\": null
  },
  \"990\": {
    \"char\": \"Ϟ\",
    \"latex\": \"\\\\Koppa \",
    \"math\": null
  },
  \"992\": {
    \"char\": \"Ϡ\",
    \"latex\": \"\\\\Sampi \",
    \"math\": null
  },
  \"1008\": {
    \"char\": \"ϰ\",
    \"latex\": \"\\\\varkappa \",
    \"math\": null
  },
  \"1009\": {
    \"char\": \"ϱ\",
    \"latex\": \"\\\\varrho \",
    \"math\": null
  },
  \"1012\": {
    \"char\": \"ϴ\",
    \"latex\": \"\\\\textTheta \",
    \"math\": null
  },
  \"1014\": {
    \"char\": \"϶\",
    \"latex\": \"\\\\backepsilon \",
    \"math\": null
  },
  \"1025\": {
    \"char\": \"Ё\",
    \"latex\": \"\\\\cyrchar\\\\CYRYO \",
    \"math\": null
  },
  \"1026\": {
    \"char\": \"Ђ\",
    \"latex\": \"\\\\cyrchar\\\\CYRDJE \",
    \"math\": null
  },
  \"1027\": {
    \"char\": \"Ѓ\",
    \"latex\": \"\\\\cyrchar{\\\\'\\\\CYRG}\",
    \"math\": null
  },
  \"1028\": {
    \"char\": \"Є\",
    \"latex\": \"\\\\cyrchar\\\\CYRIE \",
    \"math\": null
  },
  \"1029\": {
    \"char\": \"Ѕ\",
    \"latex\": \"\\\\cyrchar\\\\CYRDZE \",
    \"math\": null
  },
  \"1030\": {
    \"char\": \"І\",
    \"latex\": \"\\\\cyrchar\\\\CYRII \",
    \"math\": null
  },
  \"1031\": {
    \"char\": \"Ї\",
    \"latex\": \"\\\\cyrchar\\\\CYRYI \",
    \"math\": null
  },
  \"1032\": {
    \"char\": \"Ј\",
    \"latex\": \"\\\\cyrchar\\\\CYRJE \",
    \"math\": null
  },
  \"1033\": {
    \"char\": \"Љ\",
    \"latex\": \"\\\\cyrchar\\\\CYRLJE \",
    \"math\": null
  },
  \"1034\": {
    \"char\": \"Њ\",
    \"latex\": \"\\\\cyrchar\\\\CYRNJE \",
    \"math\": null
  },
  \"1035\": {
    \"char\": \"Ћ\",
    \"latex\": \"\\\\cyrchar\\\\CYRTSHE \",
    \"math\": null
  },
  \"1036\": {
    \"char\": \"Ќ\",
    \"latex\": \"\\\\cyrchar{\\\\'\\\\CYRK}\",
    \"math\": null
  },
  \"1038\": {
    \"char\": \"Ў\",
    \"latex\": \"\\\\cyrchar\\\\CYRUSHRT \",
    \"math\": null
  },
  \"1039\": {
    \"char\": \"Џ\",
    \"latex\": \"\\\\cyrchar\\\\CYRDZHE \",
    \"math\": null
  },
  \"1040\": {
    \"char\": \"А\",
    \"latex\": \"\\\\cyrchar\\\\CYRA \",
    \"math\": null
  },
  \"1041\": {
    \"char\": \"Б\",
    \"latex\": \"\\\\cyrchar\\\\CYRB \",
    \"math\": null
  },
  \"1042\": {
    \"char\": \"В\",
    \"latex\": \"\\\\cyrchar\\\\CYRV \",
    \"math\": null
  },
  \"1043\": {
    \"char\": \"Г\",
    \"latex\": \"\\\\cyrchar\\\\CYRG \",
    \"math\": null
  },
  \"1044\": {
    \"char\": \"Д\",
    \"latex\": \"\\\\cyrchar\\\\CYRD \",
    \"math\": null
  },
  \"1045\": {
    \"char\": \"Е\",
    \"latex\": \"\\\\cyrchar\\\\CYRE \",
    \"math\": null
  },
  \"1046\": {
    \"char\": \"Ж\",
    \"latex\": \"\\\\cyrchar\\\\CYRZH \",
    \"math\": null
  },
  \"1047\": {
    \"char\": \"З\",
    \"latex\": \"\\\\cyrchar\\\\CYRZ \",
    \"math\": null
  },
  \"1048\": {
    \"char\": \"И\",
    \"latex\": \"\\\\cyrchar\\\\CYRI \",
    \"math\": null
  },
  \"1049\": {
    \"char\": \"Й\",
    \"latex\": \"\\\\cyrchar\\\\CYRISHRT \",
    \"math\": null
  },
  \"1050\": {
    \"char\": \"К\",
    \"latex\": \"\\\\cyrchar\\\\CYRK \",
    \"math\": null
  },
  \"1051\": {
    \"char\": \"Л\",
    \"latex\": \"\\\\cyrchar\\\\CYRL \",
    \"math\": null
  },
  \"1052\": {
    \"char\": \"М\",
    \"latex\": \"\\\\cyrchar\\\\CYRM \",
    \"math\": null
  },
  \"1053\": {
    \"char\": \"Н\",
    \"latex\": \"\\\\cyrchar\\\\CYRN \",
    \"math\": null
  },
  \"1054\": {
    \"char\": \"О\",
    \"latex\": \"\\\\cyrchar\\\\CYRO \",
    \"math\": null
  },
  \"1055\": {
    \"char\": \"П\",
    \"latex\": \"\\\\cyrchar\\\\CYRP \",
    \"math\": null
  },
  \"1056\": {
    \"char\": \"Р\",
    \"latex\": \"\\\\cyrchar\\\\CYRR \",
    \"math\": null
  },
  \"1057\": {
    \"char\": \"С\",
    \"latex\": \"\\\\cyrchar\\\\CYRS \",
    \"math\": null
  },
  \"1058\": {
    \"char\": \"Т\",
    \"latex\": \"\\\\cyrchar\\\\CYRT \",
    \"math\": null
  },
  \"1059\": {
    \"char\": \"У\",
    \"latex\": \"\\\\cyrchar\\\\CYRU \",
    \"math\": null
  },
  \"1060\": {
    \"char\": \"Ф\",
    \"latex\": \"\\\\cyrchar\\\\CYRF \",
    \"math\": null
  },
  \"1061\": {
    \"char\": \"Х\",
    \"latex\": \"\\\\cyrchar\\\\CYRH \",
    \"math\": null
  },
  \"1062\": {
    \"char\": \"Ц\",
    \"latex\": \"\\\\cyrchar\\\\CYRC \",
    \"math\": null
  },
  \"1063\": {
    \"char\": \"Ч\",
    \"latex\": \"\\\\cyrchar\\\\CYRCH \",
    \"math\": null
  },
  \"1064\": {
    \"char\": \"Ш\",
    \"latex\": \"\\\\cyrchar\\\\CYRSH \",
    \"math\": null
  },
  \"1065\": {
    \"char\": \"Щ\",
    \"latex\": \"\\\\cyrchar\\\\CYRSHCH \",
    \"math\": null
  },
  \"1066\": {
    \"char\": \"Ъ\",
    \"latex\": \"\\\\cyrchar\\\\CYRHRDSN \",
    \"math\": null
  },
  \"1067\": {
    \"char\": \"Ы\",
    \"latex\": \"\\\\cyrchar\\\\CYRERY \",
    \"math\": null
  },
  \"1068\": {
    \"char\": \"Ь\",
    \"latex\": \"\\\\cyrchar\\\\CYRSFTSN \",
    \"math\": null
  },
  \"1069\": {
    \"char\": \"Э\",
    \"latex\": \"\\\\cyrchar\\\\CYREREV \",
    \"math\": null
  },
  \"1070\": {
    \"char\": \"Ю\",
    \"latex\": \"\\\\cyrchar\\\\CYRYU \",
    \"math\": null
  },
  \"1071\": {
    \"char\": \"Я\",
    \"latex\": \"\\\\cyrchar\\\\CYRYA \",
    \"math\": null
  },
  \"1072\": {
    \"char\": \"а\",
    \"latex\": \"\\\\cyrchar\\\\cyra \",
    \"math\": null
  },
  \"1073\": {
    \"char\": \"б\",
    \"latex\": \"\\\\cyrchar\\\\cyrb \",
    \"math\": null
  },
  \"1074\": {
    \"char\": \"в\",
    \"latex\": \"\\\\cyrchar\\\\cyrv \",
    \"math\": null
  },
  \"1075\": {
    \"char\": \"г\",
    \"latex\": \"\\\\cyrchar\\\\cyrg \",
    \"math\": null
  },
  \"1076\": {
    \"char\": \"д\",
    \"latex\": \"\\\\cyrchar\\\\cyrd \",
    \"math\": null
  },
  \"1077\": {
    \"char\": \"е\",
    \"latex\": \"\\\\cyrchar\\\\cyre \",
    \"math\": null
  },
  \"1078\": {
    \"char\": \"ж\",
    \"latex\": \"\\\\cyrchar\\\\cyrzh \",
    \"math\": null
  },
  \"1079\": {
    \"char\": \"з\",
    \"latex\": \"\\\\cyrchar\\\\cyrz \",
    \"math\": null
  },
  \"1080\": {
    \"char\": \"и\",
    \"latex\": \"\\\\cyrchar\\\\cyri \",
    \"math\": null
  },
  \"1081\": {
    \"char\": \"й\",
    \"latex\": \"\\\\cyrchar\\\\cyrishrt \",
    \"math\": null
  },
  \"1082\": {
    \"char\": \"к\",
    \"latex\": \"\\\\cyrchar\\\\cyrk \",
    \"math\": null
  },
  \"1083\": {
    \"char\": \"л\",
    \"latex\": \"\\\\cyrchar\\\\cyrl \",
    \"math\": null
  },
  \"1084\": {
    \"char\": \"м\",
    \"latex\": \"\\\\cyrchar\\\\cyrm \",
    \"math\": null
  },
  \"1085\": {
    \"char\": \"н\",
    \"latex\": \"\\\\cyrchar\\\\cyrn \",
    \"math\": null
  },
  \"1086\": {
    \"char\": \"о\",
    \"latex\": \"\\\\cyrchar\\\\cyro \",
    \"math\": null
  },
  \"1087\": {
    \"char\": \"п\",
    \"latex\": \"\\\\cyrchar\\\\cyrp \",
    \"math\": null
  },
  \"1088\": {
    \"char\": \"р\",
    \"latex\": \"\\\\cyrchar\\\\cyrr \",
    \"math\": null
  },
  \"1089\": {
    \"char\": \"с\",
    \"latex\": \"\\\\cyrchar\\\\cyrs \",
    \"math\": null
  },
  \"1090\": {
    \"char\": \"т\",
    \"latex\": \"\\\\cyrchar\\\\cyrt \",
    \"math\": null
  },
  \"1091\": {
    \"char\": \"у\",
    \"latex\": \"\\\\cyrchar\\\\cyru \",
    \"math\": null
  },
  \"1092\": {
    \"char\": \"ф\",
    \"latex\": \"\\\\cyrchar\\\\cyrf \",
    \"math\": null
  },
  \"1093\": {
    \"char\": \"х\",
    \"latex\": \"\\\\cyrchar\\\\cyrh \",
    \"math\": null
  },
  \"1094\": {
    \"char\": \"ц\",
    \"latex\": \"\\\\cyrchar\\\\cyrc \",
    \"math\": null
  },
  \"1095\": {
    \"char\": \"ч\",
    \"latex\": \"\\\\cyrchar\\\\cyrch \",
    \"math\": null
  },
  \"1096\": {
    \"char\": \"ш\",
    \"latex\": \"\\\\cyrchar\\\\cyrsh \",
    \"math\": null
  },
  \"1097\": {
    \"char\": \"щ\",
    \"latex\": \"\\\\cyrchar\\\\cyrshch \",
    \"math\": null
  },
  \"1098\": {
    \"char\": \"ъ\",
    \"latex\": \"\\\\cyrchar\\\\cyrhrdsn \",
    \"math\": null
  },
  \"1099\": {
    \"char\": \"ы\",
    \"latex\": \"\\\\cyrchar\\\\cyrery \",
    \"math\": null
  },
  \"1100\": {
    \"char\": \"ь\",
    \"latex\": \"\\\\cyrchar\\\\cyrsftsn \",
    \"math\": null
  },
  \"1101\": {
    \"char\": \"э\",
    \"latex\": \"\\\\cyrchar\\\\cyrerev \",
    \"math\": null
  },
  \"1102\": {
    \"char\": \"ю\",
    \"latex\": \"\\\\cyrchar\\\\cyryu \",
    \"math\": null
  },
  \"1103\": {
    \"char\": \"я\",
    \"latex\": \"\\\\cyrchar\\\\cyrya \",
    \"math\": null
  },
  \"1105\": {
    \"char\": \"ё\",
    \"latex\": \"\\\\cyrchar\\\\cyryo \",
    \"math\": null
  },
  \"1106\": {
    \"char\": \"ђ\",
    \"latex\": \"\\\\cyrchar\\\\cyrdje \",
    \"math\": null
  },
  \"1107\": {
    \"char\": \"ѓ\",
    \"latex\": \"\\\\cyrchar{\\\\'\\\\cyrg}\",
    \"math\": null
  },
  \"1108\": {
    \"char\": \"є\",
    \"latex\": \"\\\\cyrchar\\\\cyrie \",
    \"math\": null
  },
  \"1109\": {
    \"char\": \"ѕ\",
    \"latex\": \"\\\\cyrchar\\\\cyrdze \",
    \"math\": null
  },
  \"1110\": {
    \"char\": \"і\",
    \"latex\": \"\\\\cyrchar\\\\cyrii \",
    \"math\": null
  },
  \"1111\": {
    \"char\": \"ї\",
    \"latex\": \"\\\\cyrchar\\\\cyryi \",
    \"math\": null
  },
  \"1112\": {
    \"char\": \"ј\",
    \"latex\": \"\\\\cyrchar\\\\cyrje \",
    \"math\": null
  },
  \"1113\": {
    \"char\": \"љ\",
    \"latex\": \"\\\\cyrchar\\\\cyrlje \",
    \"math\": null
  },
  \"1114\": {
    \"char\": \"њ\",
    \"latex\": \"\\\\cyrchar\\\\cyrnje \",
    \"math\": null
  },
  \"1115\": {
    \"char\": \"ћ\",
    \"latex\": \"\\\\cyrchar\\\\cyrtshe \",
    \"math\": null
  },
  \"1116\": {
    \"char\": \"ќ\",
    \"latex\": \"\\\\cyrchar{\\\\'\\\\cyrk}\",
    \"math\": null
  },
  \"1118\": {
    \"char\": \"ў\",
    \"latex\": \"\\\\cyrchar\\\\cyrushrt \",
    \"math\": null
  },
  \"1119\": {
    \"char\": \"џ\",
    \"latex\": \"\\\\cyrchar\\\\cyrdzhe \",
    \"math\": null
  },
  \"1120\": {
    \"char\": \"Ѡ\",
    \"latex\": \"\\\\cyrchar\\\\CYROMEGA \",
    \"math\": null
  },
  \"1121\": {
    \"char\": \"ѡ\",
    \"latex\": \"\\\\cyrchar\\\\cyromega \",
    \"math\": null
  },
  \"1122\": {
    \"char\": \"Ѣ\",
    \"latex\": \"\\\\cyrchar\\\\CYRYAT \",
    \"math\": null
  },
  \"1124\": {
    \"char\": \"Ѥ\",
    \"latex\": \"\\\\cyrchar\\\\CYRIOTE \",
    \"math\": null
  },
  \"1125\": {
    \"char\": \"ѥ\",
    \"latex\": \"\\\\cyrchar\\\\cyriote \",
    \"math\": null
  },
  \"1126\": {
    \"char\": \"Ѧ\",
    \"latex\": \"\\\\cyrchar\\\\CYRLYUS \",
    \"math\": null
  },
  \"1127\": {
    \"char\": \"ѧ\",
    \"latex\": \"\\\\cyrchar\\\\cyrlyus \",
    \"math\": null
  },
  \"1128\": {
    \"char\": \"Ѩ\",
    \"latex\": \"\\\\cyrchar\\\\CYRIOTLYUS \",
    \"math\": null
  },
  \"1129\": {
    \"char\": \"ѩ\",
    \"latex\": \"\\\\cyrchar\\\\cyriotlyus \",
    \"math\": null
  },
  \"1130\": {
    \"char\": \"Ѫ\",
    \"latex\": \"\\\\cyrchar\\\\CYRBYUS \",
    \"math\": null
  },
  \"1132\": {
    \"char\": \"Ѭ\",
    \"latex\": \"\\\\cyrchar\\\\CYRIOTBYUS \",
    \"math\": null
  },
  \"1133\": {
    \"char\": \"ѭ\",
    \"latex\": \"\\\\cyrchar\\\\cyriotbyus \",
    \"math\": null
  },
  \"1134\": {
    \"char\": \"Ѯ\",
    \"latex\": \"\\\\cyrchar\\\\CYRKSI \",
    \"math\": null
  },
  \"1135\": {
    \"char\": \"ѯ\",
    \"latex\": \"\\\\cyrchar\\\\cyrksi \",
    \"math\": null
  },
  \"1136\": {
    \"char\": \"Ѱ\",
    \"latex\": \"\\\\cyrchar\\\\CYRPSI \",
    \"math\": null
  },
  \"1137\": {
    \"char\": \"ѱ\",
    \"latex\": \"\\\\cyrchar\\\\cyrpsi \",
    \"math\": null
  },
  \"1138\": {
    \"char\": \"Ѳ\",
    \"latex\": \"\\\\cyrchar\\\\CYRFITA \",
    \"math\": null
  },
  \"1140\": {
    \"char\": \"Ѵ\",
    \"latex\": \"\\\\cyrchar\\\\CYRIZH \",
    \"math\": null
  },
  \"1144\": {
    \"char\": \"Ѹ\",
    \"latex\": \"\\\\cyrchar\\\\CYRUK \",
    \"math\": null
  },
  \"1145\": {
    \"char\": \"ѹ\",
    \"latex\": \"\\\\cyrchar\\\\cyruk \",
    \"math\": null
  },
  \"1146\": {
    \"char\": \"Ѻ\",
    \"latex\": \"\\\\cyrchar\\\\CYROMEGARND \",
    \"math\": null
  },
  \"1147\": {
    \"char\": \"ѻ\",
    \"latex\": \"\\\\cyrchar\\\\cyromegarnd \",
    \"math\": null
  },
  \"1148\": {
    \"char\": \"Ѽ\",
    \"latex\": \"\\\\cyrchar\\\\CYROMEGATITLO \",
    \"math\": null
  },
  \"1149\": {
    \"char\": \"ѽ\",
    \"latex\": \"\\\\cyrchar\\\\cyromegatitlo \",
    \"math\": null
  },
  \"1150\": {
    \"char\": \"Ѿ\",
    \"latex\": \"\\\\cyrchar\\\\CYROT \",
    \"math\": null
  },
  \"1151\": {
    \"char\": \"ѿ\",
    \"latex\": \"\\\\cyrchar\\\\cyrot \",
    \"math\": null
  },
  \"1152\": {
    \"char\": \"Ҁ\",
    \"latex\": \"\\\\cyrchar\\\\CYRKOPPA \",
    \"math\": null
  },
  \"1153\": {
    \"char\": \"ҁ\",
    \"latex\": \"\\\\cyrchar\\\\cyrkoppa \",
    \"math\": null
  },
  \"1154\": {
    \"char\": \"҂\",
    \"latex\": \"\\\\cyrchar\\\\cyrthousands \",
    \"math\": null
  },
  \"1160\": {
    \"char\": \"҈\",
    \"latex\": \"\\\\cyrchar\\\\cyrhundredthousands \",
    \"math\": null
  },
  \"1161\": {
    \"char\": \"҉\",
    \"latex\": \"\\\\cyrchar\\\\cyrmillions \",
    \"math\": null
  },
  \"1164\": {
    \"char\": \"Ҍ\",
    \"latex\": \"\\\\cyrchar\\\\CYRSEMISFTSN \",
    \"math\": null
  },
  \"1165\": {
    \"char\": \"ҍ\",
    \"latex\": \"\\\\cyrchar\\\\cyrsemisftsn \",
    \"math\": null
  },
  \"1166\": {
    \"char\": \"Ҏ\",
    \"latex\": \"\\\\cyrchar\\\\CYRRTICK \",
    \"math\": null
  },
  \"1167\": {
    \"char\": \"ҏ\",
    \"latex\": \"\\\\cyrchar\\\\cyrrtick \",
    \"math\": null
  },
  \"1168\": {
    \"char\": \"Ґ\",
    \"latex\": \"\\\\cyrchar\\\\CYRGUP \",
    \"math\": null
  },
  \"1169\": {
    \"char\": \"ґ\",
    \"latex\": \"\\\\cyrchar\\\\cyrgup \",
    \"math\": null
  },
  \"1170\": {
    \"char\": \"Ғ\",
    \"latex\": \"\\\\cyrchar\\\\CYRGHCRS \",
    \"math\": null
  },
  \"1171\": {
    \"char\": \"ғ\",
    \"latex\": \"\\\\cyrchar\\\\cyrghcrs \",
    \"math\": null
  },
  \"1172\": {
    \"char\": \"Ҕ\",
    \"latex\": \"\\\\cyrchar\\\\CYRGHK \",
    \"math\": null
  },
  \"1173\": {
    \"char\": \"ҕ\",
    \"latex\": \"\\\\cyrchar\\\\cyrghk \",
    \"math\": null
  },
  \"1174\": {
    \"char\": \"Җ\",
    \"latex\": \"\\\\cyrchar\\\\CYRZHDSC \",
    \"math\": null
  },
  \"1175\": {
    \"char\": \"җ\",
    \"latex\": \"\\\\cyrchar\\\\cyrzhdsc \",
    \"math\": null
  },
  \"1176\": {
    \"char\": \"Ҙ\",
    \"latex\": \"\\\\cyrchar\\\\CYRZDSC \",
    \"math\": null
  },
  \"1177\": {
    \"char\": \"ҙ\",
    \"latex\": \"\\\\cyrchar\\\\cyrzdsc \",
    \"math\": null
  },
  \"1178\": {
    \"char\": \"Қ\",
    \"latex\": \"\\\\cyrchar\\\\CYRKDSC \",
    \"math\": null
  },
  \"1179\": {
    \"char\": \"қ\",
    \"latex\": \"\\\\cyrchar\\\\cyrkdsc \",
    \"math\": null
  },
  \"1180\": {
    \"char\": \"Ҝ\",
    \"latex\": \"\\\\cyrchar\\\\CYRKVCRS \",
    \"math\": null
  },
  \"1181\": {
    \"char\": \"ҝ\",
    \"latex\": \"\\\\cyrchar\\\\cyrkvcrs \",
    \"math\": null
  },
  \"1182\": {
    \"char\": \"Ҟ\",
    \"latex\": \"\\\\cyrchar\\\\CYRKHCRS \",
    \"math\": null
  },
  \"1183\": {
    \"char\": \"ҟ\",
    \"latex\": \"\\\\cyrchar\\\\cyrkhcrs \",
    \"math\": null
  },
  \"1184\": {
    \"char\": \"Ҡ\",
    \"latex\": \"\\\\cyrchar\\\\CYRKBEAK \",
    \"math\": null
  },
  \"1185\": {
    \"char\": \"ҡ\",
    \"latex\": \"\\\\cyrchar\\\\cyrkbeak \",
    \"math\": null
  },
  \"1186\": {
    \"char\": \"Ң\",
    \"latex\": \"\\\\cyrchar\\\\CYRNDSC \",
    \"math\": null
  },
  \"1187\": {
    \"char\": \"ң\",
    \"latex\": \"\\\\cyrchar\\\\cyrndsc \",
    \"math\": null
  },
  \"1188\": {
    \"char\": \"Ҥ\",
    \"latex\": \"\\\\cyrchar\\\\CYRNG \",
    \"math\": null
  },
  \"1189\": {
    \"char\": \"ҥ\",
    \"latex\": \"\\\\cyrchar\\\\cyrng \",
    \"math\": null
  },
  \"1190\": {
    \"char\": \"Ҧ\",
    \"latex\": \"\\\\cyrchar\\\\CYRPHK \",
    \"math\": null
  },
  \"1191\": {
    \"char\": \"ҧ\",
    \"latex\": \"\\\\cyrchar\\\\cyrphk \",
    \"math\": null
  },
  \"1192\": {
    \"char\": \"Ҩ\",
    \"latex\": \"\\\\cyrchar\\\\CYRABHHA \",
    \"math\": null
  },
  \"1193\": {
    \"char\": \"ҩ\",
    \"latex\": \"\\\\cyrchar\\\\cyrabhha \",
    \"math\": null
  },
  \"1194\": {
    \"char\": \"Ҫ\",
    \"latex\": \"\\\\cyrchar\\\\CYRSDSC \",
    \"math\": null
  },
  \"1195\": {
    \"char\": \"ҫ\",
    \"latex\": \"\\\\cyrchar\\\\cyrsdsc \",
    \"math\": null
  },
  \"1196\": {
    \"char\": \"Ҭ\",
    \"latex\": \"\\\\cyrchar\\\\CYRTDSC \",
    \"math\": null
  },
  \"1197\": {
    \"char\": \"ҭ\",
    \"latex\": \"\\\\cyrchar\\\\cyrtdsc \",
    \"math\": null
  },
  \"1198\": {
    \"char\": \"Ү\",
    \"latex\": \"\\\\cyrchar\\\\CYRY \",
    \"math\": null
  },
  \"1199\": {
    \"char\": \"ү\",
    \"latex\": \"\\\\cyrchar\\\\cyry \",
    \"math\": null
  },
  \"1200\": {
    \"char\": \"Ұ\",
    \"latex\": \"\\\\cyrchar\\\\CYRYHCRS \",
    \"math\": null
  },
  \"1201\": {
    \"char\": \"ұ\",
    \"latex\": \"\\\\cyrchar\\\\cyryhcrs \",
    \"math\": null
  },
  \"1202\": {
    \"char\": \"Ҳ\",
    \"latex\": \"\\\\cyrchar\\\\CYRHDSC \",
    \"math\": null
  },
  \"1203\": {
    \"char\": \"ҳ\",
    \"latex\": \"\\\\cyrchar\\\\cyrhdsc \",
    \"math\": null
  },
  \"1204\": {
    \"char\": \"Ҵ\",
    \"latex\": \"\\\\cyrchar\\\\CYRTETSE \",
    \"math\": null
  },
  \"1205\": {
    \"char\": \"ҵ\",
    \"latex\": \"\\\\cyrchar\\\\cyrtetse \",
    \"math\": null
  },
  \"1206\": {
    \"char\": \"Ҷ\",
    \"latex\": \"\\\\cyrchar\\\\CYRCHRDSC \",
    \"math\": null
  },
  \"1207\": {
    \"char\": \"ҷ\",
    \"latex\": \"\\\\cyrchar\\\\cyrchrdsc \",
    \"math\": null
  },
  \"1208\": {
    \"char\": \"Ҹ\",
    \"latex\": \"\\\\cyrchar\\\\CYRCHVCRS \",
    \"math\": null
  },
  \"1209\": {
    \"char\": \"ҹ\",
    \"latex\": \"\\\\cyrchar\\\\cyrchvcrs \",
    \"math\": null
  },
  \"1210\": {
    \"char\": \"Һ\",
    \"latex\": \"\\\\cyrchar\\\\CYRSHHA \",
    \"math\": null
  },
  \"1211\": {
    \"char\": \"һ\",
    \"latex\": \"\\\\cyrchar\\\\cyrshha \",
    \"math\": null
  },
  \"1212\": {
    \"char\": \"Ҽ\",
    \"latex\": \"\\\\cyrchar\\\\CYRABHCH \",
    \"math\": null
  },
  \"1213\": {
    \"char\": \"ҽ\",
    \"latex\": \"\\\\cyrchar\\\\cyrabhch \",
    \"math\": null
  },
  \"1214\": {
    \"char\": \"Ҿ\",
    \"latex\": \"\\\\cyrchar\\\\CYRABHCHDSC \",
    \"math\": null
  },
  \"1215\": {
    \"char\": \"ҿ\",
    \"latex\": \"\\\\cyrchar\\\\cyrabhchdsc \",
    \"math\": null
  },
  \"1216\": {
    \"char\": \"Ӏ\",
    \"latex\": \"\\\\cyrchar\\\\CYRpalochka \",
    \"math\": null
  },
  \"1219\": {
    \"char\": \"Ӄ\",
    \"latex\": \"\\\\cyrchar\\\\CYRKHK \",
    \"math\": null
  },
  \"1220\": {
    \"char\": \"ӄ\",
    \"latex\": \"\\\\cyrchar\\\\cyrkhk \",
    \"math\": null
  },
  \"1223\": {
    \"char\": \"Ӈ\",
    \"latex\": \"\\\\cyrchar\\\\CYRNHK \",
    \"math\": null
  },
  \"1224\": {
    \"char\": \"ӈ\",
    \"latex\": \"\\\\cyrchar\\\\cyrnhk \",
    \"math\": null
  },
  \"1227\": {
    \"char\": \"Ӌ\",
    \"latex\": \"\\\\cyrchar\\\\CYRCHLDSC \",
    \"math\": null
  },
  \"1228\": {
    \"char\": \"ӌ\",
    \"latex\": \"\\\\cyrchar\\\\cyrchldsc \",
    \"math\": null
  },
  \"1236\": {
    \"char\": \"Ӕ\",
    \"latex\": \"\\\\cyrchar\\\\CYRAE \",
    \"math\": null
  },
  \"1237\": {
    \"char\": \"ӕ\",
    \"latex\": \"\\\\cyrchar\\\\cyrae \",
    \"math\": null
  },
  \"1240\": {
    \"char\": \"Ә\",
    \"latex\": \"\\\\cyrchar\\\\CYRSCHWA \",
    \"math\": null
  },
  \"1241\": {
    \"char\": \"ә\",
    \"latex\": \"\\\\cyrchar\\\\cyrschwa \",
    \"math\": null
  },
  \"1248\": {
    \"char\": \"Ӡ\",
    \"latex\": \"\\\\cyrchar\\\\CYRABHDZE \",
    \"math\": null
  },
  \"1249\": {
    \"char\": \"ӡ\",
    \"latex\": \"\\\\cyrchar\\\\cyrabhdze \",
    \"math\": null
  },
  \"1256\": {
    \"char\": \"Ө\",
    \"latex\": \"\\\\cyrchar\\\\CYROTLD \",
    \"math\": null
  },
  \"1257\": {
    \"char\": \"ө\",
    \"latex\": \"\\\\cyrchar\\\\cyrotld \",
    \"math\": null
  },
  \"8194\": {
    \"char\": \" \",
    \"latex\": \"\\\\hspace{0.6em}\",
    \"math\": null
  },
  \"8195\": {
    \"char\": \" \",
    \"latex\": \"\\\\hspace{1em}\",
    \"math\": null
  },
  \"8196\": {
    \"char\": \" \",
    \"latex\": \"\\\\hspace{0.33em}\",
    \"math\": null
  },
  \"8197\": {
    \"char\": \" \",
    \"latex\": \"\\\\hspace{0.25em}\",
    \"math\": null
  },
  \"8198\": {
    \"char\": \" \",
    \"latex\": \"\\\\hspace{0.166em}\",
    \"math\": null
  },
  \"8199\": {
    \"char\": \" \",
    \"latex\": \"\\\\hphantom{0}\",
    \"math\": null
  },
  \"8200\": {
    \"char\": \" \",
    \"latex\": \"\\\\hphantom{,}\",
    \"math\": null
  },
  \"8201\": {
    \"char\": \" \",
    \"latex\": \"\\\\hspace{0.167em}\",
    \"math\": null
  },
  \"8202\": {
    \"char\": \" \",
    \"latex\": \"\\\\mkern1mu \",
    \"math\": null
  },
  \"8208\": {
    \"char\": \"‐\",
    \"latex\": \"-\",
    \"math\": null
  },
  \"8211\": {
    \"char\": \"–\",
    \"latex\": \"\\\\textendash \",
    \"math\": null
  },
  \"8212\": {
    \"char\": \"—\",
    \"latex\": \"\\\\textemdash \",
    \"math\": \"---\"
  },
  \"8213\": {
    \"char\": \"―\",
    \"latex\": \"\\\\rule{1em}{1pt}\",
    \"math\": null
  },
  \"8214\": {
    \"char\": \"‖\",
    \"latex\": \"\\\\Vert \",
    \"math\": null
  },
  \"8216\": {
    \"char\": \"‘\",
    \"latex\": \"`\",
    \"math\": null
  },
  \"8217\": {
    \"char\": \"’\",
    \"latex\": \"'\",
    \"math\": null
  },
  \"8218\": {
    \"char\": \"‚\",
    \"latex\": \",\",
    \"math\": null
  },
  \"8219\": {
    \"char\": \"‛\",
    \"latex\": \"\\\\Elzreapos \",
    \"math\": null
  },
  \"8220\": {
    \"char\": \"“\",
    \"latex\": \"\\\\textquotedblleft \",
    \"math\": null
  },
  \"8221\": {
    \"char\": \"”\",
    \"latex\": \"\\\\textquotedblright \",
    \"math\": null
  },
  \"8222\": {
    \"char\": \"„\",
    \"latex\": \",,\",
    \"math\": null
  },
  \"8224\": {
    \"char\": \"†\",
    \"latex\": \"\\\\textdagger \",
    \"math\": \"\\\\dag \"
  },
  \"8225\": {
    \"char\": \"‡\",
    \"latex\": \"\\\\textdaggerdbl \",
    \"math\": \"\\\\ddag \"
  },
  \"8226\": {
    \"char\": \"•\",
    \"latex\": \"\\\\textbullet \",
    \"math\": \"\\\\bullet\"
  },
  \"8228\": {
    \"char\": \"․\",
    \"latex\": \".\",
    \"math\": null
  },
  \"8229\": {
    \"char\": \"‥\",
    \"latex\": \"..\",
    \"math\": null
  },
  \"8230\": {
    \"char\": \"…\",
    \"latex\": \"\\\\ldots \",
    \"math\": null
  },
  \"8240\": {
    \"char\": \"‰\",
    \"latex\": \"\\\\textperthousand \",
    \"math\": null
  },
  \"8241\": {
    \"char\": \"‱\",
    \"latex\": \"\\\\textpertenthousand \",
    \"math\": null
  },
  \"8242\": {
    \"char\": \"′\",
    \"latex\": \"{'}\",
    \"math\": null
  },
  \"8243\": {
    \"char\": \"″\",
    \"latex\": \"{''}\",
    \"math\": null
  },
  \"8244\": {
    \"char\": \"‴\",
    \"latex\": \"{'''}\",
    \"math\": null
  },
  \"8245\": {
    \"char\": \"‵\",
    \"latex\": \"\\\\backprime \",
    \"math\": null
  },
  \"8249\": {
    \"char\": \"‹\",
    \"latex\": \"\\\\guilsinglleft \",
    \"math\": null
  },
  \"8250\": {
    \"char\": \"›\",
    \"latex\": \"\\\\guilsinglright \",
    \"math\": null
  },
  \"8279\": {
    \"char\": \"⁗\",
    \"latex\": \"''''\",
    \"math\": null
  },
  \"8287\": {
    \"char\": \" \",
    \"latex\": \"\\\\mkern4mu \",
    \"math\": null
  },
  \"8288\": {
    \"char\": \"⁠\",
    \"latex\": \"\\\\nolinebreak \",
    \"math\": null
  },
  \"8359\": {
    \"char\": \"₧\",
    \"latex\": \"\\\\ensuremath{\\\\Elzpes}\",
    \"math\": \"\\\\Elzpes\"
  },
  \"8364\": {
    \"char\": \"€\",
    \"latex\": \"\\\\mbox{\\\\texteuro} \",
    \"math\": null
  },
  \"8411\": {
    \"char\": \"⃛\",
    \"latex\": \"\\\\dddot \",
    \"math\": null
  },
  \"8412\": {
    \"char\": \"⃜\",
    \"latex\": \"\\\\ddddot \",
    \"math\": null
  },
  \"8450\": {
    \"char\": \"ℂ\",
    \"latex\": \"\\\\mathbb{C}\",
    \"math\": null
  },
  \"8458\": {
    \"char\": \"ℊ\",
    \"latex\": \"\\\\mathscr{g}\",
    \"math\": null
  },
  \"8459\": {
    \"char\": \"ℋ\",
    \"latex\": \"\\\\mathscr{H}\",
    \"math\": null
  },
  \"8460\": {
    \"char\": \"ℌ\",
    \"latex\": \"\\\\mathfrak{H}\",
    \"math\": null
  },
  \"8461\": {
    \"char\": \"ℍ\",
    \"latex\": \"\\\\mathbb{H}\",
    \"math\": null
  },
  \"8463\": {
    \"char\": \"ℏ\",
    \"latex\": \"\\\\hslash \",
    \"math\": null
  },
  \"8464\": {
    \"char\": \"ℐ\",
    \"latex\": \"\\\\mathscr{I}\",
    \"math\": null
  },
  \"8465\": {
    \"char\": \"ℑ\",
    \"latex\": \"\\\\mathfrak{I}\",
    \"math\": null
  },
  \"8466\": {
    \"char\": \"ℒ\",
    \"latex\": \"\\\\mathscr{L}\",
    \"math\": null
  },
  \"8467\": {
    \"char\": \"ℓ\",
    \"latex\": \"\\\\mathscr{l}\",
    \"math\": null
  },
  \"8469\": {
    \"char\": \"ℕ\",
    \"latex\": \"\\\\mathbb{N}\",
    \"math\": null
  },
  \"8470\": {
    \"char\": \"№\",
    \"latex\": \"\\\\cyrchar\\\\textnumero \",
    \"math\": null
  },
  \"8472\": {
    \"char\": \"℘\",
    \"latex\": \"\\\\wp \",
    \"math\": null
  },
  \"8473\": {
    \"char\": \"ℙ\",
    \"latex\": \"\\\\mathbb{P}\",
    \"math\": null
  },
  \"8474\": {
    \"char\": \"ℚ\",
    \"latex\": \"\\\\mathbb{Q}\",
    \"math\": null
  },
  \"8475\": {
    \"char\": \"ℛ\",
    \"latex\": \"\\\\mathscr{R}\",
    \"math\": null
  },
  \"8476\": {
    \"char\": \"ℜ\",
    \"latex\": \"\\\\mathfrak{R}\",
    \"math\": null
  },
  \"8477\": {
    \"char\": \"ℝ\",
    \"latex\": \"\\\\mathbb{R}\",
    \"math\": null
  },
  \"8478\": {
    \"char\": \"℞\",
    \"latex\": \"\\\\Elzxrat \",
    \"math\": null
  },
  \"8482\": {
    \"char\": \"™\",
    \"latex\": \"\\\\texttrademark \",
    \"math\": null
  },
  \"8484\": {
    \"char\": \"ℤ\",
    \"latex\": \"\\\\mathbb{Z}\",
    \"math\": null
  },
  \"8486\": {
    \"char\": \"Ω\",
    \"latex\": \"\\\\Omega \",
    \"math\": null
  },
  \"8487\": {
    \"char\": \"℧\",
    \"latex\": \"\\\\mho \",
    \"math\": null
  },
  \"8488\": {
    \"char\": \"ℨ\",
    \"latex\": \"\\\\mathfrak{Z}\",
    \"math\": null
  },
  \"8489\": {
    \"char\": \"℩\",
    \"latex\": \"\\\\ElsevierGlyph{2129}\",
    \"math\": null
  },
  \"8491\": {
    \"char\": \"Å\",
    \"latex\": \"\\\\AA \",
    \"math\": null
  },
  \"8492\": {
    \"char\": \"ℬ\",
    \"latex\": \"\\\\mathscr{B}\",
    \"math\": null
  },
  \"8493\": {
    \"char\": \"ℭ\",
    \"latex\": \"\\\\mathfrak{C}\",
    \"math\": null
  },
  \"8495\": {
    \"char\": \"ℯ\",
    \"latex\": \"\\\\mathscr{e}\",
    \"math\": null
  },
  \"8496\": {
    \"char\": \"ℰ\",
    \"latex\": \"\\\\mathscr{E}\",
    \"math\": null
  },
  \"8497\": {
    \"char\": \"ℱ\",
    \"latex\": \"\\\\mathscr{F}\",
    \"math\": null
  },
  \"8499\": {
    \"char\": \"ℳ\",
    \"latex\": \"\\\\mathscr{M}\",
    \"math\": null
  },
  \"8500\": {
    \"char\": \"ℴ\",
    \"latex\": \"\\\\mathscr{o}\",
    \"math\": null
  },
  \"8501\": {
    \"char\": \"ℵ\",
    \"latex\": \"\\\\aleph \",
    \"math\": null
  },
  \"8502\": {
    \"char\": \"ℶ\",
    \"latex\": \"\\\\beth \",
    \"math\": null
  },
  \"8503\": {
    \"char\": \"ℷ\",
    \"latex\": \"\\\\gimel \",
    \"math\": null
  },
  \"8504\": {
    \"char\": \"ℸ\",
    \"latex\": \"\\\\daleth \",
    \"math\": null
  },
  \"8531\": {
    \"char\": \"⅓\",
    \"latex\": \"\\\\textfrac{1}{3}\",
    \"math\": null
  },
  \"8532\": {
    \"char\": \"⅔\",
    \"latex\": \"\\\\textfrac{2}{3}\",
    \"math\": null
  },
  \"8533\": {
    \"char\": \"⅕\",
    \"latex\": \"\\\\textfrac{1}{5}\",
    \"math\": null
  },
  \"8534\": {
    \"char\": \"⅖\",
    \"latex\": \"\\\\textfrac{2}{5}\",
    \"math\": null
  },
  \"8535\": {
    \"char\": \"⅗\",
    \"latex\": \"\\\\textfrac{3}{5}\",
    \"math\": null
  },
  \"8536\": {
    \"char\": \"⅘\",
    \"latex\": \"\\\\textfrac{4}{5}\",
    \"math\": null
  },
  \"8537\": {
    \"char\": \"⅙\",
    \"latex\": \"\\\\textfrac{1}{6}\",
    \"math\": null
  },
  \"8538\": {
    \"char\": \"⅚\",
    \"latex\": \"\\\\textfrac{5}{6}\",
    \"math\": null
  },
  \"8539\": {
    \"char\": \"⅛\",
    \"latex\": \"\\\\textfrac{1}{8}\",
    \"math\": null
  },
  \"8540\": {
    \"char\": \"⅜\",
    \"latex\": \"\\\\textfrac{3}{8}\",
    \"math\": null
  },
  \"8541\": {
    \"char\": \"⅝\",
    \"latex\": \"\\\\textfrac{5}{8}\",
    \"math\": null
  },
  \"8542\": {
    \"char\": \"⅞\",
    \"latex\": \"\\\\textfrac{7}{8}\",
    \"math\": null
  },
  \"8592\": {
    \"char\": \"←\",
    \"latex\": \"\\\\leftarrow \",
    \"math\": null
  },
  \"8593\": {
    \"char\": \"↑\",
    \"latex\": \"\\\\uparrow \",
    \"math\": null
  },
  \"8594\": {
    \"char\": \"→\",
    \"latex\": \"\\\\rightarrow \",
    \"math\": null
  },
  \"8595\": {
    \"char\": \"↓\",
    \"latex\": \"\\\\downarrow \",
    \"math\": null
  },
  \"8596\": {
    \"char\": \"↔\",
    \"latex\": \"\\\\leftrightarrow \",
    \"math\": null
  },
  \"8597\": {
    \"char\": \"↕\",
    \"latex\": \"\\\\updownarrow \",
    \"math\": null
  },
  \"8598\": {
    \"char\": \"↖\",
    \"latex\": \"\\\\nwarrow \",
    \"math\": null
  },
  \"8599\": {
    \"char\": \"↗\",
    \"latex\": \"\\\\nearrow \",
    \"math\": null
  },
  \"8600\": {
    \"char\": \"↘\",
    \"latex\": \"\\\\searrow \",
    \"math\": null
  },
  \"8601\": {
    \"char\": \"↙\",
    \"latex\": \"\\\\swarrow \",
    \"math\": null
  },
  \"8602\": {
    \"char\": \"↚\",
    \"latex\": \"\\\\nleftarrow \",
    \"math\": null
  },
  \"8603\": {
    \"char\": \"↛\",
    \"latex\": \"\\\\nrightarrow \",
    \"math\": null
  },
  \"8604\": {
    \"char\": \"↜\",
    \"latex\": \"\\\\arrowwaveright \",
    \"math\": null
  },
  \"8605\": {
    \"char\": \"↝\",
    \"latex\": \"\\\\arrowwaveright \",
    \"math\": null
  },
  \"8606\": {
    \"char\": \"↞\",
    \"latex\": \"\\\\twoheadleftarrow \",
    \"math\": null
  },
  \"8608\": {
    \"char\": \"↠\",
    \"latex\": \"\\\\twoheadrightarrow \",
    \"math\": null
  },
  \"8610\": {
    \"char\": \"↢\",
    \"latex\": \"\\\\leftarrowtail \",
    \"math\": null
  },
  \"8611\": {
    \"char\": \"↣\",
    \"latex\": \"\\\\rightarrowtail \",
    \"math\": null
  },
  \"8614\": {
    \"char\": \"↦\",
    \"latex\": \"\\\\mapsto \",
    \"math\": null
  },
  \"8617\": {
    \"char\": \"↩\",
    \"latex\": \"\\\\hookleftarrow \",
    \"math\": null
  },
  \"8618\": {
    \"char\": \"↪\",
    \"latex\": \"\\\\hookrightarrow \",
    \"math\": null
  },
  \"8619\": {
    \"char\": \"↫\",
    \"latex\": \"\\\\looparrowleft \",
    \"math\": null
  },
  \"8620\": {
    \"char\": \"↬\",
    \"latex\": \"\\\\looparrowright \",
    \"math\": null
  },
  \"8621\": {
    \"char\": \"↭\",
    \"latex\": \"\\\\leftrightsquigarrow \",
    \"math\": null
  },
  \"8622\": {
    \"char\": \"↮\",
    \"latex\": \"\\\\nleftrightarrow \",
    \"math\": null
  },
  \"8624\": {
    \"char\": \"↰\",
    \"latex\": \"\\\\Lsh \",
    \"math\": null
  },
  \"8625\": {
    \"char\": \"↱\",
    \"latex\": \"\\\\Rsh \",
    \"math\": null
  },
  \"8627\": {
    \"char\": \"↳\",
    \"latex\": \"\\\\ElsevierGlyph{21B3}\",
    \"math\": null
  },
  \"8630\": {
    \"char\": \"↶\",
    \"latex\": \"\\\\curvearrowleft \",
    \"math\": null
  },
  \"8631\": {
    \"char\": \"↷\",
    \"latex\": \"\\\\curvearrowright \",
    \"math\": null
  },
  \"8634\": {
    \"char\": \"↺\",
    \"latex\": \"\\\\circlearrowleft \",
    \"math\": null
  },
  \"8635\": {
    \"char\": \"↻\",
    \"latex\": \"\\\\circlearrowright \",
    \"math\": null
  },
  \"8636\": {
    \"char\": \"↼\",
    \"latex\": \"\\\\leftharpoonup \",
    \"math\": null
  },
  \"8637\": {
    \"char\": \"↽\",
    \"latex\": \"\\\\leftharpoondown \",
    \"math\": null
  },
  \"8638\": {
    \"char\": \"↾\",
    \"latex\": \"\\\\upharpoonright \",
    \"math\": null
  },
  \"8639\": {
    \"char\": \"↿\",
    \"latex\": \"\\\\upharpoonleft \",
    \"math\": null
  },
  \"8640\": {
    \"char\": \"⇀\",
    \"latex\": \"\\\\rightharpoonup \",
    \"math\": null
  },
  \"8641\": {
    \"char\": \"⇁\",
    \"latex\": \"\\\\rightharpoondown \",
    \"math\": null
  },
  \"8642\": {
    \"char\": \"⇂\",
    \"latex\": \"\\\\downharpoonright \",
    \"math\": null
  },
  \"8643\": {
    \"char\": \"⇃\",
    \"latex\": \"\\\\downharpoonleft \",
    \"math\": null
  },
  \"8644\": {
    \"char\": \"⇄\",
    \"latex\": \"\\\\rightleftarrows \",
    \"math\": null
  },
  \"8645\": {
    \"char\": \"⇅\",
    \"latex\": \"\\\\dblarrowupdown \",
    \"math\": null
  },
  \"8646\": {
    \"char\": \"⇆\",
    \"latex\": \"\\\\leftrightarrows \",
    \"math\": null
  },
  \"8647\": {
    \"char\": \"⇇\",
    \"latex\": \"\\\\leftleftarrows \",
    \"math\": null
  },
  \"8648\": {
    \"char\": \"⇈\",
    \"latex\": \"\\\\upuparrows \",
    \"math\": null
  },
  \"8649\": {
    \"char\": \"⇉\",
    \"latex\": \"\\\\rightrightarrows \",
    \"math\": null
  },
  \"8650\": {
    \"char\": \"⇊\",
    \"latex\": \"\\\\downdownarrows \",
    \"math\": null
  },
  \"8651\": {
    \"char\": \"⇋\",
    \"latex\": \"\\\\leftrightharpoons \",
    \"math\": null
  },
  \"8652\": {
    \"char\": \"⇌\",
    \"latex\": \"\\\\rightleftharpoons \",
    \"math\": null
  },
  \"8653\": {
    \"char\": \"⇍\",
    \"latex\": \"\\\\nLeftarrow \",
    \"math\": null
  },
  \"8654\": {
    \"char\": \"⇎\",
    \"latex\": \"\\\\nLeftrightarrow \",
    \"math\": null
  },
  \"8655\": {
    \"char\": \"⇏\",
    \"latex\": \"\\\\nRightarrow \",
    \"math\": null
  },
  \"8656\": {
    \"char\": \"⇐\",
    \"latex\": \"\\\\Leftarrow \",
    \"math\": null
  },
  \"8657\": {
    \"char\": \"⇑\",
    \"latex\": \"\\\\Uparrow \",
    \"math\": null
  },
  \"8658\": {
    \"char\": \"⇒\",
    \"latex\": \"\\\\Rightarrow \",
    \"math\": null
  },
  \"8659\": {
    \"char\": \"⇓\",
    \"latex\": \"\\\\Downarrow \",
    \"math\": null
  },
  \"8660\": {
    \"char\": \"⇔\",
    \"latex\": \"\\\\Leftrightarrow \",
    \"math\": null
  },
  \"8661\": {
    \"char\": \"⇕\",
    \"latex\": \"\\\\Updownarrow \",
    \"math\": null
  },
  \"8666\": {
    \"char\": \"⇚\",
    \"latex\": \"\\\\Lleftarrow \",
    \"math\": null
  },
  \"8667\": {
    \"char\": \"⇛\",
    \"latex\": \"\\\\Rrightarrow \",
    \"math\": null
  },
  \"8669\": {
    \"char\": \"⇝\",
    \"latex\": \"\\\\rightsquigarrow \",
    \"math\": null
  },
  \"8693\": {
    \"char\": \"⇵\",
    \"latex\": \"\\\\DownArrowUpArrow \",
    \"math\": null
  },
  \"8704\": {
    \"char\": \"∀\",
    \"latex\": \"\\\\forall \",
    \"math\": null
  },
  \"8705\": {
    \"char\": \"∁\",
    \"latex\": \"\\\\complement \",
    \"math\": null
  },
  \"8706\": {
    \"char\": \"∂\",
    \"latex\": \"\\\\partial \",
    \"math\": null
  },
  \"8707\": {
    \"char\": \"∃\",
    \"latex\": \"\\\\exists \",
    \"math\": null
  },
  \"8708\": {
    \"char\": \"∄\",
    \"latex\": \"\\\\nexists \",
    \"math\": null
  },
  \"8709\": {
    \"char\": \"∅\",
    \"latex\": \"\\\\varnothing \",
    \"math\": null
  },
  \"8711\": {
    \"char\": \"∇\",
    \"latex\": \"\\\\nabla \",
    \"math\": null
  },
  \"8712\": {
    \"char\": \"∈\",
    \"latex\": \"\\\\in \",
    \"math\": null
  },
  \"8713\": {
    \"char\": \"∉\",
    \"latex\": \"\\\\not\\\\in \",
    \"math\": null
  },
  \"8715\": {
    \"char\": \"∋\",
    \"latex\": \"\\\\ni \",
    \"math\": null
  },
  \"8716\": {
    \"char\": \"∌\",
    \"latex\": \"\\\\not\\\\ni \",
    \"math\": null
  },
  \"8719\": {
    \"char\": \"∏\",
    \"latex\": \"\\\\prod \",
    \"math\": null
  },
  \"8720\": {
    \"char\": \"∐\",
    \"latex\": \"\\\\coprod \",
    \"math\": null
  },
  \"8721\": {
    \"char\": \"∑\",
    \"latex\": \"\\\\sum \",
    \"math\": null
  },
  \"8722\": {
    \"char\": \"−\",
    \"latex\": \"-\",
    \"math\": null
  },
  \"8723\": {
    \"char\": \"∓\",
    \"latex\": \"\\\\mp \",
    \"math\": null
  },
  \"8724\": {
    \"char\": \"∔\",
    \"latex\": \"\\\\dotplus \",
    \"math\": null
  },
  \"8726\": {
    \"char\": \"∖\",
    \"latex\": \"\\\\setminus \",
    \"math\": null
  },
  \"8727\": {
    \"char\": \"∗\",
    \"latex\": \"{_\\\\ast}\",
    \"math\": null
  },
  \"8728\": {
    \"char\": \"∘\",
    \"latex\": \"\\\\circ \",
    \"math\": null
  },
  \"8729\": {
    \"char\": \"∙\",
    \"latex\": \"\\\\bullet \",
    \"math\": null
  },
  \"8730\": {
    \"char\": \"√\",
    \"latex\": \"\\\\surd \",
    \"math\": null
  },
  \"8733\": {
    \"char\": \"∝\",
    \"latex\": \"\\\\propto \",
    \"math\": null
  },
  \"8734\": {
    \"char\": \"∞\",
    \"latex\": \"\\\\infty \",
    \"math\": null
  },
  \"8735\": {
    \"char\": \"∟\",
    \"latex\": \"\\\\rightangle \",
    \"math\": null
  },
  \"8736\": {
    \"char\": \"∠\",
    \"latex\": \"\\\\angle \",
    \"math\": null
  },
  \"8737\": {
    \"char\": \"∡\",
    \"latex\": \"\\\\measuredangle \",
    \"math\": null
  },
  \"8738\": {
    \"char\": \"∢\",
    \"latex\": \"\\\\sphericalangle \",
    \"math\": null
  },
  \"8739\": {
    \"char\": \"∣\",
    \"latex\": \"\\\\mid \",
    \"math\": null
  },
  \"8740\": {
    \"char\": \"∤\",
    \"latex\": \"\\\\nmid \",
    \"math\": null
  },
  \"8741\": {
    \"char\": \"∥\",
    \"latex\": \"\\\\parallel \",
    \"math\": null
  },
  \"8742\": {
    \"char\": \"∦\",
    \"latex\": \"\\\\nparallel \",
    \"math\": null
  },
  \"8743\": {
    \"char\": \"∧\",
    \"latex\": \"\\\\wedge \",
    \"math\": null
  },
  \"8744\": {
    \"char\": \"∨\",
    \"latex\": \"\\\\vee \",
    \"math\": null
  },
  \"8745\": {
    \"char\": \"∩\",
    \"latex\": \"\\\\cap \",
    \"math\": null
  },
  \"8746\": {
    \"char\": \"∪\",
    \"latex\": \"\\\\cup \",
    \"math\": null
  },
  \"8747\": {
    \"char\": \"∫\",
    \"latex\": \"\\\\int \",
    \"math\": null
  },
  \"8748\": {
    \"char\": \"∬\",
    \"latex\": \"\\\\int\\\\!\\\\int \",
    \"math\": null
  },
  \"8749\": {
    \"char\": \"∭\",
    \"latex\": \"\\\\int\\\\!\\\\int\\\\!\\\\int \",
    \"math\": null
  },
  \"8750\": {
    \"char\": \"∮\",
    \"latex\": \"\\\\oint \",
    \"math\": null
  },
  \"8751\": {
    \"char\": \"∯\",
    \"latex\": \"\\\\surfintegral \",
    \"math\": null
  },
  \"8752\": {
    \"char\": \"∰\",
    \"latex\": \"\\\\volintegral \",
    \"math\": null
  },
  \"8753\": {
    \"char\": \"∱\",
    \"latex\": \"\\\\clwintegral \",
    \"math\": null
  },
  \"8754\": {
    \"char\": \"∲\",
    \"latex\": \"\\\\ElsevierGlyph{2232}\",
    \"math\": null
  },
  \"8755\": {
    \"char\": \"∳\",
    \"latex\": \"\\\\ElsevierGlyph{2233}\",
    \"math\": null
  },
  \"8756\": {
    \"char\": \"∴\",
    \"latex\": \"\\\\therefore \",
    \"math\": null
  },
  \"8757\": {
    \"char\": \"∵\",
    \"latex\": \"\\\\because \",
    \"math\": null
  },
  \"8759\": {
    \"char\": \"∷\",
    \"latex\": \"\\\\Colon \",
    \"math\": null
  },
  \"8760\": {
    \"char\": \"∸\",
    \"latex\": \"\\\\ElsevierGlyph{2238}\",
    \"math\": null
  },
  \"8762\": {
    \"char\": \"∺\",
    \"latex\": \"\\\\mathbin{{:}\\\\!\\\\!{-}\\\\!\\\\!{:}}\",
    \"math\": null
  },
  \"8763\": {
    \"char\": \"∻\",
    \"latex\": \"\\\\homothetic \",
    \"math\": null
  },
  \"8764\": {
    \"char\": \"∼\",
    \"latex\": \"\\\\sim \",
    \"math\": null
  },
  \"8765\": {
    \"char\": \"∽\",
    \"latex\": \"\\\\backsim \",
    \"math\": null
  },
  \"8766\": {
    \"char\": \"∾\",
    \"latex\": \"\\\\lazysinv \",
    \"math\": null
  },
  \"8768\": {
    \"char\": \"≀\",
    \"latex\": \"\\\\wr \",
    \"math\": null
  },
  \"8769\": {
    \"char\": \"≁\",
    \"latex\": \"\\\\not\\\\sim \",
    \"math\": null
  },
  \"8770\": {
    \"char\": \"≂\",
    \"latex\": \"\\\\ElsevierGlyph{2242}\",
    \"math\": null
  },
  \"8771\": {
    \"char\": \"≃\",
    \"latex\": \"\\\\simeq \",
    \"math\": null
  },
  \"8772\": {
    \"char\": \"≄\",
    \"latex\": \"\\\\not\\\\simeq \",
    \"math\": null
  },
  \"8773\": {
    \"char\": \"≅\",
    \"latex\": \"\\\\cong \",
    \"math\": null
  },
  \"8774\": {
    \"char\": \"≆\",
    \"latex\": \"\\\\approxnotequal \",
    \"math\": null
  },
  \"8775\": {
    \"char\": \"≇\",
    \"latex\": \"\\\\not\\\\cong \",
    \"math\": null
  },
  \"8776\": {
    \"char\": \"≈\",
    \"latex\": \"\\\\approx \",
    \"math\": null
  },
  \"8777\": {
    \"char\": \"≉\",
    \"latex\": \"\\\\not\\\\approx \",
    \"math\": null
  },
  \"8778\": {
    \"char\": \"≊\",
    \"latex\": \"\\\\approxeq \",
    \"math\": null
  },
  \"8779\": {
    \"char\": \"≋\",
    \"latex\": \"\\\\tildetrpl \",
    \"math\": null
  },
  \"8780\": {
    \"char\": \"≌\",
    \"latex\": \"\\\\allequal \",
    \"math\": null
  },
  \"8781\": {
    \"char\": \"≍\",
    \"latex\": \"\\\\asymp \",
    \"math\": null
  },
  \"8782\": {
    \"char\": \"≎\",
    \"latex\": \"\\\\Bumpeq \",
    \"math\": null
  },
  \"8783\": {
    \"char\": \"≏\",
    \"latex\": \"\\\\bumpeq \",
    \"math\": null
  },
  \"8784\": {
    \"char\": \"≐\",
    \"latex\": \"\\\\doteq \",
    \"math\": null
  },
  \"8785\": {
    \"char\": \"≑\",
    \"latex\": \"\\\\doteqdot \",
    \"math\": null
  },
  \"8786\": {
    \"char\": \"≒\",
    \"latex\": \"\\\\fallingdotseq \",
    \"math\": null
  },
  \"8787\": {
    \"char\": \"≓\",
    \"latex\": \"\\\\risingdotseq \",
    \"math\": null
  },
  \"8788\": {
    \"char\": \"≔\",
    \"latex\": \":=\",
    \"math\": null
  },
  \"8789\": {
    \"char\": \"≕\",
    \"latex\": \"=:\",
    \"math\": null
  },
  \"8790\": {
    \"char\": \"≖\",
    \"latex\": \"\\\\eqcirc \",
    \"math\": null
  },
  \"8791\": {
    \"char\": \"≗\",
    \"latex\": \"\\\\circeq \",
    \"math\": null
  },
  \"8793\": {
    \"char\": \"≙\",
    \"latex\": \"\\\\estimates \",
    \"math\": null
  },
  \"8794\": {
    \"char\": \"≚\",
    \"latex\": \"\\\\ElsevierGlyph{225A}\",
    \"math\": null
  },
  \"8795\": {
    \"char\": \"≛\",
    \"latex\": \"\\\\starequal \",
    \"math\": null
  },
  \"8796\": {
    \"char\": \"≜\",
    \"latex\": \"\\\\triangleq \",
    \"math\": null
  },
  \"8799\": {
    \"char\": \"≟\",
    \"latex\": \"\\\\ElsevierGlyph{225F}\",
    \"math\": null
  },
  \"8800\": {
    \"char\": \"≠\",
    \"latex\": \"\\\\not =\",
    \"math\": null
  },
  \"8801\": {
    \"char\": \"≡\",
    \"latex\": \"\\\\equiv \",
    \"math\": null
  },
  \"8802\": {
    \"char\": \"≢\",
    \"latex\": \"\\\\not\\\\equiv \",
    \"math\": null
  },
  \"8804\": {
    \"char\": \"≤\",
    \"latex\": \"\\\\leq \",
    \"math\": null
  },
  \"8805\": {
    \"char\": \"≥\",
    \"latex\": \"\\\\geq \",
    \"math\": null
  },
  \"8806\": {
    \"char\": \"≦\",
    \"latex\": \"\\\\leqq \",
    \"math\": null
  },
  \"8807\": {
    \"char\": \"≧\",
    \"latex\": \"\\\\geqq \",
    \"math\": null
  },
  \"8808\": {
    \"char\": \"≨\",
    \"latex\": \"\\\\lneqq \",
    \"math\": null
  },
  \"8809\": {
    \"char\": \"≩\",
    \"latex\": \"\\\\gneqq \",
    \"math\": null
  },
  \"8810\": {
    \"char\": \"≪\",
    \"latex\": \"\\\\ll \",
    \"math\": null
  },
  \"8811\": {
    \"char\": \"≫\",
    \"latex\": \"\\\\gg \",
    \"math\": null
  },
  \"8812\": {
    \"char\": \"≬\",
    \"latex\": \"\\\\between \",
    \"math\": null
  },
  \"8813\": {
    \"char\": \"≭\",
    \"latex\": \"\\\\not\\\\kern-0.3em\\\\times \",
    \"math\": null
  },
  \"8814\": {
    \"char\": \"≮\",
    \"latex\": \"\\\\not<\",
    \"math\": null
  },
  \"8815\": {
    \"char\": \"≯\",
    \"latex\": \"\\\\not>\",
    \"math\": null
  },
  \"8816\": {
    \"char\": \"≰\",
    \"latex\": \"\\\\not\\\\leq \",
    \"math\": null
  },
  \"8817\": {
    \"char\": \"≱\",
    \"latex\": \"\\\\not\\\\geq \",
    \"math\": null
  },
  \"8818\": {
    \"char\": \"≲\",
    \"latex\": \"\\\\lessequivlnt \",
    \"math\": null
  },
  \"8819\": {
    \"char\": \"≳\",
    \"latex\": \"\\\\greaterequivlnt \",
    \"math\": null
  },
  \"8820\": {
    \"char\": \"≴\",
    \"latex\": \"\\\\ElsevierGlyph{2274}\",
    \"math\": null
  },
  \"8821\": {
    \"char\": \"≵\",
    \"latex\": \"\\\\ElsevierGlyph{2275}\",
    \"math\": null
  },
  \"8822\": {
    \"char\": \"≶\",
    \"latex\": \"\\\\lessgtr \",
    \"math\": null
  },
  \"8823\": {
    \"char\": \"≷\",
    \"latex\": \"\\\\gtrless \",
    \"math\": null
  },
  \"8824\": {
    \"char\": \"≸\",
    \"latex\": \"\\\\notlessgreater \",
    \"math\": null
  },
  \"8825\": {
    \"char\": \"≹\",
    \"latex\": \"\\\\notgreaterless \",
    \"math\": null
  },
  \"8826\": {
    \"char\": \"≺\",
    \"latex\": \"\\\\prec \",
    \"math\": null
  },
  \"8827\": {
    \"char\": \"≻\",
    \"latex\": \"\\\\succ \",
    \"math\": null
  },
  \"8828\": {
    \"char\": \"≼\",
    \"latex\": \"\\\\preccurlyeq \",
    \"math\": null
  },
  \"8829\": {
    \"char\": \"≽\",
    \"latex\": \"\\\\succcurlyeq \",
    \"math\": null
  },
  \"8830\": {
    \"char\": \"≾\",
    \"latex\": \"\\\\precapprox \",
    \"math\": null
  },
  \"8831\": {
    \"char\": \"≿\",
    \"latex\": \"\\\\succapprox \",
    \"math\": null
  },
  \"8832\": {
    \"char\": \"⊀\",
    \"latex\": \"\\\\not\\\\prec \",
    \"math\": null
  },
  \"8833\": {
    \"char\": \"⊁\",
    \"latex\": \"\\\\not\\\\succ \",
    \"math\": null
  },
  \"8834\": {
    \"char\": \"⊂\",
    \"latex\": \"\\\\subset \",
    \"math\": null
  },
  \"8835\": {
    \"char\": \"⊃\",
    \"latex\": \"\\\\supset \",
    \"math\": null
  },
  \"8836\": {
    \"char\": \"⊄\",
    \"latex\": \"\\\\not\\\\subset \",
    \"math\": null
  },
  \"8837\": {
    \"char\": \"⊅\",
    \"latex\": \"\\\\not\\\\supset \",
    \"math\": null
  },
  \"8838\": {
    \"char\": \"⊆\",
    \"latex\": \"\\\\subseteq \",
    \"math\": null
  },
  \"8839\": {
    \"char\": \"⊇\",
    \"latex\": \"\\\\supseteq \",
    \"math\": null
  },
  \"8840\": {
    \"char\": \"⊈\",
    \"latex\": \"\\\\not\\\\subseteq \",
    \"math\": null
  },
  \"8841\": {
    \"char\": \"⊉\",
    \"latex\": \"\\\\not\\\\supseteq \",
    \"math\": null
  },
  \"8842\": {
    \"char\": \"⊊\",
    \"latex\": \"\\\\subsetneq \",
    \"math\": null
  },
  \"8843\": {
    \"char\": \"⊋\",
    \"latex\": \"\\\\supsetneq \",
    \"math\": null
  },
  \"8846\": {
    \"char\": \"⊎\",
    \"latex\": \"\\\\uplus \",
    \"math\": null
  },
  \"8847\": {
    \"char\": \"⊏\",
    \"latex\": \"\\\\sqsubset \",
    \"math\": null
  },
  \"8848\": {
    \"char\": \"⊐\",
    \"latex\": \"\\\\sqsupset \",
    \"math\": null
  },
  \"8849\": {
    \"char\": \"⊑\",
    \"latex\": \"\\\\sqsubseteq \",
    \"math\": null
  },
  \"8850\": {
    \"char\": \"⊒\",
    \"latex\": \"\\\\sqsupseteq \",
    \"math\": null
  },
  \"8851\": {
    \"char\": \"⊓\",
    \"latex\": \"\\\\sqcap \",
    \"math\": null
  },
  \"8852\": {
    \"char\": \"⊔\",
    \"latex\": \"\\\\sqcup \",
    \"math\": null
  },
  \"8853\": {
    \"char\": \"⊕\",
    \"latex\": \"\\\\oplus \",
    \"math\": null
  },
  \"8854\": {
    \"char\": \"⊖\",
    \"latex\": \"\\\\ominus \",
    \"math\": null
  },
  \"8855\": {
    \"char\": \"⊗\",
    \"latex\": \"\\\\otimes \",
    \"math\": null
  },
  \"8856\": {
    \"char\": \"⊘\",
    \"latex\": \"\\\\oslash \",
    \"math\": null
  },
  \"8857\": {
    \"char\": \"⊙\",
    \"latex\": \"\\\\odot \",
    \"math\": null
  },
  \"8858\": {
    \"char\": \"⊚\",
    \"latex\": \"\\\\circledcirc \",
    \"math\": null
  },
  \"8859\": {
    \"char\": \"⊛\",
    \"latex\": \"\\\\circledast \",
    \"math\": null
  },
  \"8861\": {
    \"char\": \"⊝\",
    \"latex\": \"\\\\circleddash \",
    \"math\": null
  },
  \"8862\": {
    \"char\": \"⊞\",
    \"latex\": \"\\\\boxplus \",
    \"math\": null
  },
  \"8863\": {
    \"char\": \"⊟\",
    \"latex\": \"\\\\boxminus \",
    \"math\": null
  },
  \"8864\": {
    \"char\": \"⊠\",
    \"latex\": \"\\\\boxtimes \",
    \"math\": null
  },
  \"8865\": {
    \"char\": \"⊡\",
    \"latex\": \"\\\\boxdot \",
    \"math\": null
  },
  \"8866\": {
    \"char\": \"⊢\",
    \"latex\": \"\\\\vdash \",
    \"math\": null
  },
  \"8867\": {
    \"char\": \"⊣\",
    \"latex\": \"\\\\dashv \",
    \"math\": null
  },
  \"8868\": {
    \"char\": \"⊤\",
    \"latex\": \"\\\\top \",
    \"math\": null
  },
  \"8869\": {
    \"char\": \"⊥\",
    \"latex\": \"\\\\perp \",
    \"math\": null
  },
  \"8871\": {
    \"char\": \"⊧\",
    \"latex\": \"\\\\truestate \",
    \"math\": null
  },
  \"8872\": {
    \"char\": \"⊨\",
    \"latex\": \"\\\\forcesextra \",
    \"math\": null
  },
  \"8873\": {
    \"char\": \"⊩\",
    \"latex\": \"\\\\Vdash \",
    \"math\": null
  },
  \"8874\": {
    \"char\": \"⊪\",
    \"latex\": \"\\\\Vvdash \",
    \"math\": null
  },
  \"8875\": {
    \"char\": \"⊫\",
    \"latex\": \"\\\\VDash \",
    \"math\": null
  },
  \"8876\": {
    \"char\": \"⊬\",
    \"latex\": \"\\\\nvdash \",
    \"math\": null
  },
  \"8877\": {
    \"char\": \"⊭\",
    \"latex\": \"\\\\nvDash \",
    \"math\": null
  },
  \"8878\": {
    \"char\": \"⊮\",
    \"latex\": \"\\\\nVdash \",
    \"math\": null
  },
  \"8879\": {
    \"char\": \"⊯\",
    \"latex\": \"\\\\nVDash \",
    \"math\": null
  },
  \"8882\": {
    \"char\": \"⊲\",
    \"latex\": \"\\\\vartriangleleft \",
    \"math\": null
  },
  \"8883\": {
    \"char\": \"⊳\",
    \"latex\": \"\\\\vartriangleright \",
    \"math\": null
  },
  \"8884\": {
    \"char\": \"⊴\",
    \"latex\": \"\\\\trianglelefteq \",
    \"math\": null
  },
  \"8885\": {
    \"char\": \"⊵\",
    \"latex\": \"\\\\trianglerighteq \",
    \"math\": null
  },
  \"8886\": {
    \"char\": \"⊶\",
    \"latex\": \"\\\\original \",
    \"math\": null
  },
  \"8887\": {
    \"char\": \"⊷\",
    \"latex\": \"\\\\image \",
    \"math\": null
  },
  \"8888\": {
    \"char\": \"⊸\",
    \"latex\": \"\\\\multimap \",
    \"math\": null
  },
  \"8889\": {
    \"char\": \"⊹\",
    \"latex\": \"\\\\hermitconjmatrix \",
    \"math\": null
  },
  \"8890\": {
    \"char\": \"⊺\",
    \"latex\": \"\\\\intercal \",
    \"math\": null
  },
  \"8891\": {
    \"char\": \"⊻\",
    \"latex\": \"\\\\veebar \",
    \"math\": null
  },
  \"8894\": {
    \"char\": \"⊾\",
    \"latex\": \"\\\\rightanglearc \",
    \"math\": null
  },
  \"8896\": {
    \"char\": \"⋀\",
    \"latex\": \"\\\\ElsevierGlyph{22C0}\",
    \"math\": null
  },
  \"8897\": {
    \"char\": \"⋁\",
    \"latex\": \"\\\\ElsevierGlyph{22C1}\",
    \"math\": null
  },
  \"8898\": {
    \"char\": \"⋂\",
    \"latex\": \"\\\\bigcap \",
    \"math\": null
  },
  \"8899\": {
    \"char\": \"⋃\",
    \"latex\": \"\\\\bigcup \",
    \"math\": null
  },
  \"8900\": {
    \"char\": \"⋄\",
    \"latex\": \"\\\\diamond \",
    \"math\": null
  },
  \"8901\": {
    \"char\": \"⋅\",
    \"latex\": \"\\\\cdot \",
    \"math\": null
  },
  \"8902\": {
    \"char\": \"⋆\",
    \"latex\": \"\\\\star \",
    \"math\": null
  },
  \"8903\": {
    \"char\": \"⋇\",
    \"latex\": \"\\\\divideontimes \",
    \"math\": null
  },
  \"8904\": {
    \"char\": \"⋈\",
    \"latex\": \"\\\\bowtie \",
    \"math\": null
  },
  \"8905\": {
    \"char\": \"⋉\",
    \"latex\": \"\\\\ltimes \",
    \"math\": null
  },
  \"8906\": {
    \"char\": \"⋊\",
    \"latex\": \"\\\\rtimes \",
    \"math\": null
  },
  \"8907\": {
    \"char\": \"⋋\",
    \"latex\": \"\\\\leftthreetimes \",
    \"math\": null
  },
  \"8908\": {
    \"char\": \"⋌\",
    \"latex\": \"\\\\rightthreetimes \",
    \"math\": null
  },
  \"8909\": {
    \"char\": \"⋍\",
    \"latex\": \"\\\\backsimeq \",
    \"math\": null
  },
  \"8910\": {
    \"char\": \"⋎\",
    \"latex\": \"\\\\curlyvee \",
    \"math\": null
  },
  \"8911\": {
    \"char\": \"⋏\",
    \"latex\": \"\\\\curlywedge \",
    \"math\": null
  },
  \"8912\": {
    \"char\": \"⋐\",
    \"latex\": \"\\\\Subset \",
    \"math\": null
  },
  \"8913\": {
    \"char\": \"⋑\",
    \"latex\": \"\\\\Supset \",
    \"math\": null
  },
  \"8914\": {
    \"char\": \"⋒\",
    \"latex\": \"\\\\Cap \",
    \"math\": null
  },
  \"8915\": {
    \"char\": \"⋓\",
    \"latex\": \"\\\\Cup \",
    \"math\": null
  },
  \"8916\": {
    \"char\": \"⋔\",
    \"latex\": \"\\\\pitchfork \",
    \"math\": null
  },
  \"8918\": {
    \"char\": \"⋖\",
    \"latex\": \"\\\\lessdot \",
    \"math\": null
  },
  \"8919\": {
    \"char\": \"⋗\",
    \"latex\": \"\\\\gtrdot \",
    \"math\": null
  },
  \"8920\": {
    \"char\": \"⋘\",
    \"latex\": \"\\\\verymuchless \",
    \"math\": null
  },
  \"8921\": {
    \"char\": \"⋙\",
    \"latex\": \"\\\\verymuchgreater \",
    \"math\": null
  },
  \"8922\": {
    \"char\": \"⋚\",
    \"latex\": \"\\\\lesseqgtr \",
    \"math\": null
  },
  \"8923\": {
    \"char\": \"⋛\",
    \"latex\": \"\\\\gtreqless \",
    \"math\": null
  },
  \"8926\": {
    \"char\": \"⋞\",
    \"latex\": \"\\\\curlyeqprec \",
    \"math\": null
  },
  \"8927\": {
    \"char\": \"⋟\",
    \"latex\": \"\\\\curlyeqsucc \",
    \"math\": null
  },
  \"8930\": {
    \"char\": \"⋢\",
    \"latex\": \"\\\\not\\\\sqsubseteq \",
    \"math\": null
  },
  \"8931\": {
    \"char\": \"⋣\",
    \"latex\": \"\\\\not\\\\sqsupseteq \",
    \"math\": null
  },
  \"8933\": {
    \"char\": \"⋥\",
    \"latex\": \"\\\\Elzsqspne \",
    \"math\": null
  },
  \"8934\": {
    \"char\": \"⋦\",
    \"latex\": \"\\\\lnsim \",
    \"math\": null
  },
  \"8935\": {
    \"char\": \"⋧\",
    \"latex\": \"\\\\gnsim \",
    \"math\": null
  },
  \"8936\": {
    \"char\": \"⋨\",
    \"latex\": \"\\\\precedesnotsimilar \",
    \"math\": null
  },
  \"8937\": {
    \"char\": \"⋩\",
    \"latex\": \"\\\\succnsim \",
    \"math\": null
  },
  \"8938\": {
    \"char\": \"⋪\",
    \"latex\": \"\\\\ntriangleleft \",
    \"math\": null
  },
  \"8939\": {
    \"char\": \"⋫\",
    \"latex\": \"\\\\ntriangleright \",
    \"math\": null
  },
  \"8940\": {
    \"char\": \"⋬\",
    \"latex\": \"\\\\ntrianglelefteq \",
    \"math\": null
  },
  \"8941\": {
    \"char\": \"⋭\",
    \"latex\": \"\\\\ntrianglerighteq \",
    \"math\": null
  },
  \"8942\": {
    \"char\": \"⋮\",
    \"latex\": \"\\\\vdots \",
    \"math\": null
  },
  \"8943\": {
    \"char\": \"⋯\",
    \"latex\": \"\\\\cdots \",
    \"math\": null
  },
  \"8944\": {
    \"char\": \"⋰\",
    \"latex\": \"\\\\upslopeellipsis \",
    \"math\": null
  },
  \"8945\": {
    \"char\": \"⋱\",
    \"latex\": \"\\\\downslopeellipsis \",
    \"math\": null
  },
  \"8965\": {
    \"char\": \"⌅\",
    \"latex\": \"\\\\barwedge \",
    \"math\": null
  },
  \"8966\": {
    \"char\": \"⌆\",
    \"latex\": \"\\\\perspcorrespond \",
    \"math\": null
  },
  \"8968\": {
    \"char\": \"⌈\",
    \"latex\": \"\\\\lceil \",
    \"math\": null
  },
  \"8969\": {
    \"char\": \"⌉\",
    \"latex\": \"\\\\rceil \",
    \"math\": null
  },
  \"8970\": {
    \"char\": \"⌊\",
    \"latex\": \"\\\\lfloor \",
    \"math\": null
  },
  \"8971\": {
    \"char\": \"⌋\",
    \"latex\": \"\\\\rfloor \",
    \"math\": null
  },
  \"8981\": {
    \"char\": \"⌕\",
    \"latex\": \"\\\\recorder \",
    \"math\": null
  },
  \"8982\": {
    \"char\": \"⌖\",
    \"latex\": \"\\\\mathchar\\\"2208\",
    \"math\": null
  },
  \"8988\": {
    \"char\": \"⌜\",
    \"latex\": \"\\\\ulcorner \",
    \"math\": null
  },
  \"8989\": {
    \"char\": \"⌝\",
    \"latex\": \"\\\\urcorner \",
    \"math\": null
  },
  \"8990\": {
    \"char\": \"⌞\",
    \"latex\": \"\\\\llcorner \",
    \"math\": null
  },
  \"8991\": {
    \"char\": \"⌟\",
    \"latex\": \"\\\\lrcorner \",
    \"math\": null
  },
  \"8994\": {
    \"char\": \"⌢\",
    \"latex\": \"\\\\frown \",
    \"math\": null
  },
  \"8995\": {
    \"char\": \"⌣\",
    \"latex\": \"\\\\smile \",
    \"math\": null
  },
  \"9001\": {
    \"char\": \"〈\",
    \"latex\": \"\\\\langle \",
    \"math\": null
  },
  \"9002\": {
    \"char\": \"〉\",
    \"latex\": \"\\\\rangle \",
    \"math\": null
  },
  \"9021\": {
    \"char\": \"⌽\",
    \"latex\": \"\\\\ElsevierGlyph{E838}\",
    \"math\": null
  },
  \"9123\": {
    \"char\": \"⎣\",
    \"latex\": \"\\\\Elzdlcorn \",
    \"math\": null
  },
  \"9136\": {
    \"char\": \"⎰\",
    \"latex\": \"\\\\lmoustache \",
    \"math\": null
  },
  \"9137\": {
    \"char\": \"⎱\",
    \"latex\": \"\\\\rmoustache \",
    \"math\": null
  },
  \"9251\": {
    \"char\": \"␣\",
    \"latex\": \"\\\\textvisiblespace \",
    \"math\": null
  },
  \"9312\": {
    \"char\": \"①\",
    \"latex\": \"\\\\ding{172}\",
    \"math\": null
  },
  \"9313\": {
    \"char\": \"②\",
    \"latex\": \"\\\\ding{173}\",
    \"math\": null
  },
  \"9314\": {
    \"char\": \"③\",
    \"latex\": \"\\\\ding{174}\",
    \"math\": null
  },
  \"9315\": {
    \"char\": \"④\",
    \"latex\": \"\\\\ding{175}\",
    \"math\": null
  },
  \"9316\": {
    \"char\": \"⑤\",
    \"latex\": \"\\\\ding{176}\",
    \"math\": null
  },
  \"9317\": {
    \"char\": \"⑥\",
    \"latex\": \"\\\\ding{177}\",
    \"math\": null
  },
  \"9318\": {
    \"char\": \"⑦\",
    \"latex\": \"\\\\ding{178}\",
    \"math\": null
  },
  \"9319\": {
    \"char\": \"⑧\",
    \"latex\": \"\\\\ding{179}\",
    \"math\": null
  },
  \"9320\": {
    \"char\": \"⑨\",
    \"latex\": \"\\\\ding{180}\",
    \"math\": null
  },
  \"9321\": {
    \"char\": \"⑩\",
    \"latex\": \"\\\\ding{181}\",
    \"math\": null
  },
  \"9416\": {
    \"char\": \"Ⓢ\",
    \"latex\": \"\\\\circledS \",
    \"math\": null
  },
  \"9478\": {
    \"char\": \"┆\",
    \"latex\": \"\\\\Elzdshfnc \",
    \"math\": null
  },
  \"9497\": {
    \"char\": \"┙\",
    \"latex\": \"\\\\Elzsqfnw \",
    \"math\": null
  },
  \"9585\": {
    \"char\": \"╱\",
    \"latex\": \"\\\\diagup \",
    \"math\": null
  },
  \"9632\": {
    \"char\": \"■\",
    \"latex\": \"\\\\ding{110}\",
    \"math\": null
  },
  \"9633\": {
    \"char\": \"□\",
    \"latex\": \"\\\\square \",
    \"math\": null
  },
  \"9642\": {
    \"char\": \"▪\",
    \"latex\": \"\\\\blacksquare \",
    \"math\": null
  },
  \"9645\": {
    \"char\": \"▭\",
    \"latex\": \"\\\\fbox{~~}\",
    \"math\": null
  },
  \"9647\": {
    \"char\": \"▯\",
    \"latex\": \"\\\\Elzvrecto \",
    \"math\": null
  },
  \"9649\": {
    \"char\": \"▱\",
    \"latex\": \"\\\\ElsevierGlyph{E381}\",
    \"math\": null
  },
  \"9650\": {
    \"char\": \"▲\",
    \"latex\": \"\\\\ding{115}\",
    \"math\": null
  },
  \"9651\": {
    \"char\": \"△\",
    \"latex\": \"\\\\bigtriangleup \",
    \"math\": null
  },
  \"9652\": {
    \"char\": \"▴\",
    \"latex\": \"\\\\blacktriangle \",
    \"math\": null
  },
  \"9653\": {
    \"char\": \"▵\",
    \"latex\": \"\\\\vartriangle \",
    \"math\": null
  },
  \"9656\": {
    \"char\": \"▸\",
    \"latex\": \"\\\\blacktriangleright \",
    \"math\": null
  },
  \"9657\": {
    \"char\": \"▹\",
    \"latex\": \"\\\\triangleright \",
    \"math\": null
  },
  \"9660\": {
    \"char\": \"▼\",
    \"latex\": \"\\\\ding{116}\",
    \"math\": null
  },
  \"9661\": {
    \"char\": \"▽\",
    \"latex\": \"\\\\bigtriangledown \",
    \"math\": null
  },
  \"9662\": {
    \"char\": \"▾\",
    \"latex\": \"\\\\blacktriangledown \",
    \"math\": null
  },
  \"9663\": {
    \"char\": \"▿\",
    \"latex\": \"\\\\triangledown \",
    \"math\": null
  },
  \"9666\": {
    \"char\": \"◂\",
    \"latex\": \"\\\\blacktriangleleft \",
    \"math\": null
  },
  \"9667\": {
    \"char\": \"◃\",
    \"latex\": \"\\\\triangleleft \",
    \"math\": null
  },
  \"9670\": {
    \"char\": \"◆\",
    \"latex\": \"\\\\ding{117}\",
    \"math\": null
  },
  \"9674\": {
    \"char\": \"◊\",
    \"latex\": \"\\\\lozenge \",
    \"math\": null
  },
  \"9675\": {
    \"char\": \"○\",
    \"latex\": \"\\\\bigcirc \",
    \"math\": null
  },
  \"9679\": {
    \"char\": \"●\",
    \"latex\": \"\\\\ding{108}\",
    \"math\": null
  },
  \"9680\": {
    \"char\": \"◐\",
    \"latex\": \"\\\\Elzcirfl \",
    \"math\": null
  },
  \"9681\": {
    \"char\": \"◑\",
    \"latex\": \"\\\\Elzcirfr \",
    \"math\": null
  },
  \"9682\": {
    \"char\": \"◒\",
    \"latex\": \"\\\\Elzcirfb \",
    \"math\": null
  },
  \"9687\": {
    \"char\": \"◗\",
    \"latex\": \"\\\\ding{119}\",
    \"math\": null
  },
  \"9688\": {
    \"char\": \"◘\",
    \"latex\": \"\\\\Elzrvbull \",
    \"math\": null
  },
  \"9703\": {
    \"char\": \"◧\",
    \"latex\": \"\\\\Elzsqfl \",
    \"math\": null
  },
  \"9704\": {
    \"char\": \"◨\",
    \"latex\": \"\\\\Elzsqfr \",
    \"math\": null
  },
  \"9706\": {
    \"char\": \"◪\",
    \"latex\": \"\\\\Elzsqfse \",
    \"math\": null
  },
  \"9711\": {
    \"char\": \"◯\",
    \"latex\": \"\\\\bigcirc \",
    \"math\": null
  },
  \"9733\": {
    \"char\": \"★\",
    \"latex\": \"\\\\ding{72}\",
    \"math\": null
  },
  \"9734\": {
    \"char\": \"☆\",
    \"latex\": \"\\\\ding{73}\",
    \"math\": null
  },
  \"9742\": {
    \"char\": \"☎\",
    \"latex\": \"\\\\ding{37}\",
    \"math\": null
  },
  \"9755\": {
    \"char\": \"☛\",
    \"latex\": \"\\\\ding{42}\",
    \"math\": null
  },
  \"9758\": {
    \"char\": \"☞\",
    \"latex\": \"\\\\ding{43}\",
    \"math\": null
  },
  \"9790\": {
    \"char\": \"☾\",
    \"latex\": \"\\\\rightmoon \",
    \"math\": null
  },
  \"9791\": {
    \"char\": \"☿\",
    \"latex\": \"\\\\mercury \",
    \"math\": null
  },
  \"9792\": {
    \"char\": \"♀\",
    \"latex\": \"\\\\venus \",
    \"math\": null
  },
  \"9794\": {
    \"char\": \"♂\",
    \"latex\": \"\\\\male \",
    \"math\": null
  },
  \"9795\": {
    \"char\": \"♃\",
    \"latex\": \"\\\\jupiter \",
    \"math\": null
  },
  \"9796\": {
    \"char\": \"♄\",
    \"latex\": \"\\\\saturn \",
    \"math\": null
  },
  \"9797\": {
    \"char\": \"♅\",
    \"latex\": \"\\\\uranus \",
    \"math\": null
  },
  \"9798\": {
    \"char\": \"♆\",
    \"latex\": \"\\\\neptune \",
    \"math\": null
  },
  \"9799\": {
    \"char\": \"♇\",
    \"latex\": \"\\\\pluto \",
    \"math\": null
  },
  \"9800\": {
    \"char\": \"♈\",
    \"latex\": \"\\\\aries \",
    \"math\": null
  },
  \"9801\": {
    \"char\": \"♉\",
    \"latex\": \"\\\\taurus \",
    \"math\": null
  },
  \"9802\": {
    \"char\": \"♊\",
    \"latex\": \"\\\\gemini \",
    \"math\": null
  },
  \"9803\": {
    \"char\": \"♋\",
    \"latex\": \"\\\\cancer \",
    \"math\": null
  },
  \"9804\": {
    \"char\": \"♌\",
    \"latex\": \"\\\\leo \",
    \"math\": null
  },
  \"9805\": {
    \"char\": \"♍\",
    \"latex\": \"\\\\virgo \",
    \"math\": null
  },
  \"9806\": {
    \"char\": \"♎\",
    \"latex\": \"\\\\libra \",
    \"math\": null
  },
  \"9807\": {
    \"char\": \"♏\",
    \"latex\": \"\\\\scorpio \",
    \"math\": null
  },
  \"9808\": {
    \"char\": \"♐\",
    \"latex\": \"\\\\sagittarius \",
    \"math\": null
  },
  \"9809\": {
    \"char\": \"♑\",
    \"latex\": \"\\\\capricornus \",
    \"math\": null
  },
  \"9810\": {
    \"char\": \"♒\",
    \"latex\": \"\\\\aquarius \",
    \"math\": null
  },
  \"9811\": {
    \"char\": \"♓\",
    \"latex\": \"\\\\pisces \",
    \"math\": null
  },
  \"9824\": {
    \"char\": \"♠\",
    \"latex\": \"\\\\ding{171}\",
    \"math\": null
  },
  \"9826\": {
    \"char\": \"♢\",
    \"latex\": \"\\\\diamond \",
    \"math\": null
  },
  \"9827\": {
    \"char\": \"♣\",
    \"latex\": \"\\\\ding{168}\",
    \"math\": null
  },
  \"9829\": {
    \"char\": \"♥\",
    \"latex\": \"\\\\ding{170}\",
    \"math\": null
  },
  \"9830\": {
    \"char\": \"♦\",
    \"latex\": \"\\\\ding{169}\",
    \"math\": null
  },
  \"9833\": {
    \"char\": \"♩\",
    \"latex\": \"\\\\quarternote \",
    \"math\": null
  },
  \"9834\": {
    \"char\": \"♪\",
    \"latex\": \"\\\\eighthnote \",
    \"math\": null
  },
  \"9837\": {
    \"char\": \"♭\",
    \"latex\": \"\\\\flat \",
    \"math\": null
  },
  \"9838\": {
    \"char\": \"♮\",
    \"latex\": \"\\\\natural \",
    \"math\": null
  },
  \"9839\": {
    \"char\": \"♯\",
    \"latex\": \"\\\\sharp \",
    \"math\": null
  },
  \"9985\": {
    \"char\": \"✁\",
    \"latex\": \"\\\\ding{33}\",
    \"math\": null
  },
  \"9986\": {
    \"char\": \"✂\",
    \"latex\": \"\\\\ding{34}\",
    \"math\": null
  },
  \"9987\": {
    \"char\": \"✃\",
    \"latex\": \"\\\\ding{35}\",
    \"math\": null
  },
  \"9988\": {
    \"char\": \"✄\",
    \"latex\": \"\\\\ding{36}\",
    \"math\": null
  },
  \"9990\": {
    \"char\": \"✆\",
    \"latex\": \"\\\\ding{38}\",
    \"math\": null
  },
  \"9991\": {
    \"char\": \"✇\",
    \"latex\": \"\\\\ding{39}\",
    \"math\": null
  },
  \"9992\": {
    \"char\": \"✈\",
    \"latex\": \"\\\\ding{40}\",
    \"math\": null
  },
  \"9993\": {
    \"char\": \"✉\",
    \"latex\": \"\\\\ding{41}\",
    \"math\": null
  },
  \"9996\": {
    \"char\": \"✌\",
    \"latex\": \"\\\\ding{44}\",
    \"math\": null
  },
  \"9997\": {
    \"char\": \"✍\",
    \"latex\": \"\\\\ding{45}\",
    \"math\": null
  },
  \"9998\": {
    \"char\": \"✎\",
    \"latex\": \"\\\\ding{46}\",
    \"math\": null
  },
  \"9999\": {
    \"char\": \"✏\",
    \"latex\": \"\\\\ding{47}\",
    \"math\": null
  },
  \"10000\": {
    \"char\": \"✐\",
    \"latex\": \"\\\\ding{48}\",
    \"math\": null
  },
  \"10001\": {
    \"char\": \"✑\",
    \"latex\": \"\\\\ding{49}\",
    \"math\": null
  },
  \"10002\": {
    \"char\": \"✒\",
    \"latex\": \"\\\\ding{50}\",
    \"math\": null
  },
  \"10003\": {
    \"char\": \"✓\",
    \"latex\": \"\\\\ding{51}\",
    \"math\": null
  },
  \"10004\": {
    \"char\": \"✔\",
    \"latex\": \"\\\\ding{52}\",
    \"math\": null
  },
  \"10005\": {
    \"char\": \"✕\",
    \"latex\": \"\\\\ding{53}\",
    \"math\": null
  },
  \"10006\": {
    \"char\": \"✖\",
    \"latex\": \"\\\\ding{54}\",
    \"math\": null
  },
  \"10007\": {
    \"char\": \"✗\",
    \"latex\": \"\\\\ding{55}\",
    \"math\": null
  },
  \"10008\": {
    \"char\": \"✘\",
    \"latex\": \"\\\\ding{56}\",
    \"math\": null
  },
  \"10009\": {
    \"char\": \"✙\",
    \"latex\": \"\\\\ding{57}\",
    \"math\": null
  },
  \"10010\": {
    \"char\": \"✚\",
    \"latex\": \"\\\\ding{58}\",
    \"math\": null
  },
  \"10011\": {
    \"char\": \"✛\",
    \"latex\": \"\\\\ding{59}\",
    \"math\": null
  },
  \"10012\": {
    \"char\": \"✜\",
    \"latex\": \"\\\\ding{60}\",
    \"math\": null
  },
  \"10013\": {
    \"char\": \"✝\",
    \"latex\": \"\\\\ding{61}\",
    \"math\": null
  },
  \"10014\": {
    \"char\": \"✞\",
    \"latex\": \"\\\\ding{62}\",
    \"math\": null
  },
  \"10015\": {
    \"char\": \"✟\",
    \"latex\": \"\\\\ding{63}\",
    \"math\": null
  },
  \"10016\": {
    \"char\": \"✠\",
    \"latex\": \"\\\\ding{64}\",
    \"math\": null
  },
  \"10017\": {
    \"char\": \"✡\",
    \"latex\": \"\\\\ding{65}\",
    \"math\": null
  },
  \"10018\": {
    \"char\": \"✢\",
    \"latex\": \"\\\\ding{66}\",
    \"math\": null
  },
  \"10019\": {
    \"char\": \"✣\",
    \"latex\": \"\\\\ding{67}\",
    \"math\": null
  },
  \"10020\": {
    \"char\": \"✤\",
    \"latex\": \"\\\\ding{68}\",
    \"math\": null
  },
  \"10021\": {
    \"char\": \"✥\",
    \"latex\": \"\\\\ding{69}\",
    \"math\": null
  },
  \"10022\": {
    \"char\": \"✦\",
    \"latex\": \"\\\\ding{70}\",
    \"math\": null
  },
  \"10023\": {
    \"char\": \"✧\",
    \"latex\": \"\\\\ding{71}\",
    \"math\": null
  },
  \"10025\": {
    \"char\": \"✩\",
    \"latex\": \"\\\\ding{73}\",
    \"math\": null
  },
  \"10026\": {
    \"char\": \"✪\",
    \"latex\": \"\\\\ding{74}\",
    \"math\": null
  },
  \"10027\": {
    \"char\": \"✫\",
    \"latex\": \"\\\\ding{75}\",
    \"math\": null
  },
  \"10028\": {
    \"char\": \"✬\",
    \"latex\": \"\\\\ding{76}\",
    \"math\": null
  },
  \"10029\": {
    \"char\": \"✭\",
    \"latex\": \"\\\\ding{77}\",
    \"math\": null
  },
  \"10030\": {
    \"char\": \"✮\",
    \"latex\": \"\\\\ding{78}\",
    \"math\": null
  },
  \"10031\": {
    \"char\": \"✯\",
    \"latex\": \"\\\\ding{79}\",
    \"math\": null
  },
  \"10032\": {
    \"char\": \"✰\",
    \"latex\": \"\\\\ding{80}\",
    \"math\": null
  },
  \"10033\": {
    \"char\": \"✱\",
    \"latex\": \"\\\\ding{81}\",
    \"math\": null
  },
  \"10034\": {
    \"char\": \"✲\",
    \"latex\": \"\\\\ding{82}\",
    \"math\": null
  },
  \"10035\": {
    \"char\": \"✳\",
    \"latex\": \"\\\\ding{83}\",
    \"math\": null
  },
  \"10036\": {
    \"char\": \"✴\",
    \"latex\": \"\\\\ding{84}\",
    \"math\": null
  },
  \"10037\": {
    \"char\": \"✵\",
    \"latex\": \"\\\\ding{85}\",
    \"math\": null
  },
  \"10038\": {
    \"char\": \"✶\",
    \"latex\": \"\\\\ding{86}\",
    \"math\": null
  },
  \"10039\": {
    \"char\": \"✷\",
    \"latex\": \"\\\\ding{87}\",
    \"math\": null
  },
  \"10040\": {
    \"char\": \"✸\",
    \"latex\": \"\\\\ding{88}\",
    \"math\": null
  },
  \"10041\": {
    \"char\": \"✹\",
    \"latex\": \"\\\\ding{89}\",
    \"math\": null
  },
  \"10042\": {
    \"char\": \"✺\",
    \"latex\": \"\\\\ding{90}\",
    \"math\": null
  },
  \"10043\": {
    \"char\": \"✻\",
    \"latex\": \"\\\\ding{91}\",
    \"math\": null
  },
  \"10044\": {
    \"char\": \"✼\",
    \"latex\": \"\\\\ding{92}\",
    \"math\": null
  },
  \"10045\": {
    \"char\": \"✽\",
    \"latex\": \"\\\\ding{93}\",
    \"math\": null
  },
  \"10046\": {
    \"char\": \"✾\",
    \"latex\": \"\\\\ding{94}\",
    \"math\": null
  },
  \"10047\": {
    \"char\": \"✿\",
    \"latex\": \"\\\\ding{95}\",
    \"math\": null
  },
  \"10048\": {
    \"char\": \"❀\",
    \"latex\": \"\\\\ding{96}\",
    \"math\": null
  },
  \"10049\": {
    \"char\": \"❁\",
    \"latex\": \"\\\\ding{97}\",
    \"math\": null
  },
  \"10050\": {
    \"char\": \"❂\",
    \"latex\": \"\\\\ding{98}\",
    \"math\": null
  },
  \"10051\": {
    \"char\": \"❃\",
    \"latex\": \"\\\\ding{99}\",
    \"math\": null
  },
  \"10052\": {
    \"char\": \"❄\",
    \"latex\": \"\\\\ding{100}\",
    \"math\": null
  },
  \"10053\": {
    \"char\": \"❅\",
    \"latex\": \"\\\\ding{101}\",
    \"math\": null
  },
  \"10054\": {
    \"char\": \"❆\",
    \"latex\": \"\\\\ding{102}\",
    \"math\": null
  },
  \"10055\": {
    \"char\": \"❇\",
    \"latex\": \"\\\\ding{103}\",
    \"math\": null
  },
  \"10056\": {
    \"char\": \"❈\",
    \"latex\": \"\\\\ding{104}\",
    \"math\": null
  },
  \"10057\": {
    \"char\": \"❉\",
    \"latex\": \"\\\\ding{105}\",
    \"math\": null
  },
  \"10058\": {
    \"char\": \"❊\",
    \"latex\": \"\\\\ding{106}\",
    \"math\": null
  },
  \"10059\": {
    \"char\": \"❋\",
    \"latex\": \"\\\\ding{107}\",
    \"math\": null
  },
  \"10061\": {
    \"char\": \"❍\",
    \"latex\": \"\\\\ding{109}\",
    \"math\": null
  },
  \"10063\": {
    \"char\": \"❏\",
    \"latex\": \"\\\\ding{111}\",
    \"math\": null
  },
  \"10064\": {
    \"char\": \"❐\",
    \"latex\": \"\\\\ding{112}\",
    \"math\": null
  },
  \"10065\": {
    \"char\": \"❑\",
    \"latex\": \"\\\\ding{113}\",
    \"math\": null
  },
  \"10066\": {
    \"char\": \"❒\",
    \"latex\": \"\\\\ding{114}\",
    \"math\": null
  },
  \"10070\": {
    \"char\": \"❖\",
    \"latex\": \"\\\\ding{118}\",
    \"math\": null
  },
  \"10072\": {
    \"char\": \"❘\",
    \"latex\": \"\\\\ding{120}\",
    \"math\": null
  },
  \"10073\": {
    \"char\": \"❙\",
    \"latex\": \"\\\\ding{121}\",
    \"math\": null
  },
  \"10074\": {
    \"char\": \"❚\",
    \"latex\": \"\\\\ding{122}\",
    \"math\": null
  },
  \"10075\": {
    \"char\": \"❛\",
    \"latex\": \"\\\\ding{123}\",
    \"math\": null
  },
  \"10076\": {
    \"char\": \"❜\",
    \"latex\": \"\\\\ding{124}\",
    \"math\": null
  },
  \"10077\": {
    \"char\": \"❝\",
    \"latex\": \"\\\\ding{125}\",
    \"math\": null
  },
  \"10078\": {
    \"char\": \"❞\",
    \"latex\": \"\\\\ding{126}\",
    \"math\": null
  },
  \"10081\": {
    \"char\": \"❡\",
    \"latex\": \"\\\\ding{161}\",
    \"math\": null
  },
  \"10082\": {
    \"char\": \"❢\",
    \"latex\": \"\\\\ding{162}\",
    \"math\": null
  },
  \"10083\": {
    \"char\": \"❣\",
    \"latex\": \"\\\\ding{163}\",
    \"math\": null
  },
  \"10084\": {
    \"char\": \"❤\",
    \"latex\": \"\\\\ding{164}\",
    \"math\": null
  },
  \"10085\": {
    \"char\": \"❥\",
    \"latex\": \"\\\\ding{165}\",
    \"math\": null
  },
  \"10086\": {
    \"char\": \"❦\",
    \"latex\": \"\\\\ding{166}\",
    \"math\": null
  },
  \"10087\": {
    \"char\": \"❧\",
    \"latex\": \"\\\\ding{167}\",
    \"math\": null
  },
  \"10102\": {
    \"char\": \"❶\",
    \"latex\": \"\\\\ding{182}\",
    \"math\": null
  },
  \"10103\": {
    \"char\": \"❷\",
    \"latex\": \"\\\\ding{183}\",
    \"math\": null
  },
  \"10104\": {
    \"char\": \"❸\",
    \"latex\": \"\\\\ding{184}\",
    \"math\": null
  },
  \"10105\": {
    \"char\": \"❹\",
    \"latex\": \"\\\\ding{185}\",
    \"math\": null
  },
  \"10106\": {
    \"char\": \"❺\",
    \"latex\": \"\\\\ding{186}\",
    \"math\": null
  },
  \"10107\": {
    \"char\": \"❻\",
    \"latex\": \"\\\\ding{187}\",
    \"math\": null
  },
  \"10108\": {
    \"char\": \"❼\",
    \"latex\": \"\\\\ding{188}\",
    \"math\": null
  },
  \"10109\": {
    \"char\": \"❽\",
    \"latex\": \"\\\\ding{189}\",
    \"math\": null
  },
  \"10110\": {
    \"char\": \"❾\",
    \"latex\": \"\\\\ding{190}\",
    \"math\": null
  },
  \"10111\": {
    \"char\": \"❿\",
    \"latex\": \"\\\\ding{191}\",
    \"math\": null
  },
  \"10112\": {
    \"char\": \"➀\",
    \"latex\": \"\\\\ding{192}\",
    \"math\": null
  },
  \"10113\": {
    \"char\": \"➁\",
    \"latex\": \"\\\\ding{193}\",
    \"math\": null
  },
  \"10114\": {
    \"char\": \"➂\",
    \"latex\": \"\\\\ding{194}\",
    \"math\": null
  },
  \"10115\": {
    \"char\": \"➃\",
    \"latex\": \"\\\\ding{195}\",
    \"math\": null
  },
  \"10116\": {
    \"char\": \"➄\",
    \"latex\": \"\\\\ding{196}\",
    \"math\": null
  },
  \"10117\": {
    \"char\": \"➅\",
    \"latex\": \"\\\\ding{197}\",
    \"math\": null
  },
  \"10118\": {
    \"char\": \"➆\",
    \"latex\": \"\\\\ding{198}\",
    \"math\": null
  },
  \"10119\": {
    \"char\": \"➇\",
    \"latex\": \"\\\\ding{199}\",
    \"math\": null
  },
  \"10120\": {
    \"char\": \"➈\",
    \"latex\": \"\\\\ding{200}\",
    \"math\": null
  },
  \"10121\": {
    \"char\": \"➉\",
    \"latex\": \"\\\\ding{201}\",
    \"math\": null
  },
  \"10122\": {
    \"char\": \"➊\",
    \"latex\": \"\\\\ding{202}\",
    \"math\": null
  },
  \"10123\": {
    \"char\": \"➋\",
    \"latex\": \"\\\\ding{203}\",
    \"math\": null
  },
  \"10124\": {
    \"char\": \"➌\",
    \"latex\": \"\\\\ding{204}\",
    \"math\": null
  },
  \"10125\": {
    \"char\": \"➍\",
    \"latex\": \"\\\\ding{205}\",
    \"math\": null
  },
  \"10126\": {
    \"char\": \"➎\",
    \"latex\": \"\\\\ding{206}\",
    \"math\": null
  },
  \"10127\": {
    \"char\": \"➏\",
    \"latex\": \"\\\\ding{207}\",
    \"math\": null
  },
  \"10128\": {
    \"char\": \"➐\",
    \"latex\": \"\\\\ding{208}\",
    \"math\": null
  },
  \"10129\": {
    \"char\": \"➑\",
    \"latex\": \"\\\\ding{209}\",
    \"math\": null
  },
  \"10130\": {
    \"char\": \"➒\",
    \"latex\": \"\\\\ding{210}\",
    \"math\": null
  },
  \"10131\": {
    \"char\": \"➓\",
    \"latex\": \"\\\\ding{211}\",
    \"math\": null
  },
  \"10132\": {
    \"char\": \"➔\",
    \"latex\": \"\\\\ding{212}\",
    \"math\": null
  },
  \"10136\": {
    \"char\": \"➘\",
    \"latex\": \"\\\\ding{216}\",
    \"math\": null
  },
  \"10137\": {
    \"char\": \"➙\",
    \"latex\": \"\\\\ding{217}\",
    \"math\": null
  },
  \"10138\": {
    \"char\": \"➚\",
    \"latex\": \"\\\\ding{218}\",
    \"math\": null
  },
  \"10139\": {
    \"char\": \"➛\",
    \"latex\": \"\\\\ding{219}\",
    \"math\": null
  },
  \"10140\": {
    \"char\": \"➜\",
    \"latex\": \"\\\\ding{220}\",
    \"math\": null
  },
  \"10141\": {
    \"char\": \"➝\",
    \"latex\": \"\\\\ding{221}\",
    \"math\": null
  },
  \"10142\": {
    \"char\": \"➞\",
    \"latex\": \"\\\\ding{222}\",
    \"math\": null
  },
  \"10143\": {
    \"char\": \"➟\",
    \"latex\": \"\\\\ding{223}\",
    \"math\": null
  },
  \"10144\": {
    \"char\": \"➠\",
    \"latex\": \"\\\\ding{224}\",
    \"math\": null
  },
  \"10145\": {
    \"char\": \"➡\",
    \"latex\": \"\\\\ding{225}\",
    \"math\": null
  },
  \"10146\": {
    \"char\": \"➢\",
    \"latex\": \"\\\\ding{226}\",
    \"math\": null
  },
  \"10147\": {
    \"char\": \"➣\",
    \"latex\": \"\\\\ding{227}\",
    \"math\": null
  },
  \"10148\": {
    \"char\": \"➤\",
    \"latex\": \"\\\\ding{228}\",
    \"math\": null
  },
  \"10149\": {
    \"char\": \"➥\",
    \"latex\": \"\\\\ding{229}\",
    \"math\": null
  },
  \"10150\": {
    \"char\": \"➦\",
    \"latex\": \"\\\\ding{230}\",
    \"math\": null
  },
  \"10151\": {
    \"char\": \"➧\",
    \"latex\": \"\\\\ding{231}\",
    \"math\": null
  },
  \"10152\": {
    \"char\": \"➨\",
    \"latex\": \"\\\\ding{232}\",
    \"math\": null
  },
  \"10153\": {
    \"char\": \"➩\",
    \"latex\": \"\\\\ding{233}\",
    \"math\": null
  },
  \"10154\": {
    \"char\": \"➪\",
    \"latex\": \"\\\\ding{234}\",
    \"math\": null
  },
  \"10155\": {
    \"char\": \"➫\",
    \"latex\": \"\\\\ding{235}\",
    \"math\": null
  },
  \"10156\": {
    \"char\": \"➬\",
    \"latex\": \"\\\\ding{236}\",
    \"math\": null
  },
  \"10157\": {
    \"char\": \"➭\",
    \"latex\": \"\\\\ding{237}\",
    \"math\": null
  },
  \"10158\": {
    \"char\": \"➮\",
    \"latex\": \"\\\\ding{238}\",
    \"math\": null
  },
  \"10159\": {
    \"char\": \"➯\",
    \"latex\": \"\\\\ding{239}\",
    \"math\": null
  },
  \"10161\": {
    \"char\": \"➱\",
    \"latex\": \"\\\\ding{241}\",
    \"math\": null
  },
  \"10162\": {
    \"char\": \"➲\",
    \"latex\": \"\\\\ding{242}\",
    \"math\": null
  },
  \"10163\": {
    \"char\": \"➳\",
    \"latex\": \"\\\\ding{243}\",
    \"math\": null
  },
  \"10164\": {
    \"char\": \"➴\",
    \"latex\": \"\\\\ding{244}\",
    \"math\": null
  },
  \"10165\": {
    \"char\": \"➵\",
    \"latex\": \"\\\\ding{245}\",
    \"math\": null
  },
  \"10166\": {
    \"char\": \"➶\",
    \"latex\": \"\\\\ding{246}\",
    \"math\": null
  },
  \"10167\": {
    \"char\": \"➷\",
    \"latex\": \"\\\\ding{247}\",
    \"math\": null
  },
  \"10168\": {
    \"char\": \"➸\",
    \"latex\": \"\\\\ding{248}\",
    \"math\": null
  },
  \"10169\": {
    \"char\": \"➹\",
    \"latex\": \"\\\\ding{249}\",
    \"math\": null
  },
  \"10170\": {
    \"char\": \"➺\",
    \"latex\": \"\\\\ding{250}\",
    \"math\": null
  },
  \"10171\": {
    \"char\": \"➻\",
    \"latex\": \"\\\\ding{251}\",
    \"math\": null
  },
  \"10172\": {
    \"char\": \"➼\",
    \"latex\": \"\\\\ding{252}\",
    \"math\": null
  },
  \"10173\": {
    \"char\": \"➽\",
    \"latex\": \"\\\\ding{253}\",
    \"math\": null
  },
  \"10174\": {
    \"char\": \"➾\",
    \"latex\": \"\\\\ding{254}\",
    \"math\": null
  },
  \"10229\": {
    \"char\": \"⟵\",
    \"latex\": \"\\\\longleftarrow \",
    \"math\": null
  },
  \"10230\": {
    \"char\": \"⟶\",
    \"latex\": \"\\\\longrightarrow \",
    \"math\": null
  },
  \"10231\": {
    \"char\": \"⟷\",
    \"latex\": \"\\\\longleftrightarrow \",
    \"math\": null
  },
  \"10232\": {
    \"char\": \"⟸\",
    \"latex\": \"\\\\Longleftarrow \",
    \"math\": null
  },
  \"10233\": {
    \"char\": \"⟹\",
    \"latex\": \"\\\\Longrightarrow \",
    \"math\": null
  },
  \"10234\": {
    \"char\": \"⟺\",
    \"latex\": \"\\\\Longleftrightarrow \",
    \"math\": null
  },
  \"10236\": {
    \"char\": \"⟼\",
    \"latex\": \"\\\\longmapsto \",
    \"math\": null
  },
  \"10239\": {
    \"char\": \"⟿\",
    \"latex\": \"\\\\sim\\\\joinrel\\\\leadsto\",
    \"math\": null
  },
  \"10501\": {
    \"char\": \"⤅\",
    \"latex\": \"\\\\ElsevierGlyph{E212}\",
    \"math\": null
  },
  \"10514\": {
    \"char\": \"⤒\",
    \"latex\": \"\\\\UpArrowBar \",
    \"math\": null
  },
  \"10515\": {
    \"char\": \"⤓\",
    \"latex\": \"\\\\DownArrowBar \",
    \"math\": null
  },
  \"10531\": {
    \"char\": \"⤣\",
    \"latex\": \"\\\\ElsevierGlyph{E20C}\",
    \"math\": null
  },
  \"10532\": {
    \"char\": \"⤤\",
    \"latex\": \"\\\\ElsevierGlyph{E20D}\",
    \"math\": null
  },
  \"10533\": {
    \"char\": \"⤥\",
    \"latex\": \"\\\\ElsevierGlyph{E20B}\",
    \"math\": null
  },
  \"10534\": {
    \"char\": \"⤦\",
    \"latex\": \"\\\\ElsevierGlyph{E20A}\",
    \"math\": null
  },
  \"10535\": {
    \"char\": \"⤧\",
    \"latex\": \"\\\\ElsevierGlyph{E211}\",
    \"math\": null
  },
  \"10536\": {
    \"char\": \"⤨\",
    \"latex\": \"\\\\ElsevierGlyph{E20E}\",
    \"math\": null
  },
  \"10537\": {
    \"char\": \"⤩\",
    \"latex\": \"\\\\ElsevierGlyph{E20F}\",
    \"math\": null
  },
  \"10538\": {
    \"char\": \"⤪\",
    \"latex\": \"\\\\ElsevierGlyph{E210}\",
    \"math\": null
  },
  \"10547\": {
    \"char\": \"⤳\",
    \"latex\": \"\\\\ElsevierGlyph{E21C}\",
    \"math\": null
  },
  \"10550\": {
    \"char\": \"⤶\",
    \"latex\": \"\\\\ElsevierGlyph{E21A}\",
    \"math\": null
  },
  \"10551\": {
    \"char\": \"⤷\",
    \"latex\": \"\\\\ElsevierGlyph{E219}\",
    \"math\": null
  },
  \"10560\": {
    \"char\": \"⥀\",
    \"latex\": \"\\\\Elolarr \",
    \"math\": null
  },
  \"10561\": {
    \"char\": \"⥁\",
    \"latex\": \"\\\\Elorarr \",
    \"math\": null
  },
  \"10562\": {
    \"char\": \"⥂\",
    \"latex\": \"\\\\ElzRlarr \",
    \"math\": null
  },
  \"10564\": {
    \"char\": \"⥄\",
    \"latex\": \"\\\\ElzrLarr \",
    \"math\": null
  },
  \"10567\": {
    \"char\": \"⥇\",
    \"latex\": \"\\\\Elzrarrx \",
    \"math\": null
  },
  \"10574\": {
    \"char\": \"⥎\",
    \"latex\": \"\\\\LeftRightVector \",
    \"math\": null
  },
  \"10575\": {
    \"char\": \"⥏\",
    \"latex\": \"\\\\RightUpDownVector \",
    \"math\": null
  },
  \"10576\": {
    \"char\": \"⥐\",
    \"latex\": \"\\\\DownLeftRightVector \",
    \"math\": null
  },
  \"10577\": {
    \"char\": \"⥑\",
    \"latex\": \"\\\\LeftUpDownVector \",
    \"math\": null
  },
  \"10578\": {
    \"char\": \"⥒\",
    \"latex\": \"\\\\LeftVectorBar \",
    \"math\": null
  },
  \"10579\": {
    \"char\": \"⥓\",
    \"latex\": \"\\\\RightVectorBar \",
    \"math\": null
  },
  \"10580\": {
    \"char\": \"⥔\",
    \"latex\": \"\\\\RightUpVectorBar \",
    \"math\": null
  },
  \"10581\": {
    \"char\": \"⥕\",
    \"latex\": \"\\\\RightDownVectorBar \",
    \"math\": null
  },
  \"10582\": {
    \"char\": \"⥖\",
    \"latex\": \"\\\\DownLeftVectorBar \",
    \"math\": null
  },
  \"10583\": {
    \"char\": \"⥗\",
    \"latex\": \"\\\\DownRightVectorBar \",
    \"math\": null
  },
  \"10584\": {
    \"char\": \"⥘\",
    \"latex\": \"\\\\LeftUpVectorBar \",
    \"math\": null
  },
  \"10585\": {
    \"char\": \"⥙\",
    \"latex\": \"\\\\LeftDownVectorBar \",
    \"math\": null
  },
  \"10586\": {
    \"char\": \"⥚\",
    \"latex\": \"\\\\LeftTeeVector \",
    \"math\": null
  },
  \"10587\": {
    \"char\": \"⥛\",
    \"latex\": \"\\\\RightTeeVector \",
    \"math\": null
  },
  \"10588\": {
    \"char\": \"⥜\",
    \"latex\": \"\\\\RightUpTeeVector \",
    \"math\": null
  },
  \"10589\": {
    \"char\": \"⥝\",
    \"latex\": \"\\\\RightDownTeeVector \",
    \"math\": null
  },
  \"10590\": {
    \"char\": \"⥞\",
    \"latex\": \"\\\\DownLeftTeeVector \",
    \"math\": null
  },
  \"10591\": {
    \"char\": \"⥟\",
    \"latex\": \"\\\\DownRightTeeVector \",
    \"math\": null
  },
  \"10592\": {
    \"char\": \"⥠\",
    \"latex\": \"\\\\LeftUpTeeVector \",
    \"math\": null
  },
  \"10593\": {
    \"char\": \"⥡\",
    \"latex\": \"\\\\LeftDownTeeVector \",
    \"math\": null
  },
  \"10606\": {
    \"char\": \"⥮\",
    \"latex\": \"\\\\UpEquilibrium \",
    \"math\": null
  },
  \"10607\": {
    \"char\": \"⥯\",
    \"latex\": \"\\\\ReverseUpEquilibrium \",
    \"math\": null
  },
  \"10608\": {
    \"char\": \"⥰\",
    \"latex\": \"\\\\RoundImplies \",
    \"math\": null
  },
  \"10620\": {
    \"char\": \"⥼\",
    \"latex\": \"\\\\ElsevierGlyph{E214}\",
    \"math\": null
  },
  \"10621\": {
    \"char\": \"⥽\",
    \"latex\": \"\\\\ElsevierGlyph{E215}\",
    \"math\": null
  },
  \"10624\": {
    \"char\": \"⦀\",
    \"latex\": \"\\\\Elztfnc \",
    \"math\": null
  },
  \"10629\": {
    \"char\": \"⦅\",
    \"latex\": \"\\\\ElsevierGlyph{3018}\",
    \"math\": null
  },
  \"10630\": {
    \"char\": \"⦆\",
    \"latex\": \"\\\\Elroang \",
    \"math\": null
  },
  \"10643\": {
    \"char\": \"⦓\",
    \"latex\": \"<\\\\kern-0.58em(\",
    \"math\": null
  },
  \"10644\": {
    \"char\": \"⦔\",
    \"latex\": \"\\\\ElsevierGlyph{E291}\",
    \"math\": null
  },
  \"10649\": {
    \"char\": \"⦙\",
    \"latex\": \"\\\\Elzddfnc \",
    \"math\": null
  },
  \"10652\": {
    \"char\": \"⦜\",
    \"latex\": \"\\\\Angle \",
    \"math\": null
  },
  \"10656\": {
    \"char\": \"⦠\",
    \"latex\": \"\\\\Elzlpargt \",
    \"math\": null
  },
  \"10677\": {
    \"char\": \"⦵\",
    \"latex\": \"\\\\ElsevierGlyph{E260}\",
    \"math\": null
  },
  \"10678\": {
    \"char\": \"⦶\",
    \"latex\": \"\\\\ElsevierGlyph{E61B}\",
    \"math\": null
  },
  \"10698\": {
    \"char\": \"⧊\",
    \"latex\": \"\\\\ElzLap \",
    \"math\": null
  },
  \"10699\": {
    \"char\": \"⧋\",
    \"latex\": \"\\\\Elzdefas \",
    \"math\": null
  },
  \"10703\": {
    \"char\": \"⧏\",
    \"latex\": \"\\\\LeftTriangleBar \",
    \"math\": null
  },
  \"10704\": {
    \"char\": \"⧐\",
    \"latex\": \"\\\\RightTriangleBar \",
    \"math\": null
  },
  \"10716\": {
    \"char\": \"⧜\",
    \"latex\": \"\\\\ElsevierGlyph{E372}\",
    \"math\": null
  },
  \"10731\": {
    \"char\": \"⧫\",
    \"latex\": \"\\\\blacklozenge \",
    \"math\": null
  },
  \"10740\": {
    \"char\": \"⧴\",
    \"latex\": \"\\\\RuleDelayed \",
    \"math\": null
  },
  \"10756\": {
    \"char\": \"⨄\",
    \"latex\": \"\\\\Elxuplus \",
    \"math\": null
  },
  \"10757\": {
    \"char\": \"⨅\",
    \"latex\": \"\\\\ElzThr \",
    \"math\": null
  },
  \"10758\": {
    \"char\": \"⨆\",
    \"latex\": \"\\\\Elxsqcup \",
    \"math\": null
  },
  \"10759\": {
    \"char\": \"⨇\",
    \"latex\": \"\\\\ElzInf \",
    \"math\": null
  },
  \"10760\": {
    \"char\": \"⨈\",
    \"latex\": \"\\\\ElzSup \",
    \"math\": null
  },
  \"10765\": {
    \"char\": \"⨍\",
    \"latex\": \"\\\\ElzCint \",
    \"math\": null
  },
  \"10767\": {
    \"char\": \"⨏\",
    \"latex\": \"\\\\clockoint \",
    \"math\": null
  },
  \"10768\": {
    \"char\": \"⨐\",
    \"latex\": \"\\\\ElsevierGlyph{E395}\",
    \"math\": null
  },
  \"10774\": {
    \"char\": \"⨖\",
    \"latex\": \"\\\\sqrint \",
    \"math\": null
  },
  \"10789\": {
    \"char\": \"⨥\",
    \"latex\": \"\\\\ElsevierGlyph{E25A}\",
    \"math\": null
  },
  \"10794\": {
    \"char\": \"⨪\",
    \"latex\": \"\\\\ElsevierGlyph{E25B}\",
    \"math\": null
  },
  \"10797\": {
    \"char\": \"⨭\",
    \"latex\": \"\\\\ElsevierGlyph{E25C}\",
    \"math\": null
  },
  \"10798\": {
    \"char\": \"⨮\",
    \"latex\": \"\\\\ElsevierGlyph{E25D}\",
    \"math\": null
  },
  \"10799\": {
    \"char\": \"⨯\",
    \"latex\": \"\\\\ElzTimes \",
    \"math\": null
  },
  \"10804\": {
    \"char\": \"⨴\",
    \"latex\": \"\\\\ElsevierGlyph{E25E}\",
    \"math\": null
  },
  \"10805\": {
    \"char\": \"⨵\",
    \"latex\": \"\\\\ElsevierGlyph{E25E}\",
    \"math\": null
  },
  \"10812\": {
    \"char\": \"⨼\",
    \"latex\": \"\\\\ElsevierGlyph{E259}\",
    \"math\": null
  },
  \"10815\": {
    \"char\": \"⨿\",
    \"latex\": \"\\\\amalg \",
    \"math\": null
  },
  \"10835\": {
    \"char\": \"⩓\",
    \"latex\": \"\\\\ElzAnd \",
    \"math\": null
  },
  \"10836\": {
    \"char\": \"⩔\",
    \"latex\": \"\\\\ElzOr \",
    \"math\": null
  },
  \"10837\": {
    \"char\": \"⩕\",
    \"latex\": \"\\\\ElsevierGlyph{E36E}\",
    \"math\": null
  },
  \"10838\": {
    \"char\": \"⩖\",
    \"latex\": \"\\\\ElOr \",
    \"math\": null
  },
  \"10846\": {
    \"char\": \"⩞\",
    \"latex\": \"\\\\perspcorrespond \",
    \"math\": null
  },
  \"10847\": {
    \"char\": \"⩟\",
    \"latex\": \"\\\\Elzminhat \",
    \"math\": null
  },
  \"10851\": {
    \"char\": \"⩣\",
    \"latex\": \"\\\\ElsevierGlyph{225A}\",
    \"math\": null
  },
  \"10862\": {
    \"char\": \"⩮\",
    \"latex\": \"\\\\stackrel{*}{=}\",
    \"math\": null
  },
  \"10869\": {
    \"char\": \"⩵\",
    \"latex\": \"\\\\Equal \",
    \"math\": null
  },
  \"10877\": {
    \"char\": \"⩽\",
    \"latex\": \"\\\\leqslant \",
    \"math\": null
  },
  \"10878\": {
    \"char\": \"⩾\",
    \"latex\": \"\\\\geqslant \",
    \"math\": null
  },
  \"10885\": {
    \"char\": \"⪅\",
    \"latex\": \"\\\\lessapprox \",
    \"math\": null
  },
  \"10886\": {
    \"char\": \"⪆\",
    \"latex\": \"\\\\gtrapprox \",
    \"math\": null
  },
  \"10887\": {
    \"char\": \"⪇\",
    \"latex\": \"\\\\lneq \",
    \"math\": null
  },
  \"10888\": {
    \"char\": \"⪈\",
    \"latex\": \"\\\\gneq \",
    \"math\": null
  },
  \"10889\": {
    \"char\": \"⪉\",
    \"latex\": \"\\\\lnapprox \",
    \"math\": null
  },
  \"10890\": {
    \"char\": \"⪊\",
    \"latex\": \"\\\\gnapprox \",
    \"math\": null
  },
  \"10891\": {
    \"char\": \"⪋\",
    \"latex\": \"\\\\lesseqqgtr \",
    \"math\": null
  },
  \"10892\": {
    \"char\": \"⪌\",
    \"latex\": \"\\\\gtreqqless \",
    \"math\": null
  },
  \"10901\": {
    \"char\": \"⪕\",
    \"latex\": \"\\\\eqslantless \",
    \"math\": null
  },
  \"10902\": {
    \"char\": \"⪖\",
    \"latex\": \"\\\\eqslantgtr \",
    \"math\": null
  },
  \"10909\": {
    \"char\": \"⪝\",
    \"latex\": \"\\\\Pisymbol{ppi020}{117}\",
    \"math\": null
  },
  \"10910\": {
    \"char\": \"⪞\",
    \"latex\": \"\\\\Pisymbol{ppi020}{105}\",
    \"math\": null
  },
  \"10913\": {
    \"char\": \"⪡\",
    \"latex\": \"\\\\NestedLessLess \",
    \"math\": null
  },
  \"10914\": {
    \"char\": \"⪢\",
    \"latex\": \"\\\\NestedGreaterGreater \",
    \"math\": null
  },
  \"10927\": {
    \"char\": \"⪯\",
    \"latex\": \"\\\\preceq \",
    \"math\": null
  },
  \"10928\": {
    \"char\": \"⪰\",
    \"latex\": \"\\\\succeq \",
    \"math\": null
  },
  \"10933\": {
    \"char\": \"⪵\",
    \"latex\": \"\\\\precneqq \",
    \"math\": null
  },
  \"10934\": {
    \"char\": \"⪶\",
    \"latex\": \"\\\\succneqq \",
    \"math\": null
  },
  \"10935\": {
    \"char\": \"⪷\",
    \"latex\": \"\\\\precapprox \",
    \"math\": null
  },
  \"10936\": {
    \"char\": \"⪸\",
    \"latex\": \"\\\\succapprox \",
    \"math\": null
  },
  \"10937\": {
    \"char\": \"⪹\",
    \"latex\": \"\\\\precnapprox \",
    \"math\": null
  },
  \"10938\": {
    \"char\": \"⪺\",
    \"latex\": \"\\\\succnapprox \",
    \"math\": null
  },
  \"10949\": {
    \"char\": \"⫅\",
    \"latex\": \"\\\\subseteqq \",
    \"math\": null
  },
  \"10950\": {
    \"char\": \"⫆\",
    \"latex\": \"\\\\supseteqq \",
    \"math\": null
  },
  \"10955\": {
    \"char\": \"⫋\",
    \"latex\": \"\\\\subsetneqq \",
    \"math\": null
  },
  \"10956\": {
    \"char\": \"⫌\",
    \"latex\": \"\\\\supsetneqq \",
    \"math\": null
  },
  \"10987\": {
    \"char\": \"⫫\",
    \"latex\": \"\\\\ElsevierGlyph{E30D}\",
    \"math\": null
  },
  \"10998\": {
    \"char\": \"⫶\",
    \"latex\": \"\\\\Elztdcol \",
    \"math\": null
  },
  \"11005\": {
    \"char\": \"⫽\",
    \"latex\": \"{{/}\\\\!\\\\!{/}}\",
    \"math\": null
  },
  \"12298\": {
    \"char\": \"《\",
    \"latex\": \"\\\\ElsevierGlyph{300A}\",
    \"math\": null
  },
  \"12299\": {
    \"char\": \"》\",
    \"latex\": \"\\\\ElsevierGlyph{300B}\",
    \"math\": null
  },
  \"12312\": {
    \"char\": \"〘\",
    \"latex\": \"\\\\ElsevierGlyph{3018}\",
    \"math\": null
  },
  \"12313\": {
    \"char\": \"〙\",
    \"latex\": \"\\\\ElsevierGlyph{3019}\",
    \"math\": null
  },
  \"12314\": {
    \"char\": \"〚\",
    \"latex\": \"\\\\openbracketleft \",
    \"math\": null
  },
  \"12315\": {
    \"char\": \"〛\",
    \"latex\": \"\\\\openbracketright \",
    \"math\": null
  },
  \"64256\": {
    \"char\": \"ﬀ\",
    \"latex\": \"ff\",
    \"math\": null
  },
  \"64257\": {
    \"char\": \"ﬁ\",
    \"latex\": \"fi\",
    \"math\": null
  },
  \"64258\": {
    \"char\": \"ﬂ\",
    \"latex\": \"fl\",
    \"math\": null
  },
  \"64259\": {
    \"char\": \"ﬃ\",
    \"latex\": \"ffi\",
    \"math\": null
  },
  \"64260\": {
    \"char\": \"ﬄ\",
    \"latex\": \"ffl\",
    \"math\": null
  },
  \"119808\": {
    \"char\": \"𝐀\",
    \"latex\": \"\\\\mathbf{A}\",
    \"math\": null
  },
  \"119809\": {
    \"char\": \"𝐁\",
    \"latex\": \"\\\\mathbf{B}\",
    \"math\": null
  },
  \"119810\": {
    \"char\": \"𝐂\",
    \"latex\": \"\\\\mathbf{C}\",
    \"math\": null
  },
  \"119811\": {
    \"char\": \"𝐃\",
    \"latex\": \"\\\\mathbf{D}\",
    \"math\": null
  },
  \"119812\": {
    \"char\": \"𝐄\",
    \"latex\": \"\\\\mathbf{E}\",
    \"math\": null
  },
  \"119813\": {
    \"char\": \"𝐅\",
    \"latex\": \"\\\\mathbf{F}\",
    \"math\": null
  },
  \"119814\": {
    \"char\": \"𝐆\",
    \"latex\": \"\\\\mathbf{G}\",
    \"math\": null
  },
  \"119815\": {
    \"char\": \"𝐇\",
    \"latex\": \"\\\\mathbf{H}\",
    \"math\": null
  },
  \"119816\": {
    \"char\": \"𝐈\",
    \"latex\": \"\\\\mathbf{I}\",
    \"math\": null
  },
  \"119817\": {
    \"char\": \"𝐉\",
    \"latex\": \"\\\\mathbf{J}\",
    \"math\": null
  },
  \"119818\": {
    \"char\": \"𝐊\",
    \"latex\": \"\\\\mathbf{K}\",
    \"math\": null
  },
  \"119819\": {
    \"char\": \"𝐋\",
    \"latex\": \"\\\\mathbf{L}\",
    \"math\": null
  },
  \"119820\": {
    \"char\": \"𝐌\",
    \"latex\": \"\\\\mathbf{M}\",
    \"math\": null
  },
  \"119821\": {
    \"char\": \"𝐍\",
    \"latex\": \"\\\\mathbf{N}\",
    \"math\": null
  },
  \"119822\": {
    \"char\": \"𝐎\",
    \"latex\": \"\\\\mathbf{O}\",
    \"math\": null
  },
  \"119823\": {
    \"char\": \"𝐏\",
    \"latex\": \"\\\\mathbf{P}\",
    \"math\": null
  },
  \"119824\": {
    \"char\": \"𝐐\",
    \"latex\": \"\\\\mathbf{Q}\",
    \"math\": null
  },
  \"119825\": {
    \"char\": \"𝐑\",
    \"latex\": \"\\\\mathbf{R}\",
    \"math\": null
  },
  \"119826\": {
    \"char\": \"𝐒\",
    \"latex\": \"\\\\mathbf{S}\",
    \"math\": null
  },
  \"119827\": {
    \"char\": \"𝐓\",
    \"latex\": \"\\\\mathbf{T}\",
    \"math\": null
  },
  \"119828\": {
    \"char\": \"𝐔\",
    \"latex\": \"\\\\mathbf{U}\",
    \"math\": null
  },
  \"119829\": {
    \"char\": \"𝐕\",
    \"latex\": \"\\\\mathbf{V}\",
    \"math\": null
  },
  \"119830\": {
    \"char\": \"𝐖\",
    \"latex\": \"\\\\mathbf{W}\",
    \"math\": null
  },
  \"119831\": {
    \"char\": \"𝐗\",
    \"latex\": \"\\\\mathbf{X}\",
    \"math\": null
  },
  \"119832\": {
    \"char\": \"𝐘\",
    \"latex\": \"\\\\mathbf{Y}\",
    \"math\": null
  },
  \"119833\": {
    \"char\": \"𝐙\",
    \"latex\": \"\\\\mathbf{Z}\",
    \"math\": null
  },
  \"119834\": {
    \"char\": \"𝐚\",
    \"latex\": \"\\\\mathbf{a}\",
    \"math\": null
  },
  \"119835\": {
    \"char\": \"𝐛\",
    \"latex\": \"\\\\mathbf{b}\",
    \"math\": null
  },
  \"119836\": {
    \"char\": \"𝐜\",
    \"latex\": \"\\\\mathbf{c}\",
    \"math\": null
  },
  \"119837\": {
    \"char\": \"𝐝\",
    \"latex\": \"\\\\mathbf{d}\",
    \"math\": null
  },
  \"119838\": {
    \"char\": \"𝐞\",
    \"latex\": \"\\\\mathbf{e}\",
    \"math\": null
  },
  \"119839\": {
    \"char\": \"𝐟\",
    \"latex\": \"\\\\mathbf{f}\",
    \"math\": null
  },
  \"119840\": {
    \"char\": \"𝐠\",
    \"latex\": \"\\\\mathbf{g}\",
    \"math\": null
  },
  \"119841\": {
    \"char\": \"𝐡\",
    \"latex\": \"\\\\mathbf{h}\",
    \"math\": null
  },
  \"119842\": {
    \"char\": \"𝐢\",
    \"latex\": \"\\\\mathbf{i}\",
    \"math\": null
  },
  \"119843\": {
    \"char\": \"𝐣\",
    \"latex\": \"\\\\mathbf{j}\",
    \"math\": null
  },
  \"119844\": {
    \"char\": \"𝐤\",
    \"latex\": \"\\\\mathbf{k}\",
    \"math\": null
  },
  \"119845\": {
    \"char\": \"𝐥\",
    \"latex\": \"\\\\mathbf{l}\",
    \"math\": null
  },
  \"119846\": {
    \"char\": \"𝐦\",
    \"latex\": \"\\\\mathbf{m}\",
    \"math\": null
  },
  \"119847\": {
    \"char\": \"𝐧\",
    \"latex\": \"\\\\mathbf{n}\",
    \"math\": null
  },
  \"119848\": {
    \"char\": \"𝐨\",
    \"latex\": \"\\\\mathbf{o}\",
    \"math\": null
  },
  \"119849\": {
    \"char\": \"𝐩\",
    \"latex\": \"\\\\mathbf{p}\",
    \"math\": null
  },
  \"119850\": {
    \"char\": \"𝐪\",
    \"latex\": \"\\\\mathbf{q}\",
    \"math\": null
  },
  \"119851\": {
    \"char\": \"𝐫\",
    \"latex\": \"\\\\mathbf{r}\",
    \"math\": null
  },
  \"119852\": {
    \"char\": \"𝐬\",
    \"latex\": \"\\\\mathbf{s}\",
    \"math\": null
  },
  \"119853\": {
    \"char\": \"𝐭\",
    \"latex\": \"\\\\mathbf{t}\",
    \"math\": null
  },
  \"119854\": {
    \"char\": \"𝐮\",
    \"latex\": \"\\\\mathbf{u}\",
    \"math\": null
  },
  \"119855\": {
    \"char\": \"𝐯\",
    \"latex\": \"\\\\mathbf{v}\",
    \"math\": null
  },
  \"119856\": {
    \"char\": \"𝐰\",
    \"latex\": \"\\\\mathbf{w}\",
    \"math\": null
  },
  \"119857\": {
    \"char\": \"𝐱\",
    \"latex\": \"\\\\mathbf{x}\",
    \"math\": null
  },
  \"119858\": {
    \"char\": \"𝐲\",
    \"latex\": \"\\\\mathbf{y}\",
    \"math\": null
  },
  \"119859\": {
    \"char\": \"𝐳\",
    \"latex\": \"\\\\mathbf{z}\",
    \"math\": null
  },
  \"119860\": {
    \"char\": \"𝐴\",
    \"latex\": \"\\\\mathsl{A}\",
    \"math\": null
  },
  \"119861\": {
    \"char\": \"𝐵\",
    \"latex\": \"\\\\mathsl{B}\",
    \"math\": null
  },
  \"119862\": {
    \"char\": \"𝐶\",
    \"latex\": \"\\\\mathsl{C}\",
    \"math\": null
  },
  \"119863\": {
    \"char\": \"𝐷\",
    \"latex\": \"\\\\mathsl{D}\",
    \"math\": null
  },
  \"119864\": {
    \"char\": \"𝐸\",
    \"latex\": \"\\\\mathsl{E}\",
    \"math\": null
  },
  \"119865\": {
    \"char\": \"𝐹\",
    \"latex\": \"\\\\mathsl{F}\",
    \"math\": null
  },
  \"119866\": {
    \"char\": \"𝐺\",
    \"latex\": \"\\\\mathsl{G}\",
    \"math\": null
  },
  \"119867\": {
    \"char\": \"𝐻\",
    \"latex\": \"\\\\mathsl{H}\",
    \"math\": null
  },
  \"119868\": {
    \"char\": \"𝐼\",
    \"latex\": \"\\\\mathsl{I}\",
    \"math\": null
  },
  \"119869\": {
    \"char\": \"𝐽\",
    \"latex\": \"\\\\mathsl{J}\",
    \"math\": null
  },
  \"119870\": {
    \"char\": \"𝐾\",
    \"latex\": \"\\\\mathsl{K}\",
    \"math\": null
  },
  \"119871\": {
    \"char\": \"𝐿\",
    \"latex\": \"\\\\mathsl{L}\",
    \"math\": null
  },
  \"119872\": {
    \"char\": \"𝑀\",
    \"latex\": \"\\\\mathsl{M}\",
    \"math\": null
  },
  \"119873\": {
    \"char\": \"𝑁\",
    \"latex\": \"\\\\mathsl{N}\",
    \"math\": null
  },
  \"119874\": {
    \"char\": \"𝑂\",
    \"latex\": \"\\\\mathsl{O}\",
    \"math\": null
  },
  \"119875\": {
    \"char\": \"𝑃\",
    \"latex\": \"\\\\mathsl{P}\",
    \"math\": null
  },
  \"119876\": {
    \"char\": \"𝑄\",
    \"latex\": \"\\\\mathsl{Q}\",
    \"math\": null
  },
  \"119877\": {
    \"char\": \"𝑅\",
    \"latex\": \"\\\\mathsl{R}\",
    \"math\": null
  },
  \"119878\": {
    \"char\": \"𝑆\",
    \"latex\": \"\\\\mathsl{S}\",
    \"math\": null
  },
  \"119879\": {
    \"char\": \"𝑇\",
    \"latex\": \"\\\\mathsl{T}\",
    \"math\": null
  },
  \"119880\": {
    \"char\": \"𝑈\",
    \"latex\": \"\\\\mathsl{U}\",
    \"math\": null
  },
  \"119881\": {
    \"char\": \"𝑉\",
    \"latex\": \"\\\\mathsl{V}\",
    \"math\": null
  },
  \"119882\": {
    \"char\": \"𝑊\",
    \"latex\": \"\\\\mathsl{W}\",
    \"math\": null
  },
  \"119883\": {
    \"char\": \"𝑋\",
    \"latex\": \"\\\\mathsl{X}\",
    \"math\": null
  },
  \"119884\": {
    \"char\": \"𝑌\",
    \"latex\": \"\\\\mathsl{Y}\",
    \"math\": null
  },
  \"119885\": {
    \"char\": \"𝑍\",
    \"latex\": \"\\\\mathsl{Z}\",
    \"math\": null
  },
  \"119886\": {
    \"char\": \"𝑎\",
    \"latex\": \"\\\\mathsl{a}\",
    \"math\": null
  },
  \"119887\": {
    \"char\": \"𝑏\",
    \"latex\": \"\\\\mathsl{b}\",
    \"math\": null
  },
  \"119888\": {
    \"char\": \"𝑐\",
    \"latex\": \"\\\\mathsl{c}\",
    \"math\": null
  },
  \"119889\": {
    \"char\": \"𝑑\",
    \"latex\": \"\\\\mathsl{d}\",
    \"math\": null
  },
  \"119890\": {
    \"char\": \"𝑒\",
    \"latex\": \"\\\\mathsl{e}\",
    \"math\": null
  },
  \"119891\": {
    \"char\": \"𝑓\",
    \"latex\": \"\\\\mathsl{f}\",
    \"math\": null
  },
  \"119892\": {
    \"char\": \"𝑔\",
    \"latex\": \"\\\\mathsl{g}\",
    \"math\": null
  },
  \"119894\": {
    \"char\": \"𝑖\",
    \"latex\": \"\\\\mathsl{i}\",
    \"math\": null
  },
  \"119895\": {
    \"char\": \"𝑗\",
    \"latex\": \"\\\\mathsl{j}\",
    \"math\": null
  },
  \"119896\": {
    \"char\": \"𝑘\",
    \"latex\": \"\\\\mathsl{k}\",
    \"math\": null
  },
  \"119897\": {
    \"char\": \"𝑙\",
    \"latex\": \"\\\\mathsl{l}\",
    \"math\": null
  },
  \"119898\": {
    \"char\": \"𝑚\",
    \"latex\": \"\\\\mathsl{m}\",
    \"math\": null
  },
  \"119899\": {
    \"char\": \"𝑛\",
    \"latex\": \"\\\\mathsl{n}\",
    \"math\": null
  },
  \"119900\": {
    \"char\": \"𝑜\",
    \"latex\": \"\\\\mathsl{o}\",
    \"math\": null
  },
  \"119901\": {
    \"char\": \"𝑝\",
    \"latex\": \"\\\\mathsl{p}\",
    \"math\": null
  },
  \"119902\": {
    \"char\": \"𝑞\",
    \"latex\": \"\\\\mathsl{q}\",
    \"math\": null
  },
  \"119903\": {
    \"char\": \"𝑟\",
    \"latex\": \"\\\\mathsl{r}\",
    \"math\": null
  },
  \"119904\": {
    \"char\": \"𝑠\",
    \"latex\": \"\\\\mathsl{s}\",
    \"math\": null
  },
  \"119905\": {
    \"char\": \"𝑡\",
    \"latex\": \"\\\\mathsl{t}\",
    \"math\": null
  },
  \"119906\": {
    \"char\": \"𝑢\",
    \"latex\": \"\\\\mathsl{u}\",
    \"math\": null
  },
  \"119907\": {
    \"char\": \"𝑣\",
    \"latex\": \"\\\\mathsl{v}\",
    \"math\": null
  },
  \"119908\": {
    \"char\": \"𝑤\",
    \"latex\": \"\\\\mathsl{w}\",
    \"math\": null
  },
  \"119909\": {
    \"char\": \"𝑥\",
    \"latex\": \"\\\\mathsl{x}\",
    \"math\": null
  },
  \"119910\": {
    \"char\": \"𝑦\",
    \"latex\": \"\\\\mathsl{y}\",
    \"math\": null
  },
  \"119911\": {
    \"char\": \"𝑧\",
    \"latex\": \"\\\\mathsl{z}\",
    \"math\": null
  },
  \"119912\": {
    \"char\": \"𝑨\",
    \"latex\": \"\\\\mathbit{A}\",
    \"math\": null
  },
  \"119913\": {
    \"char\": \"𝑩\",
    \"latex\": \"\\\\mathbit{B}\",
    \"math\": null
  },
  \"119914\": {
    \"char\": \"𝑪\",
    \"latex\": \"\\\\mathbit{C}\",
    \"math\": null
  },
  \"119915\": {
    \"char\": \"𝑫\",
    \"latex\": \"\\\\mathbit{D}\",
    \"math\": null
  },
  \"119916\": {
    \"char\": \"𝑬\",
    \"latex\": \"\\\\mathbit{E}\",
    \"math\": null
  },
  \"119917\": {
    \"char\": \"𝑭\",
    \"latex\": \"\\\\mathbit{F}\",
    \"math\": null
  },
  \"119918\": {
    \"char\": \"𝑮\",
    \"latex\": \"\\\\mathbit{G}\",
    \"math\": null
  },
  \"119919\": {
    \"char\": \"𝑯\",
    \"latex\": \"\\\\mathbit{H}\",
    \"math\": null
  },
  \"119920\": {
    \"char\": \"𝑰\",
    \"latex\": \"\\\\mathbit{I}\",
    \"math\": null
  },
  \"119921\": {
    \"char\": \"𝑱\",
    \"latex\": \"\\\\mathbit{J}\",
    \"math\": null
  },
  \"119922\": {
    \"char\": \"𝑲\",
    \"latex\": \"\\\\mathbit{K}\",
    \"math\": null
  },
  \"119923\": {
    \"char\": \"𝑳\",
    \"latex\": \"\\\\mathbit{L}\",
    \"math\": null
  },
  \"119924\": {
    \"char\": \"𝑴\",
    \"latex\": \"\\\\mathbit{M}\",
    \"math\": null
  },
  \"119925\": {
    \"char\": \"𝑵\",
    \"latex\": \"\\\\mathbit{N}\",
    \"math\": null
  },
  \"119926\": {
    \"char\": \"𝑶\",
    \"latex\": \"\\\\mathbit{O}\",
    \"math\": null
  },
  \"119927\": {
    \"char\": \"𝑷\",
    \"latex\": \"\\\\mathbit{P}\",
    \"math\": null
  },
  \"119928\": {
    \"char\": \"𝑸\",
    \"latex\": \"\\\\mathbit{Q}\",
    \"math\": null
  },
  \"119929\": {
    \"char\": \"𝑹\",
    \"latex\": \"\\\\mathbit{R}\",
    \"math\": null
  },
  \"119930\": {
    \"char\": \"𝑺\",
    \"latex\": \"\\\\mathbit{S}\",
    \"math\": null
  },
  \"119931\": {
    \"char\": \"𝑻\",
    \"latex\": \"\\\\mathbit{T}\",
    \"math\": null
  },
  \"119932\": {
    \"char\": \"𝑼\",
    \"latex\": \"\\\\mathbit{U}\",
    \"math\": null
  },
  \"119933\": {
    \"char\": \"𝑽\",
    \"latex\": \"\\\\mathbit{V}\",
    \"math\": null
  },
  \"119934\": {
    \"char\": \"𝑾\",
    \"latex\": \"\\\\mathbit{W}\",
    \"math\": null
  },
  \"119935\": {
    \"char\": \"𝑿\",
    \"latex\": \"\\\\mathbit{X}\",
    \"math\": null
  },
  \"119936\": {
    \"char\": \"𝒀\",
    \"latex\": \"\\\\mathbit{Y}\",
    \"math\": null
  },
  \"119937\": {
    \"char\": \"𝒁\",
    \"latex\": \"\\\\mathbit{Z}\",
    \"math\": null
  },
  \"119938\": {
    \"char\": \"𝒂\",
    \"latex\": \"\\\\mathbit{a}\",
    \"math\": null
  },
  \"119939\": {
    \"char\": \"𝒃\",
    \"latex\": \"\\\\mathbit{b}\",
    \"math\": null
  },
  \"119940\": {
    \"char\": \"𝒄\",
    \"latex\": \"\\\\mathbit{c}\",
    \"math\": null
  },
  \"119941\": {
    \"char\": \"𝒅\",
    \"latex\": \"\\\\mathbit{d}\",
    \"math\": null
  },
  \"119942\": {
    \"char\": \"𝒆\",
    \"latex\": \"\\\\mathbit{e}\",
    \"math\": null
  },
  \"119943\": {
    \"char\": \"𝒇\",
    \"latex\": \"\\\\mathbit{f}\",
    \"math\": null
  },
  \"119944\": {
    \"char\": \"𝒈\",
    \"latex\": \"\\\\mathbit{g}\",
    \"math\": null
  },
  \"119945\": {
    \"char\": \"𝒉\",
    \"latex\": \"\\\\mathbit{h}\",
    \"math\": null
  },
  \"119946\": {
    \"char\": \"𝒊\",
    \"latex\": \"\\\\mathbit{i}\",
    \"math\": null
  },
  \"119947\": {
    \"char\": \"𝒋\",
    \"latex\": \"\\\\mathbit{j}\",
    \"math\": null
  },
  \"119948\": {
    \"char\": \"𝒌\",
    \"latex\": \"\\\\mathbit{k}\",
    \"math\": null
  },
  \"119949\": {
    \"char\": \"𝒍\",
    \"latex\": \"\\\\mathbit{l}\",
    \"math\": null
  },
  \"119950\": {
    \"char\": \"𝒎\",
    \"latex\": \"\\\\mathbit{m}\",
    \"math\": null
  },
  \"119951\": {
    \"char\": \"𝒏\",
    \"latex\": \"\\\\mathbit{n}\",
    \"math\": null
  },
  \"119952\": {
    \"char\": \"𝒐\",
    \"latex\": \"\\\\mathbit{o}\",
    \"math\": null
  },
  \"119953\": {
    \"char\": \"𝒑\",
    \"latex\": \"\\\\mathbit{p}\",
    \"math\": null
  },
  \"119954\": {
    \"char\": \"𝒒\",
    \"latex\": \"\\\\mathbit{q}\",
    \"math\": null
  },
  \"119955\": {
    \"char\": \"𝒓\",
    \"latex\": \"\\\\mathbit{r}\",
    \"math\": null
  },
  \"119956\": {
    \"char\": \"𝒔\",
    \"latex\": \"\\\\mathbit{s}\",
    \"math\": null
  },
  \"119957\": {
    \"char\": \"𝒕\",
    \"latex\": \"\\\\mathbit{t}\",
    \"math\": null
  },
  \"119958\": {
    \"char\": \"𝒖\",
    \"latex\": \"\\\\mathbit{u}\",
    \"math\": null
  },
  \"119959\": {
    \"char\": \"𝒗\",
    \"latex\": \"\\\\mathbit{v}\",
    \"math\": null
  },
  \"119960\": {
    \"char\": \"𝒘\",
    \"latex\": \"\\\\mathbit{w}\",
    \"math\": null
  },
  \"119961\": {
    \"char\": \"𝒙\",
    \"latex\": \"\\\\mathbit{x}\",
    \"math\": null
  },
  \"119962\": {
    \"char\": \"𝒚\",
    \"latex\": \"\\\\mathbit{y}\",
    \"math\": null
  },
  \"119963\": {
    \"char\": \"𝒛\",
    \"latex\": \"\\\\mathbit{z}\",
    \"math\": null
  },
  \"119964\": {
    \"char\": \"𝒜\",
    \"latex\": \"\\\\mathscr{A}\",
    \"math\": null
  },
  \"119966\": {
    \"char\": \"𝒞\",
    \"latex\": \"\\\\mathscr{C}\",
    \"math\": null
  },
  \"119967\": {
    \"char\": \"𝒟\",
    \"latex\": \"\\\\mathscr{D}\",
    \"math\": null
  },
  \"119970\": {
    \"char\": \"𝒢\",
    \"latex\": \"\\\\mathscr{G}\",
    \"math\": null
  },
  \"119973\": {
    \"char\": \"𝒥\",
    \"latex\": \"\\\\mathscr{J}\",
    \"math\": null
  },
  \"119974\": {
    \"char\": \"𝒦\",
    \"latex\": \"\\\\mathscr{K}\",
    \"math\": null
  },
  \"119977\": {
    \"char\": \"𝒩\",
    \"latex\": \"\\\\mathscr{N}\",
    \"math\": null
  },
  \"119978\": {
    \"char\": \"𝒪\",
    \"latex\": \"\\\\mathscr{O}\",
    \"math\": null
  },
  \"119979\": {
    \"char\": \"𝒫\",
    \"latex\": \"\\\\mathscr{P}\",
    \"math\": null
  },
  \"119980\": {
    \"char\": \"𝒬\",
    \"latex\": \"\\\\mathscr{Q}\",
    \"math\": null
  },
  \"119982\": {
    \"char\": \"𝒮\",
    \"latex\": \"\\\\mathscr{S}\",
    \"math\": null
  },
  \"119983\": {
    \"char\": \"𝒯\",
    \"latex\": \"\\\\mathscr{T}\",
    \"math\": null
  },
  \"119984\": {
    \"char\": \"𝒰\",
    \"latex\": \"\\\\mathscr{U}\",
    \"math\": null
  },
  \"119985\": {
    \"char\": \"𝒱\",
    \"latex\": \"\\\\mathscr{V}\",
    \"math\": null
  },
  \"119986\": {
    \"char\": \"𝒲\",
    \"latex\": \"\\\\mathscr{W}\",
    \"math\": null
  },
  \"119987\": {
    \"char\": \"𝒳\",
    \"latex\": \"\\\\mathscr{X}\",
    \"math\": null
  },
  \"119988\": {
    \"char\": \"𝒴\",
    \"latex\": \"\\\\mathscr{Y}\",
    \"math\": null
  },
  \"119989\": {
    \"char\": \"𝒵\",
    \"latex\": \"\\\\mathscr{Z}\",
    \"math\": null
  },
  \"119990\": {
    \"char\": \"𝒶\",
    \"latex\": \"\\\\mathscr{a}\",
    \"math\": null
  },
  \"119991\": {
    \"char\": \"𝒷\",
    \"latex\": \"\\\\mathscr{b}\",
    \"math\": null
  },
  \"119992\": {
    \"char\": \"𝒸\",
    \"latex\": \"\\\\mathscr{c}\",
    \"math\": null
  },
  \"119993\": {
    \"char\": \"𝒹\",
    \"latex\": \"\\\\mathscr{d}\",
    \"math\": null
  },
  \"119995\": {
    \"char\": \"𝒻\",
    \"latex\": \"\\\\mathscr{f}\",
    \"math\": null
  },
  \"119997\": {
    \"char\": \"𝒽\",
    \"latex\": \"\\\\mathscr{h}\",
    \"math\": null
  },
  \"119998\": {
    \"char\": \"𝒾\",
    \"latex\": \"\\\\mathscr{i}\",
    \"math\": null
  },
  \"119999\": {
    \"char\": \"𝒿\",
    \"latex\": \"\\\\mathscr{j}\",
    \"math\": null
  },
  \"120000\": {
    \"char\": \"𝓀\",
    \"latex\": \"\\\\mathscr{k}\",
    \"math\": null
  },
  \"120001\": {
    \"char\": \"𝓁\",
    \"latex\": \"\\\\mathscr{l}\",
    \"math\": null
  },
  \"120002\": {
    \"char\": \"𝓂\",
    \"latex\": \"\\\\mathscr{m}\",
    \"math\": null
  },
  \"120003\": {
    \"char\": \"𝓃\",
    \"latex\": \"\\\\mathscr{n}\",
    \"math\": null
  },
  \"120005\": {
    \"char\": \"𝓅\",
    \"latex\": \"\\\\mathscr{p}\",
    \"math\": null
  },
  \"120006\": {
    \"char\": \"𝓆\",
    \"latex\": \"\\\\mathscr{q}\",
    \"math\": null
  },
  \"120007\": {
    \"char\": \"𝓇\",
    \"latex\": \"\\\\mathscr{r}\",
    \"math\": null
  },
  \"120008\": {
    \"char\": \"𝓈\",
    \"latex\": \"\\\\mathscr{s}\",
    \"math\": null
  },
  \"120009\": {
    \"char\": \"𝓉\",
    \"latex\": \"\\\\mathscr{t}\",
    \"math\": null
  },
  \"120010\": {
    \"char\": \"𝓊\",
    \"latex\": \"\\\\mathscr{u}\",
    \"math\": null
  },
  \"120011\": {
    \"char\": \"𝓋\",
    \"latex\": \"\\\\mathscr{v}\",
    \"math\": null
  },
  \"120012\": {
    \"char\": \"𝓌\",
    \"latex\": \"\\\\mathscr{w}\",
    \"math\": null
  },
  \"120013\": {
    \"char\": \"𝓍\",
    \"latex\": \"\\\\mathscr{x}\",
    \"math\": null
  },
  \"120014\": {
    \"char\": \"𝓎\",
    \"latex\": \"\\\\mathscr{y}\",
    \"math\": null
  },
  \"120015\": {
    \"char\": \"𝓏\",
    \"latex\": \"\\\\mathscr{z}\",
    \"math\": null
  },
  \"120016\": {
    \"char\": \"𝓐\",
    \"latex\": \"\\\\mathmit{A}\",
    \"math\": null
  },
  \"120017\": {
    \"char\": \"𝓑\",
    \"latex\": \"\\\\mathmit{B}\",
    \"math\": null
  },
  \"120018\": {
    \"char\": \"𝓒\",
    \"latex\": \"\\\\mathmit{C}\",
    \"math\": null
  },
  \"120019\": {
    \"char\": \"𝓓\",
    \"latex\": \"\\\\mathmit{D}\",
    \"math\": null
  },
  \"120020\": {
    \"char\": \"𝓔\",
    \"latex\": \"\\\\mathmit{E}\",
    \"math\": null
  },
  \"120021\": {
    \"char\": \"𝓕\",
    \"latex\": \"\\\\mathmit{F}\",
    \"math\": null
  },
  \"120022\": {
    \"char\": \"𝓖\",
    \"latex\": \"\\\\mathmit{G}\",
    \"math\": null
  },
  \"120023\": {
    \"char\": \"𝓗\",
    \"latex\": \"\\\\mathmit{H}\",
    \"math\": null
  },
  \"120024\": {
    \"char\": \"𝓘\",
    \"latex\": \"\\\\mathmit{I}\",
    \"math\": null
  },
  \"120025\": {
    \"char\": \"𝓙\",
    \"latex\": \"\\\\mathmit{J}\",
    \"math\": null
  },
  \"120026\": {
    \"char\": \"𝓚\",
    \"latex\": \"\\\\mathmit{K}\",
    \"math\": null
  },
  \"120027\": {
    \"char\": \"𝓛\",
    \"latex\": \"\\\\mathmit{L}\",
    \"math\": null
  },
  \"120028\": {
    \"char\": \"𝓜\",
    \"latex\": \"\\\\mathmit{M}\",
    \"math\": null
  },
  \"120029\": {
    \"char\": \"𝓝\",
    \"latex\": \"\\\\mathmit{N}\",
    \"math\": null
  },
  \"120030\": {
    \"char\": \"𝓞\",
    \"latex\": \"\\\\mathmit{O}\",
    \"math\": null
  },
  \"120031\": {
    \"char\": \"𝓟\",
    \"latex\": \"\\\\mathmit{P}\",
    \"math\": null
  },
  \"120032\": {
    \"char\": \"𝓠\",
    \"latex\": \"\\\\mathmit{Q}\",
    \"math\": null
  },
  \"120033\": {
    \"char\": \"𝓡\",
    \"latex\": \"\\\\mathmit{R}\",
    \"math\": null
  },
  \"120034\": {
    \"char\": \"𝓢\",
    \"latex\": \"\\\\mathmit{S}\",
    \"math\": null
  },
  \"120035\": {
    \"char\": \"𝓣\",
    \"latex\": \"\\\\mathmit{T}\",
    \"math\": null
  },
  \"120036\": {
    \"char\": \"𝓤\",
    \"latex\": \"\\\\mathmit{U}\",
    \"math\": null
  },
  \"120037\": {
    \"char\": \"𝓥\",
    \"latex\": \"\\\\mathmit{V}\",
    \"math\": null
  },
  \"120038\": {
    \"char\": \"𝓦\",
    \"latex\": \"\\\\mathmit{W}\",
    \"math\": null
  },
  \"120039\": {
    \"char\": \"𝓧\",
    \"latex\": \"\\\\mathmit{X}\",
    \"math\": null
  },
  \"120040\": {
    \"char\": \"𝓨\",
    \"latex\": \"\\\\mathmit{Y}\",
    \"math\": null
  },
  \"120041\": {
    \"char\": \"𝓩\",
    \"latex\": \"\\\\mathmit{Z}\",
    \"math\": null
  },
  \"120042\": {
    \"char\": \"𝓪\",
    \"latex\": \"\\\\mathmit{a}\",
    \"math\": null
  },
  \"120043\": {
    \"char\": \"𝓫\",
    \"latex\": \"\\\\mathmit{b}\",
    \"math\": null
  },
  \"120044\": {
    \"char\": \"𝓬\",
    \"latex\": \"\\\\mathmit{c}\",
    \"math\": null
  },
  \"120045\": {
    \"char\": \"𝓭\",
    \"latex\": \"\\\\mathmit{d}\",
    \"math\": null
  },
  \"120046\": {
    \"char\": \"𝓮\",
    \"latex\": \"\\\\mathmit{e}\",
    \"math\": null
  },
  \"120047\": {
    \"char\": \"𝓯\",
    \"latex\": \"\\\\mathmit{f}\",
    \"math\": null
  },
  \"120048\": {
    \"char\": \"𝓰\",
    \"latex\": \"\\\\mathmit{g}\",
    \"math\": null
  },
  \"120049\": {
    \"char\": \"𝓱\",
    \"latex\": \"\\\\mathmit{h}\",
    \"math\": null
  },
  \"120050\": {
    \"char\": \"𝓲\",
    \"latex\": \"\\\\mathmit{i}\",
    \"math\": null
  },
  \"120051\": {
    \"char\": \"𝓳\",
    \"latex\": \"\\\\mathmit{j}\",
    \"math\": null
  },
  \"120052\": {
    \"char\": \"𝓴\",
    \"latex\": \"\\\\mathmit{k}\",
    \"math\": null
  },
  \"120053\": {
    \"char\": \"𝓵\",
    \"latex\": \"\\\\mathmit{l}\",
    \"math\": null
  },
  \"120054\": {
    \"char\": \"𝓶\",
    \"latex\": \"\\\\mathmit{m}\",
    \"math\": null
  },
  \"120055\": {
    \"char\": \"𝓷\",
    \"latex\": \"\\\\mathmit{n}\",
    \"math\": null
  },
  \"120056\": {
    \"char\": \"𝓸\",
    \"latex\": \"\\\\mathmit{o}\",
    \"math\": null
  },
  \"120057\": {
    \"char\": \"𝓹\",
    \"latex\": \"\\\\mathmit{p}\",
    \"math\": null
  },
  \"120058\": {
    \"char\": \"𝓺\",
    \"latex\": \"\\\\mathmit{q}\",
    \"math\": null
  },
  \"120059\": {
    \"char\": \"𝓻\",
    \"latex\": \"\\\\mathmit{r}\",
    \"math\": null
  },
  \"120060\": {
    \"char\": \"𝓼\",
    \"latex\": \"\\\\mathmit{s}\",
    \"math\": null
  },
  \"120061\": {
    \"char\": \"𝓽\",
    \"latex\": \"\\\\mathmit{t}\",
    \"math\": null
  },
  \"120062\": {
    \"char\": \"𝓾\",
    \"latex\": \"\\\\mathmit{u}\",
    \"math\": null
  },
  \"120063\": {
    \"char\": \"𝓿\",
    \"latex\": \"\\\\mathmit{v}\",
    \"math\": null
  },
  \"120064\": {
    \"char\": \"𝔀\",
    \"latex\": \"\\\\mathmit{w}\",
    \"math\": null
  },
  \"120065\": {
    \"char\": \"𝔁\",
    \"latex\": \"\\\\mathmit{x}\",
    \"math\": null
  },
  \"120066\": {
    \"char\": \"𝔂\",
    \"latex\": \"\\\\mathmit{y}\",
    \"math\": null
  },
  \"120067\": {
    \"char\": \"𝔃\",
    \"latex\": \"\\\\mathmit{z}\",
    \"math\": null
  },
  \"120068\": {
    \"char\": \"𝔄\",
    \"latex\": \"\\\\mathfrak{A}\",
    \"math\": null
  },
  \"120069\": {
    \"char\": \"𝔅\",
    \"latex\": \"\\\\mathfrak{B}\",
    \"math\": null
  },
  \"120071\": {
    \"char\": \"𝔇\",
    \"latex\": \"\\\\mathfrak{D}\",
    \"math\": null
  },
  \"120072\": {
    \"char\": \"𝔈\",
    \"latex\": \"\\\\mathfrak{E}\",
    \"math\": null
  },
  \"120073\": {
    \"char\": \"𝔉\",
    \"latex\": \"\\\\mathfrak{F}\",
    \"math\": null
  },
  \"120074\": {
    \"char\": \"𝔊\",
    \"latex\": \"\\\\mathfrak{G}\",
    \"math\": null
  },
  \"120077\": {
    \"char\": \"𝔍\",
    \"latex\": \"\\\\mathfrak{J}\",
    \"math\": null
  },
  \"120078\": {
    \"char\": \"𝔎\",
    \"latex\": \"\\\\mathfrak{K}\",
    \"math\": null
  },
  \"120079\": {
    \"char\": \"𝔏\",
    \"latex\": \"\\\\mathfrak{L}\",
    \"math\": null
  },
  \"120080\": {
    \"char\": \"𝔐\",
    \"latex\": \"\\\\mathfrak{M}\",
    \"math\": null
  },
  \"120081\": {
    \"char\": \"𝔑\",
    \"latex\": \"\\\\mathfrak{N}\",
    \"math\": null
  },
  \"120082\": {
    \"char\": \"𝔒\",
    \"latex\": \"\\\\mathfrak{O}\",
    \"math\": null
  },
  \"120083\": {
    \"char\": \"𝔓\",
    \"latex\": \"\\\\mathfrak{P}\",
    \"math\": null
  },
  \"120084\": {
    \"char\": \"𝔔\",
    \"latex\": \"\\\\mathfrak{Q}\",
    \"math\": null
  },
  \"120086\": {
    \"char\": \"𝔖\",
    \"latex\": \"\\\\mathfrak{S}\",
    \"math\": null
  },
  \"120087\": {
    \"char\": \"𝔗\",
    \"latex\": \"\\\\mathfrak{T}\",
    \"math\": null
  },
  \"120088\": {
    \"char\": \"𝔘\",
    \"latex\": \"\\\\mathfrak{U}\",
    \"math\": null
  },
  \"120089\": {
    \"char\": \"𝔙\",
    \"latex\": \"\\\\mathfrak{V}\",
    \"math\": null
  },
  \"120090\": {
    \"char\": \"𝔚\",
    \"latex\": \"\\\\mathfrak{W}\",
    \"math\": null
  },
  \"120091\": {
    \"char\": \"𝔛\",
    \"latex\": \"\\\\mathfrak{X}\",
    \"math\": null
  },
  \"120092\": {
    \"char\": \"𝔜\",
    \"latex\": \"\\\\mathfrak{Y}\",
    \"math\": null
  },
  \"120094\": {
    \"char\": \"𝔞\",
    \"latex\": \"\\\\mathfrak{a}\",
    \"math\": null
  },
  \"120095\": {
    \"char\": \"𝔟\",
    \"latex\": \"\\\\mathfrak{b}\",
    \"math\": null
  },
  \"120096\": {
    \"char\": \"𝔠\",
    \"latex\": \"\\\\mathfrak{c}\",
    \"math\": null
  },
  \"120097\": {
    \"char\": \"𝔡\",
    \"latex\": \"\\\\mathfrak{d}\",
    \"math\": null
  },
  \"120098\": {
    \"char\": \"𝔢\",
    \"latex\": \"\\\\mathfrak{e}\",
    \"math\": null
  },
  \"120099\": {
    \"char\": \"𝔣\",
    \"latex\": \"\\\\mathfrak{f}\",
    \"math\": null
  },
  \"120100\": {
    \"char\": \"𝔤\",
    \"latex\": \"\\\\mathfrak{g}\",
    \"math\": null
  },
  \"120101\": {
    \"char\": \"𝔥\",
    \"latex\": \"\\\\mathfrak{h}\",
    \"math\": null
  },
  \"120102\": {
    \"char\": \"𝔦\",
    \"latex\": \"\\\\mathfrak{i}\",
    \"math\": null
  },
  \"120103\": {
    \"char\": \"𝔧\",
    \"latex\": \"\\\\mathfrak{j}\",
    \"math\": null
  },
  \"120104\": {
    \"char\": \"𝔨\",
    \"latex\": \"\\\\mathfrak{k}\",
    \"math\": null
  },
  \"120105\": {
    \"char\": \"𝔩\",
    \"latex\": \"\\\\mathfrak{l}\",
    \"math\": null
  },
  \"120106\": {
    \"char\": \"𝔪\",
    \"latex\": \"\\\\mathfrak{m}\",
    \"math\": null
  },
  \"120107\": {
    \"char\": \"𝔫\",
    \"latex\": \"\\\\mathfrak{n}\",
    \"math\": null
  },
  \"120108\": {
    \"char\": \"𝔬\",
    \"latex\": \"\\\\mathfrak{o}\",
    \"math\": null
  },
  \"120109\": {
    \"char\": \"𝔭\",
    \"latex\": \"\\\\mathfrak{p}\",
    \"math\": null
  },
  \"120110\": {
    \"char\": \"𝔮\",
    \"latex\": \"\\\\mathfrak{q}\",
    \"math\": null
  },
  \"120111\": {
    \"char\": \"𝔯\",
    \"latex\": \"\\\\mathfrak{r}\",
    \"math\": null
  },
  \"120112\": {
    \"char\": \"𝔰\",
    \"latex\": \"\\\\mathfrak{s}\",
    \"math\": null
  },
  \"120113\": {
    \"char\": \"𝔱\",
    \"latex\": \"\\\\mathfrak{t}\",
    \"math\": null
  },
  \"120114\": {
    \"char\": \"𝔲\",
    \"latex\": \"\\\\mathfrak{u}\",
    \"math\": null
  },
  \"120115\": {
    \"char\": \"𝔳\",
    \"latex\": \"\\\\mathfrak{v}\",
    \"math\": null
  },
  \"120116\": {
    \"char\": \"𝔴\",
    \"latex\": \"\\\\mathfrak{w}\",
    \"math\": null
  },
  \"120117\": {
    \"char\": \"𝔵\",
    \"latex\": \"\\\\mathfrak{x}\",
    \"math\": null
  },
  \"120118\": {
    \"char\": \"𝔶\",
    \"latex\": \"\\\\mathfrak{y}\",
    \"math\": null
  },
  \"120119\": {
    \"char\": \"𝔷\",
    \"latex\": \"\\\\mathfrak{z}\",
    \"math\": null
  },
  \"120120\": {
    \"char\": \"𝔸\",
    \"latex\": \"\\\\mathbb{A}\",
    \"math\": null
  },
  \"120121\": {
    \"char\": \"𝔹\",
    \"latex\": \"\\\\mathbb{B}\",
    \"math\": null
  },
  \"120123\": {
    \"char\": \"𝔻\",
    \"latex\": \"\\\\mathbb{D}\",
    \"math\": null
  },
  \"120124\": {
    \"char\": \"𝔼\",
    \"latex\": \"\\\\mathbb{E}\",
    \"math\": null
  },
  \"120125\": {
    \"char\": \"𝔽\",
    \"latex\": \"\\\\mathbb{F}\",
    \"math\": null
  },
  \"120126\": {
    \"char\": \"𝔾\",
    \"latex\": \"\\\\mathbb{G}\",
    \"math\": null
  },
  \"120128\": {
    \"char\": \"𝕀\",
    \"latex\": \"\\\\mathbb{I}\",
    \"math\": null
  },
  \"120129\": {
    \"char\": \"𝕁\",
    \"latex\": \"\\\\mathbb{J}\",
    \"math\": null
  },
  \"120130\": {
    \"char\": \"𝕂\",
    \"latex\": \"\\\\mathbb{K}\",
    \"math\": null
  },
  \"120131\": {
    \"char\": \"𝕃\",
    \"latex\": \"\\\\mathbb{L}\",
    \"math\": null
  },
  \"120132\": {
    \"char\": \"𝕄\",
    \"latex\": \"\\\\mathbb{M}\",
    \"math\": null
  },
  \"120134\": {
    \"char\": \"𝕆\",
    \"latex\": \"\\\\mathbb{O}\",
    \"math\": null
  },
  \"120138\": {
    \"char\": \"𝕊\",
    \"latex\": \"\\\\mathbb{S}\",
    \"math\": null
  },
  \"120139\": {
    \"char\": \"𝕋\",
    \"latex\": \"\\\\mathbb{T}\",
    \"math\": null
  },
  \"120140\": {
    \"char\": \"𝕌\",
    \"latex\": \"\\\\mathbb{U}\",
    \"math\": null
  },
  \"120141\": {
    \"char\": \"𝕍\",
    \"latex\": \"\\\\mathbb{V}\",
    \"math\": null
  },
  \"120142\": {
    \"char\": \"𝕎\",
    \"latex\": \"\\\\mathbb{W}\",
    \"math\": null
  },
  \"120143\": {
    \"char\": \"𝕏\",
    \"latex\": \"\\\\mathbb{X}\",
    \"math\": null
  },
  \"120144\": {
    \"char\": \"𝕐\",
    \"latex\": \"\\\\mathbb{Y}\",
    \"math\": null
  },
  \"120146\": {
    \"char\": \"𝕒\",
    \"latex\": \"\\\\mathbb{a}\",
    \"math\": null
  },
  \"120147\": {
    \"char\": \"𝕓\",
    \"latex\": \"\\\\mathbb{b}\",
    \"math\": null
  },
  \"120148\": {
    \"char\": \"𝕔\",
    \"latex\": \"\\\\mathbb{c}\",
    \"math\": null
  },
  \"120149\": {
    \"char\": \"𝕕\",
    \"latex\": \"\\\\mathbb{d}\",
    \"math\": null
  },
  \"120150\": {
    \"char\": \"𝕖\",
    \"latex\": \"\\\\mathbb{e}\",
    \"math\": null
  },
  \"120151\": {
    \"char\": \"𝕗\",
    \"latex\": \"\\\\mathbb{f}\",
    \"math\": null
  },
  \"120152\": {
    \"char\": \"𝕘\",
    \"latex\": \"\\\\mathbb{g}\",
    \"math\": null
  },
  \"120153\": {
    \"char\": \"𝕙\",
    \"latex\": \"\\\\mathbb{h}\",
    \"math\": null
  },
  \"120154\": {
    \"char\": \"𝕚\",
    \"latex\": \"\\\\mathbb{i}\",
    \"math\": null
  },
  \"120155\": {
    \"char\": \"𝕛\",
    \"latex\": \"\\\\mathbb{j}\",
    \"math\": null
  },
  \"120156\": {
    \"char\": \"𝕜\",
    \"latex\": \"\\\\mathbb{k}\",
    \"math\": null
  },
  \"120157\": {
    \"char\": \"𝕝\",
    \"latex\": \"\\\\mathbb{l}\",
    \"math\": null
  },
  \"120158\": {
    \"char\": \"𝕞\",
    \"latex\": \"\\\\mathbb{m}\",
    \"math\": null
  },
  \"120159\": {
    \"char\": \"𝕟\",
    \"latex\": \"\\\\mathbb{n}\",
    \"math\": null
  },
  \"120160\": {
    \"char\": \"𝕠\",
    \"latex\": \"\\\\mathbb{o}\",
    \"math\": null
  },
  \"120161\": {
    \"char\": \"𝕡\",
    \"latex\": \"\\\\mathbb{p}\",
    \"math\": null
  },
  \"120162\": {
    \"char\": \"𝕢\",
    \"latex\": \"\\\\mathbb{q}\",
    \"math\": null
  },
  \"120163\": {
    \"char\": \"𝕣\",
    \"latex\": \"\\\\mathbb{r}\",
    \"math\": null
  },
  \"120164\": {
    \"char\": \"𝕤\",
    \"latex\": \"\\\\mathbb{s}\",
    \"math\": null
  },
  \"120165\": {
    \"char\": \"𝕥\",
    \"latex\": \"\\\\mathbb{t}\",
    \"math\": null
  },
  \"120166\": {
    \"char\": \"𝕦\",
    \"latex\": \"\\\\mathbb{u}\",
    \"math\": null
  },
  \"120167\": {
    \"char\": \"𝕧\",
    \"latex\": \"\\\\mathbb{v}\",
    \"math\": null
  },
  \"120168\": {
    \"char\": \"𝕨\",
    \"latex\": \"\\\\mathbb{w}\",
    \"math\": null
  },
  \"120169\": {
    \"char\": \"𝕩\",
    \"latex\": \"\\\\mathbb{x}\",
    \"math\": null
  },
  \"120170\": {
    \"char\": \"𝕪\",
    \"latex\": \"\\\\mathbb{y}\",
    \"math\": null
  },
  \"120171\": {
    \"char\": \"𝕫\",
    \"latex\": \"\\\\mathbb{z}\",
    \"math\": null
  },
  \"120172\": {
    \"char\": \"𝕬\",
    \"latex\": \"\\\\mathslbb{A}\",
    \"math\": null
  },
  \"120173\": {
    \"char\": \"𝕭\",
    \"latex\": \"\\\\mathslbb{B}\",
    \"math\": null
  },
  \"120174\": {
    \"char\": \"𝕮\",
    \"latex\": \"\\\\mathslbb{C}\",
    \"math\": null
  },
  \"120175\": {
    \"char\": \"𝕯\",
    \"latex\": \"\\\\mathslbb{D}\",
    \"math\": null
  },
  \"120176\": {
    \"char\": \"𝕰\",
    \"latex\": \"\\\\mathslbb{E}\",
    \"math\": null
  },
  \"120177\": {
    \"char\": \"𝕱\",
    \"latex\": \"\\\\mathslbb{F}\",
    \"math\": null
  },
  \"120178\": {
    \"char\": \"𝕲\",
    \"latex\": \"\\\\mathslbb{G}\",
    \"math\": null
  },
  \"120179\": {
    \"char\": \"𝕳\",
    \"latex\": \"\\\\mathslbb{H}\",
    \"math\": null
  },
  \"120180\": {
    \"char\": \"𝕴\",
    \"latex\": \"\\\\mathslbb{I}\",
    \"math\": null
  },
  \"120181\": {
    \"char\": \"𝕵\",
    \"latex\": \"\\\\mathslbb{J}\",
    \"math\": null
  },
  \"120182\": {
    \"char\": \"𝕶\",
    \"latex\": \"\\\\mathslbb{K}\",
    \"math\": null
  },
  \"120183\": {
    \"char\": \"𝕷\",
    \"latex\": \"\\\\mathslbb{L}\",
    \"math\": null
  },
  \"120184\": {
    \"char\": \"𝕸\",
    \"latex\": \"\\\\mathslbb{M}\",
    \"math\": null
  },
  \"120185\": {
    \"char\": \"𝕹\",
    \"latex\": \"\\\\mathslbb{N}\",
    \"math\": null
  },
  \"120186\": {
    \"char\": \"𝕺\",
    \"latex\": \"\\\\mathslbb{O}\",
    \"math\": null
  },
  \"120187\": {
    \"char\": \"𝕻\",
    \"latex\": \"\\\\mathslbb{P}\",
    \"math\": null
  },
  \"120188\": {
    \"char\": \"𝕼\",
    \"latex\": \"\\\\mathslbb{Q}\",
    \"math\": null
  },
  \"120189\": {
    \"char\": \"𝕽\",
    \"latex\": \"\\\\mathslbb{R}\",
    \"math\": null
  },
  \"120190\": {
    \"char\": \"𝕾\",
    \"latex\": \"\\\\mathslbb{S}\",
    \"math\": null
  },
  \"120191\": {
    \"char\": \"𝕿\",
    \"latex\": \"\\\\mathslbb{T}\",
    \"math\": null
  },
  \"120192\": {
    \"char\": \"𝖀\",
    \"latex\": \"\\\\mathslbb{U}\",
    \"math\": null
  },
  \"120193\": {
    \"char\": \"𝖁\",
    \"latex\": \"\\\\mathslbb{V}\",
    \"math\": null
  },
  \"120194\": {
    \"char\": \"𝖂\",
    \"latex\": \"\\\\mathslbb{W}\",
    \"math\": null
  },
  \"120195\": {
    \"char\": \"𝖃\",
    \"latex\": \"\\\\mathslbb{X}\",
    \"math\": null
  },
  \"120196\": {
    \"char\": \"𝖄\",
    \"latex\": \"\\\\mathslbb{Y}\",
    \"math\": null
  },
  \"120197\": {
    \"char\": \"𝖅\",
    \"latex\": \"\\\\mathslbb{Z}\",
    \"math\": null
  },
  \"120198\": {
    \"char\": \"𝖆\",
    \"latex\": \"\\\\mathslbb{a}\",
    \"math\": null
  },
  \"120199\": {
    \"char\": \"𝖇\",
    \"latex\": \"\\\\mathslbb{b}\",
    \"math\": null
  },
  \"120200\": {
    \"char\": \"𝖈\",
    \"latex\": \"\\\\mathslbb{c}\",
    \"math\": null
  },
  \"120201\": {
    \"char\": \"𝖉\",
    \"latex\": \"\\\\mathslbb{d}\",
    \"math\": null
  },
  \"120202\": {
    \"char\": \"𝖊\",
    \"latex\": \"\\\\mathslbb{e}\",
    \"math\": null
  },
  \"120203\": {
    \"char\": \"𝖋\",
    \"latex\": \"\\\\mathslbb{f}\",
    \"math\": null
  },
  \"120204\": {
    \"char\": \"𝖌\",
    \"latex\": \"\\\\mathslbb{g}\",
    \"math\": null
  },
  \"120205\": {
    \"char\": \"𝖍\",
    \"latex\": \"\\\\mathslbb{h}\",
    \"math\": null
  },
  \"120206\": {
    \"char\": \"𝖎\",
    \"latex\": \"\\\\mathslbb{i}\",
    \"math\": null
  },
  \"120207\": {
    \"char\": \"𝖏\",
    \"latex\": \"\\\\mathslbb{j}\",
    \"math\": null
  },
  \"120208\": {
    \"char\": \"𝖐\",
    \"latex\": \"\\\\mathslbb{k}\",
    \"math\": null
  },
  \"120209\": {
    \"char\": \"𝖑\",
    \"latex\": \"\\\\mathslbb{l}\",
    \"math\": null
  },
  \"120210\": {
    \"char\": \"𝖒\",
    \"latex\": \"\\\\mathslbb{m}\",
    \"math\": null
  },
  \"120211\": {
    \"char\": \"𝖓\",
    \"latex\": \"\\\\mathslbb{n}\",
    \"math\": null
  },
  \"120212\": {
    \"char\": \"𝖔\",
    \"latex\": \"\\\\mathslbb{o}\",
    \"math\": null
  },
  \"120213\": {
    \"char\": \"𝖕\",
    \"latex\": \"\\\\mathslbb{p}\",
    \"math\": null
  },
  \"120214\": {
    \"char\": \"𝖖\",
    \"latex\": \"\\\\mathslbb{q}\",
    \"math\": null
  },
  \"120215\": {
    \"char\": \"𝖗\",
    \"latex\": \"\\\\mathslbb{r}\",
    \"math\": null
  },
  \"120216\": {
    \"char\": \"𝖘\",
    \"latex\": \"\\\\mathslbb{s}\",
    \"math\": null
  },
  \"120217\": {
    \"char\": \"𝖙\",
    \"latex\": \"\\\\mathslbb{t}\",
    \"math\": null
  },
  \"120218\": {
    \"char\": \"𝖚\",
    \"latex\": \"\\\\mathslbb{u}\",
    \"math\": null
  },
  \"120219\": {
    \"char\": \"𝖛\",
    \"latex\": \"\\\\mathslbb{v}\",
    \"math\": null
  },
  \"120220\": {
    \"char\": \"𝖜\",
    \"latex\": \"\\\\mathslbb{w}\",
    \"math\": null
  },
  \"120221\": {
    \"char\": \"𝖝\",
    \"latex\": \"\\\\mathslbb{x}\",
    \"math\": null
  },
  \"120222\": {
    \"char\": \"𝖞\",
    \"latex\": \"\\\\mathslbb{y}\",
    \"math\": null
  },
  \"120223\": {
    \"char\": \"𝖟\",
    \"latex\": \"\\\\mathslbb{z}\",
    \"math\": null
  },
  \"120224\": {
    \"char\": \"𝖠\",
    \"latex\": \"\\\\mathsf{A}\",
    \"math\": null
  },
  \"120225\": {
    \"char\": \"𝖡\",
    \"latex\": \"\\\\mathsf{B}\",
    \"math\": null
  },
  \"120226\": {
    \"char\": \"𝖢\",
    \"latex\": \"\\\\mathsf{C}\",
    \"math\": null
  },
  \"120227\": {
    \"char\": \"𝖣\",
    \"latex\": \"\\\\mathsf{D}\",
    \"math\": null
  },
  \"120228\": {
    \"char\": \"𝖤\",
    \"latex\": \"\\\\mathsf{E}\",
    \"math\": null
  },
  \"120229\": {
    \"char\": \"𝖥\",
    \"latex\": \"\\\\mathsf{F}\",
    \"math\": null
  },
  \"120230\": {
    \"char\": \"𝖦\",
    \"latex\": \"\\\\mathsf{G}\",
    \"math\": null
  },
  \"120231\": {
    \"char\": \"𝖧\",
    \"latex\": \"\\\\mathsf{H}\",
    \"math\": null
  },
  \"120232\": {
    \"char\": \"𝖨\",
    \"latex\": \"\\\\mathsf{I}\",
    \"math\": null
  },
  \"120233\": {
    \"char\": \"𝖩\",
    \"latex\": \"\\\\mathsf{J}\",
    \"math\": null
  },
  \"120234\": {
    \"char\": \"𝖪\",
    \"latex\": \"\\\\mathsf{K}\",
    \"math\": null
  },
  \"120235\": {
    \"char\": \"𝖫\",
    \"latex\": \"\\\\mathsf{L}\",
    \"math\": null
  },
  \"120236\": {
    \"char\": \"𝖬\",
    \"latex\": \"\\\\mathsf{M}\",
    \"math\": null
  },
  \"120237\": {
    \"char\": \"𝖭\",
    \"latex\": \"\\\\mathsf{N}\",
    \"math\": null
  },
  \"120238\": {
    \"char\": \"𝖮\",
    \"latex\": \"\\\\mathsf{O}\",
    \"math\": null
  },
  \"120239\": {
    \"char\": \"𝖯\",
    \"latex\": \"\\\\mathsf{P}\",
    \"math\": null
  },
  \"120240\": {
    \"char\": \"𝖰\",
    \"latex\": \"\\\\mathsf{Q}\",
    \"math\": null
  },
  \"120241\": {
    \"char\": \"𝖱\",
    \"latex\": \"\\\\mathsf{R}\",
    \"math\": null
  },
  \"120242\": {
    \"char\": \"𝖲\",
    \"latex\": \"\\\\mathsf{S}\",
    \"math\": null
  },
  \"120243\": {
    \"char\": \"𝖳\",
    \"latex\": \"\\\\mathsf{T}\",
    \"math\": null
  },
  \"120244\": {
    \"char\": \"𝖴\",
    \"latex\": \"\\\\mathsf{U}\",
    \"math\": null
  },
  \"120245\": {
    \"char\": \"𝖵\",
    \"latex\": \"\\\\mathsf{V}\",
    \"math\": null
  },
  \"120246\": {
    \"char\": \"𝖶\",
    \"latex\": \"\\\\mathsf{W}\",
    \"math\": null
  },
  \"120247\": {
    \"char\": \"𝖷\",
    \"latex\": \"\\\\mathsf{X}\",
    \"math\": null
  },
  \"120248\": {
    \"char\": \"𝖸\",
    \"latex\": \"\\\\mathsf{Y}\",
    \"math\": null
  },
  \"120249\": {
    \"char\": \"𝖹\",
    \"latex\": \"\\\\mathsf{Z}\",
    \"math\": null
  },
  \"120250\": {
    \"char\": \"𝖺\",
    \"latex\": \"\\\\mathsf{a}\",
    \"math\": null
  },
  \"120251\": {
    \"char\": \"𝖻\",
    \"latex\": \"\\\\mathsf{b}\",
    \"math\": null
  },
  \"120252\": {
    \"char\": \"𝖼\",
    \"latex\": \"\\\\mathsf{c}\",
    \"math\": null
  },
  \"120253\": {
    \"char\": \"𝖽\",
    \"latex\": \"\\\\mathsf{d}\",
    \"math\": null
  },
  \"120254\": {
    \"char\": \"𝖾\",
    \"latex\": \"\\\\mathsf{e}\",
    \"math\": null
  },
  \"120255\": {
    \"char\": \"𝖿\",
    \"latex\": \"\\\\mathsf{f}\",
    \"math\": null
  },
  \"120256\": {
    \"char\": \"𝗀\",
    \"latex\": \"\\\\mathsf{g}\",
    \"math\": null
  },
  \"120257\": {
    \"char\": \"𝗁\",
    \"latex\": \"\\\\mathsf{h}\",
    \"math\": null
  },
  \"120258\": {
    \"char\": \"𝗂\",
    \"latex\": \"\\\\mathsf{i}\",
    \"math\": null
  },
  \"120259\": {
    \"char\": \"𝗃\",
    \"latex\": \"\\\\mathsf{j}\",
    \"math\": null
  },
  \"120260\": {
    \"char\": \"𝗄\",
    \"latex\": \"\\\\mathsf{k}\",
    \"math\": null
  },
  \"120261\": {
    \"char\": \"𝗅\",
    \"latex\": \"\\\\mathsf{l}\",
    \"math\": null
  },
  \"120262\": {
    \"char\": \"𝗆\",
    \"latex\": \"\\\\mathsf{m}\",
    \"math\": null
  },
  \"120263\": {
    \"char\": \"𝗇\",
    \"latex\": \"\\\\mathsf{n}\",
    \"math\": null
  },
  \"120264\": {
    \"char\": \"𝗈\",
    \"latex\": \"\\\\mathsf{o}\",
    \"math\": null
  },
  \"120265\": {
    \"char\": \"𝗉\",
    \"latex\": \"\\\\mathsf{p}\",
    \"math\": null
  },
  \"120266\": {
    \"char\": \"𝗊\",
    \"latex\": \"\\\\mathsf{q}\",
    \"math\": null
  },
  \"120267\": {
    \"char\": \"𝗋\",
    \"latex\": \"\\\\mathsf{r}\",
    \"math\": null
  },
  \"120268\": {
    \"char\": \"𝗌\",
    \"latex\": \"\\\\mathsf{s}\",
    \"math\": null
  },
  \"120269\": {
    \"char\": \"𝗍\",
    \"latex\": \"\\\\mathsf{t}\",
    \"math\": null
  },
  \"120270\": {
    \"char\": \"𝗎\",
    \"latex\": \"\\\\mathsf{u}\",
    \"math\": null
  },
  \"120271\": {
    \"char\": \"𝗏\",
    \"latex\": \"\\\\mathsf{v}\",
    \"math\": null
  },
  \"120272\": {
    \"char\": \"𝗐\",
    \"latex\": \"\\\\mathsf{w}\",
    \"math\": null
  },
  \"120273\": {
    \"char\": \"𝗑\",
    \"latex\": \"\\\\mathsf{x}\",
    \"math\": null
  },
  \"120274\": {
    \"char\": \"𝗒\",
    \"latex\": \"\\\\mathsf{y}\",
    \"math\": null
  },
  \"120275\": {
    \"char\": \"𝗓\",
    \"latex\": \"\\\\mathsf{z}\",
    \"math\": null
  },
  \"120276\": {
    \"char\": \"𝗔\",
    \"latex\": \"\\\\mathsfbf{A}\",
    \"math\": null
  },
  \"120277\": {
    \"char\": \"𝗕\",
    \"latex\": \"\\\\mathsfbf{B}\",
    \"math\": null
  },
  \"120278\": {
    \"char\": \"𝗖\",
    \"latex\": \"\\\\mathsfbf{C}\",
    \"math\": null
  },
  \"120279\": {
    \"char\": \"𝗗\",
    \"latex\": \"\\\\mathsfbf{D}\",
    \"math\": null
  },
  \"120280\": {
    \"char\": \"𝗘\",
    \"latex\": \"\\\\mathsfbf{E}\",
    \"math\": null
  },
  \"120281\": {
    \"char\": \"𝗙\",
    \"latex\": \"\\\\mathsfbf{F}\",
    \"math\": null
  },
  \"120282\": {
    \"char\": \"𝗚\",
    \"latex\": \"\\\\mathsfbf{G}\",
    \"math\": null
  },
  \"120283\": {
    \"char\": \"𝗛\",
    \"latex\": \"\\\\mathsfbf{H}\",
    \"math\": null
  },
  \"120284\": {
    \"char\": \"𝗜\",
    \"latex\": \"\\\\mathsfbf{I}\",
    \"math\": null
  },
  \"120285\": {
    \"char\": \"𝗝\",
    \"latex\": \"\\\\mathsfbf{J}\",
    \"math\": null
  },
  \"120286\": {
    \"char\": \"𝗞\",
    \"latex\": \"\\\\mathsfbf{K}\",
    \"math\": null
  },
  \"120287\": {
    \"char\": \"𝗟\",
    \"latex\": \"\\\\mathsfbf{L}\",
    \"math\": null
  },
  \"120288\": {
    \"char\": \"𝗠\",
    \"latex\": \"\\\\mathsfbf{M}\",
    \"math\": null
  },
  \"120289\": {
    \"char\": \"𝗡\",
    \"latex\": \"\\\\mathsfbf{N}\",
    \"math\": null
  },
  \"120290\": {
    \"char\": \"𝗢\",
    \"latex\": \"\\\\mathsfbf{O}\",
    \"math\": null
  },
  \"120291\": {
    \"char\": \"𝗣\",
    \"latex\": \"\\\\mathsfbf{P}\",
    \"math\": null
  },
  \"120292\": {
    \"char\": \"𝗤\",
    \"latex\": \"\\\\mathsfbf{Q}\",
    \"math\": null
  },
  \"120293\": {
    \"char\": \"𝗥\",
    \"latex\": \"\\\\mathsfbf{R}\",
    \"math\": null
  },
  \"120294\": {
    \"char\": \"𝗦\",
    \"latex\": \"\\\\mathsfbf{S}\",
    \"math\": null
  },
  \"120295\": {
    \"char\": \"𝗧\",
    \"latex\": \"\\\\mathsfbf{T}\",
    \"math\": null
  },
  \"120296\": {
    \"char\": \"𝗨\",
    \"latex\": \"\\\\mathsfbf{U}\",
    \"math\": null
  },
  \"120297\": {
    \"char\": \"𝗩\",
    \"latex\": \"\\\\mathsfbf{V}\",
    \"math\": null
  },
  \"120298\": {
    \"char\": \"𝗪\",
    \"latex\": \"\\\\mathsfbf{W}\",
    \"math\": null
  },
  \"120299\": {
    \"char\": \"𝗫\",
    \"latex\": \"\\\\mathsfbf{X}\",
    \"math\": null
  },
  \"120300\": {
    \"char\": \"𝗬\",
    \"latex\": \"\\\\mathsfbf{Y}\",
    \"math\": null
  },
  \"120301\": {
    \"char\": \"𝗭\",
    \"latex\": \"\\\\mathsfbf{Z}\",
    \"math\": null
  },
  \"120302\": {
    \"char\": \"𝗮\",
    \"latex\": \"\\\\mathsfbf{a}\",
    \"math\": null
  },
  \"120303\": {
    \"char\": \"𝗯\",
    \"latex\": \"\\\\mathsfbf{b}\",
    \"math\": null
  },
  \"120304\": {
    \"char\": \"𝗰\",
    \"latex\": \"\\\\mathsfbf{c}\",
    \"math\": null
  },
  \"120305\": {
    \"char\": \"𝗱\",
    \"latex\": \"\\\\mathsfbf{d}\",
    \"math\": null
  },
  \"120306\": {
    \"char\": \"𝗲\",
    \"latex\": \"\\\\mathsfbf{e}\",
    \"math\": null
  },
  \"120307\": {
    \"char\": \"𝗳\",
    \"latex\": \"\\\\mathsfbf{f}\",
    \"math\": null
  },
  \"120308\": {
    \"char\": \"𝗴\",
    \"latex\": \"\\\\mathsfbf{g}\",
    \"math\": null
  },
  \"120309\": {
    \"char\": \"𝗵\",
    \"latex\": \"\\\\mathsfbf{h}\",
    \"math\": null
  },
  \"120310\": {
    \"char\": \"𝗶\",
    \"latex\": \"\\\\mathsfbf{i}\",
    \"math\": null
  },
  \"120311\": {
    \"char\": \"𝗷\",
    \"latex\": \"\\\\mathsfbf{j}\",
    \"math\": null
  },
  \"120312\": {
    \"char\": \"𝗸\",
    \"latex\": \"\\\\mathsfbf{k}\",
    \"math\": null
  },
  \"120313\": {
    \"char\": \"𝗹\",
    \"latex\": \"\\\\mathsfbf{l}\",
    \"math\": null
  },
  \"120314\": {
    \"char\": \"𝗺\",
    \"latex\": \"\\\\mathsfbf{m}\",
    \"math\": null
  },
  \"120315\": {
    \"char\": \"𝗻\",
    \"latex\": \"\\\\mathsfbf{n}\",
    \"math\": null
  },
  \"120316\": {
    \"char\": \"𝗼\",
    \"latex\": \"\\\\mathsfbf{o}\",
    \"math\": null
  },
  \"120317\": {
    \"char\": \"𝗽\",
    \"latex\": \"\\\\mathsfbf{p}\",
    \"math\": null
  },
  \"120318\": {
    \"char\": \"𝗾\",
    \"latex\": \"\\\\mathsfbf{q}\",
    \"math\": null
  },
  \"120319\": {
    \"char\": \"𝗿\",
    \"latex\": \"\\\\mathsfbf{r}\",
    \"math\": null
  },
  \"120320\": {
    \"char\": \"𝘀\",
    \"latex\": \"\\\\mathsfbf{s}\",
    \"math\": null
  },
  \"120321\": {
    \"char\": \"𝘁\",
    \"latex\": \"\\\\mathsfbf{t}\",
    \"math\": null
  },
  \"120322\": {
    \"char\": \"𝘂\",
    \"latex\": \"\\\\mathsfbf{u}\",
    \"math\": null
  },
  \"120323\": {
    \"char\": \"𝘃\",
    \"latex\": \"\\\\mathsfbf{v}\",
    \"math\": null
  },
  \"120324\": {
    \"char\": \"𝘄\",
    \"latex\": \"\\\\mathsfbf{w}\",
    \"math\": null
  },
  \"120325\": {
    \"char\": \"𝘅\",
    \"latex\": \"\\\\mathsfbf{x}\",
    \"math\": null
  },
  \"120326\": {
    \"char\": \"𝘆\",
    \"latex\": \"\\\\mathsfbf{y}\",
    \"math\": null
  },
  \"120327\": {
    \"char\": \"𝘇\",
    \"latex\": \"\\\\mathsfbf{z}\",
    \"math\": null
  },
  \"120328\": {
    \"char\": \"𝘈\",
    \"latex\": \"\\\\mathsfsl{A}\",
    \"math\": null
  },
  \"120329\": {
    \"char\": \"𝘉\",
    \"latex\": \"\\\\mathsfsl{B}\",
    \"math\": null
  },
  \"120330\": {
    \"char\": \"𝘊\",
    \"latex\": \"\\\\mathsfsl{C}\",
    \"math\": null
  },
  \"120331\": {
    \"char\": \"𝘋\",
    \"latex\": \"\\\\mathsfsl{D}\",
    \"math\": null
  },
  \"120332\": {
    \"char\": \"𝘌\",
    \"latex\": \"\\\\mathsfsl{E}\",
    \"math\": null
  },
  \"120333\": {
    \"char\": \"𝘍\",
    \"latex\": \"\\\\mathsfsl{F}\",
    \"math\": null
  },
  \"120334\": {
    \"char\": \"𝘎\",
    \"latex\": \"\\\\mathsfsl{G}\",
    \"math\": null
  },
  \"120335\": {
    \"char\": \"𝘏\",
    \"latex\": \"\\\\mathsfsl{H}\",
    \"math\": null
  },
  \"120336\": {
    \"char\": \"𝘐\",
    \"latex\": \"\\\\mathsfsl{I}\",
    \"math\": null
  },
  \"120337\": {
    \"char\": \"𝘑\",
    \"latex\": \"\\\\mathsfsl{J}\",
    \"math\": null
  },
  \"120338\": {
    \"char\": \"𝘒\",
    \"latex\": \"\\\\mathsfsl{K}\",
    \"math\": null
  },
  \"120339\": {
    \"char\": \"𝘓\",
    \"latex\": \"\\\\mathsfsl{L}\",
    \"math\": null
  },
  \"120340\": {
    \"char\": \"𝘔\",
    \"latex\": \"\\\\mathsfsl{M}\",
    \"math\": null
  },
  \"120341\": {
    \"char\": \"𝘕\",
    \"latex\": \"\\\\mathsfsl{N}\",
    \"math\": null
  },
  \"120342\": {
    \"char\": \"𝘖\",
    \"latex\": \"\\\\mathsfsl{O}\",
    \"math\": null
  },
  \"120343\": {
    \"char\": \"𝘗\",
    \"latex\": \"\\\\mathsfsl{P}\",
    \"math\": null
  },
  \"120344\": {
    \"char\": \"𝘘\",
    \"latex\": \"\\\\mathsfsl{Q}\",
    \"math\": null
  },
  \"120345\": {
    \"char\": \"𝘙\",
    \"latex\": \"\\\\mathsfsl{R}\",
    \"math\": null
  },
  \"120346\": {
    \"char\": \"𝘚\",
    \"latex\": \"\\\\mathsfsl{S}\",
    \"math\": null
  },
  \"120347\": {
    \"char\": \"𝘛\",
    \"latex\": \"\\\\mathsfsl{T}\",
    \"math\": null
  },
  \"120348\": {
    \"char\": \"𝘜\",
    \"latex\": \"\\\\mathsfsl{U}\",
    \"math\": null
  },
  \"120349\": {
    \"char\": \"𝘝\",
    \"latex\": \"\\\\mathsfsl{V}\",
    \"math\": null
  },
  \"120350\": {
    \"char\": \"𝘞\",
    \"latex\": \"\\\\mathsfsl{W}\",
    \"math\": null
  },
  \"120351\": {
    \"char\": \"𝘟\",
    \"latex\": \"\\\\mathsfsl{X}\",
    \"math\": null
  },
  \"120352\": {
    \"char\": \"𝘠\",
    \"latex\": \"\\\\mathsfsl{Y}\",
    \"math\": null
  },
  \"120353\": {
    \"char\": \"𝘡\",
    \"latex\": \"\\\\mathsfsl{Z}\",
    \"math\": null
  },
  \"120354\": {
    \"char\": \"𝘢\",
    \"latex\": \"\\\\mathsfsl{a}\",
    \"math\": null
  },
  \"120355\": {
    \"char\": \"𝘣\",
    \"latex\": \"\\\\mathsfsl{b}\",
    \"math\": null
  },
  \"120356\": {
    \"char\": \"𝘤\",
    \"latex\": \"\\\\mathsfsl{c}\",
    \"math\": null
  },
  \"120357\": {
    \"char\": \"𝘥\",
    \"latex\": \"\\\\mathsfsl{d}\",
    \"math\": null
  },
  \"120358\": {
    \"char\": \"𝘦\",
    \"latex\": \"\\\\mathsfsl{e}\",
    \"math\": null
  },
  \"120359\": {
    \"char\": \"𝘧\",
    \"latex\": \"\\\\mathsfsl{f}\",
    \"math\": null
  },
  \"120360\": {
    \"char\": \"𝘨\",
    \"latex\": \"\\\\mathsfsl{g}\",
    \"math\": null
  },
  \"120361\": {
    \"char\": \"𝘩\",
    \"latex\": \"\\\\mathsfsl{h}\",
    \"math\": null
  },
  \"120362\": {
    \"char\": \"𝘪\",
    \"latex\": \"\\\\mathsfsl{i}\",
    \"math\": null
  },
  \"120363\": {
    \"char\": \"𝘫\",
    \"latex\": \"\\\\mathsfsl{j}\",
    \"math\": null
  },
  \"120364\": {
    \"char\": \"𝘬\",
    \"latex\": \"\\\\mathsfsl{k}\",
    \"math\": null
  },
  \"120365\": {
    \"char\": \"𝘭\",
    \"latex\": \"\\\\mathsfsl{l}\",
    \"math\": null
  },
  \"120366\": {
    \"char\": \"𝘮\",
    \"latex\": \"\\\\mathsfsl{m}\",
    \"math\": null
  },
  \"120367\": {
    \"char\": \"𝘯\",
    \"latex\": \"\\\\mathsfsl{n}\",
    \"math\": null
  },
  \"120368\": {
    \"char\": \"𝘰\",
    \"latex\": \"\\\\mathsfsl{o}\",
    \"math\": null
  },
  \"120369\": {
    \"char\": \"𝘱\",
    \"latex\": \"\\\\mathsfsl{p}\",
    \"math\": null
  },
  \"120370\": {
    \"char\": \"𝘲\",
    \"latex\": \"\\\\mathsfsl{q}\",
    \"math\": null
  },
  \"120371\": {
    \"char\": \"𝘳\",
    \"latex\": \"\\\\mathsfsl{r}\",
    \"math\": null
  },
  \"120372\": {
    \"char\": \"𝘴\",
    \"latex\": \"\\\\mathsfsl{s}\",
    \"math\": null
  },
  \"120373\": {
    \"char\": \"𝘵\",
    \"latex\": \"\\\\mathsfsl{t}\",
    \"math\": null
  },
  \"120374\": {
    \"char\": \"𝘶\",
    \"latex\": \"\\\\mathsfsl{u}\",
    \"math\": null
  },
  \"120375\": {
    \"char\": \"𝘷\",
    \"latex\": \"\\\\mathsfsl{v}\",
    \"math\": null
  },
  \"120376\": {
    \"char\": \"𝘸\",
    \"latex\": \"\\\\mathsfsl{w}\",
    \"math\": null
  },
  \"120377\": {
    \"char\": \"𝘹\",
    \"latex\": \"\\\\mathsfsl{x}\",
    \"math\": null
  },
  \"120378\": {
    \"char\": \"𝘺\",
    \"latex\": \"\\\\mathsfsl{y}\",
    \"math\": null
  },
  \"120379\": {
    \"char\": \"𝘻\",
    \"latex\": \"\\\\mathsfsl{z}\",
    \"math\": null
  },
  \"120380\": {
    \"char\": \"𝘼\",
    \"latex\": \"\\\\mathsfbfsl{A}\",
    \"math\": null
  },
  \"120381\": {
    \"char\": \"𝘽\",
    \"latex\": \"\\\\mathsfbfsl{B}\",
    \"math\": null
  },
  \"120382\": {
    \"char\": \"𝘾\",
    \"latex\": \"\\\\mathsfbfsl{C}\",
    \"math\": null
  },
  \"120383\": {
    \"char\": \"𝘿\",
    \"latex\": \"\\\\mathsfbfsl{D}\",
    \"math\": null
  },
  \"120384\": {
    \"char\": \"𝙀\",
    \"latex\": \"\\\\mathsfbfsl{E}\",
    \"math\": null
  },
  \"120385\": {
    \"char\": \"𝙁\",
    \"latex\": \"\\\\mathsfbfsl{F}\",
    \"math\": null
  },
  \"120386\": {
    \"char\": \"𝙂\",
    \"latex\": \"\\\\mathsfbfsl{G}\",
    \"math\": null
  },
  \"120387\": {
    \"char\": \"𝙃\",
    \"latex\": \"\\\\mathsfbfsl{H}\",
    \"math\": null
  },
  \"120388\": {
    \"char\": \"𝙄\",
    \"latex\": \"\\\\mathsfbfsl{I}\",
    \"math\": null
  },
  \"120389\": {
    \"char\": \"𝙅\",
    \"latex\": \"\\\\mathsfbfsl{J}\",
    \"math\": null
  },
  \"120390\": {
    \"char\": \"𝙆\",
    \"latex\": \"\\\\mathsfbfsl{K}\",
    \"math\": null
  },
  \"120391\": {
    \"char\": \"𝙇\",
    \"latex\": \"\\\\mathsfbfsl{L}\",
    \"math\": null
  },
  \"120392\": {
    \"char\": \"𝙈\",
    \"latex\": \"\\\\mathsfbfsl{M}\",
    \"math\": null
  },
  \"120393\": {
    \"char\": \"𝙉\",
    \"latex\": \"\\\\mathsfbfsl{N}\",
    \"math\": null
  },
  \"120394\": {
    \"char\": \"𝙊\",
    \"latex\": \"\\\\mathsfbfsl{O}\",
    \"math\": null
  },
  \"120395\": {
    \"char\": \"𝙋\",
    \"latex\": \"\\\\mathsfbfsl{P}\",
    \"math\": null
  },
  \"120396\": {
    \"char\": \"𝙌\",
    \"latex\": \"\\\\mathsfbfsl{Q}\",
    \"math\": null
  },
  \"120397\": {
    \"char\": \"𝙍\",
    \"latex\": \"\\\\mathsfbfsl{R}\",
    \"math\": null
  },
  \"120398\": {
    \"char\": \"𝙎\",
    \"latex\": \"\\\\mathsfbfsl{S}\",
    \"math\": null
  },
  \"120399\": {
    \"char\": \"𝙏\",
    \"latex\": \"\\\\mathsfbfsl{T}\",
    \"math\": null
  },
  \"120400\": {
    \"char\": \"𝙐\",
    \"latex\": \"\\\\mathsfbfsl{U}\",
    \"math\": null
  },
  \"120401\": {
    \"char\": \"𝙑\",
    \"latex\": \"\\\\mathsfbfsl{V}\",
    \"math\": null
  },
  \"120402\": {
    \"char\": \"𝙒\",
    \"latex\": \"\\\\mathsfbfsl{W}\",
    \"math\": null
  },
  \"120403\": {
    \"char\": \"𝙓\",
    \"latex\": \"\\\\mathsfbfsl{X}\",
    \"math\": null
  },
  \"120404\": {
    \"char\": \"𝙔\",
    \"latex\": \"\\\\mathsfbfsl{Y}\",
    \"math\": null
  },
  \"120405\": {
    \"char\": \"𝙕\",
    \"latex\": \"\\\\mathsfbfsl{Z}\",
    \"math\": null
  },
  \"120406\": {
    \"char\": \"𝙖\",
    \"latex\": \"\\\\mathsfbfsl{a}\",
    \"math\": null
  },
  \"120407\": {
    \"char\": \"𝙗\",
    \"latex\": \"\\\\mathsfbfsl{b}\",
    \"math\": null
  },
  \"120408\": {
    \"char\": \"𝙘\",
    \"latex\": \"\\\\mathsfbfsl{c}\",
    \"math\": null
  },
  \"120409\": {
    \"char\": \"𝙙\",
    \"latex\": \"\\\\mathsfbfsl{d}\",
    \"math\": null
  },
  \"120410\": {
    \"char\": \"𝙚\",
    \"latex\": \"\\\\mathsfbfsl{e}\",
    \"math\": null
  },
  \"120411\": {
    \"char\": \"𝙛\",
    \"latex\": \"\\\\mathsfbfsl{f}\",
    \"math\": null
  },
  \"120412\": {
    \"char\": \"𝙜\",
    \"latex\": \"\\\\mathsfbfsl{g}\",
    \"math\": null
  },
  \"120413\": {
    \"char\": \"𝙝\",
    \"latex\": \"\\\\mathsfbfsl{h}\",
    \"math\": null
  },
  \"120414\": {
    \"char\": \"𝙞\",
    \"latex\": \"\\\\mathsfbfsl{i}\",
    \"math\": null
  },
  \"120415\": {
    \"char\": \"𝙟\",
    \"latex\": \"\\\\mathsfbfsl{j}\",
    \"math\": null
  },
  \"120416\": {
    \"char\": \"𝙠\",
    \"latex\": \"\\\\mathsfbfsl{k}\",
    \"math\": null
  },
  \"120417\": {
    \"char\": \"𝙡\",
    \"latex\": \"\\\\mathsfbfsl{l}\",
    \"math\": null
  },
  \"120418\": {
    \"char\": \"𝙢\",
    \"latex\": \"\\\\mathsfbfsl{m}\",
    \"math\": null
  },
  \"120419\": {
    \"char\": \"𝙣\",
    \"latex\": \"\\\\mathsfbfsl{n}\",
    \"math\": null
  },
  \"120420\": {
    \"char\": \"𝙤\",
    \"latex\": \"\\\\mathsfbfsl{o}\",
    \"math\": null
  },
  \"120421\": {
    \"char\": \"𝙥\",
    \"latex\": \"\\\\mathsfbfsl{p}\",
    \"math\": null
  },
  \"120422\": {
    \"char\": \"𝙦\",
    \"latex\": \"\\\\mathsfbfsl{q}\",
    \"math\": null
  },
  \"120423\": {
    \"char\": \"𝙧\",
    \"latex\": \"\\\\mathsfbfsl{r}\",
    \"math\": null
  },
  \"120424\": {
    \"char\": \"𝙨\",
    \"latex\": \"\\\\mathsfbfsl{s}\",
    \"math\": null
  },
  \"120425\": {
    \"char\": \"𝙩\",
    \"latex\": \"\\\\mathsfbfsl{t}\",
    \"math\": null
  },
  \"120426\": {
    \"char\": \"𝙪\",
    \"latex\": \"\\\\mathsfbfsl{u}\",
    \"math\": null
  },
  \"120427\": {
    \"char\": \"𝙫\",
    \"latex\": \"\\\\mathsfbfsl{v}\",
    \"math\": null
  },
  \"120428\": {
    \"char\": \"𝙬\",
    \"latex\": \"\\\\mathsfbfsl{w}\",
    \"math\": null
  },
  \"120429\": {
    \"char\": \"𝙭\",
    \"latex\": \"\\\\mathsfbfsl{x}\",
    \"math\": null
  },
  \"120430\": {
    \"char\": \"𝙮\",
    \"latex\": \"\\\\mathsfbfsl{y}\",
    \"math\": null
  },
  \"120431\": {
    \"char\": \"𝙯\",
    \"latex\": \"\\\\mathsfbfsl{z}\",
    \"math\": null
  },
  \"120432\": {
    \"char\": \"𝙰\",
    \"latex\": \"\\\\mathtt{A}\",
    \"math\": null
  },
  \"120433\": {
    \"char\": \"𝙱\",
    \"latex\": \"\\\\mathtt{B}\",
    \"math\": null
  },
  \"120434\": {
    \"char\": \"𝙲\",
    \"latex\": \"\\\\mathtt{C}\",
    \"math\": null
  },
  \"120435\": {
    \"char\": \"𝙳\",
    \"latex\": \"\\\\mathtt{D}\",
    \"math\": null
  },
  \"120436\": {
    \"char\": \"𝙴\",
    \"latex\": \"\\\\mathtt{E}\",
    \"math\": null
  },
  \"120437\": {
    \"char\": \"𝙵\",
    \"latex\": \"\\\\mathtt{F}\",
    \"math\": null
  },
  \"120438\": {
    \"char\": \"𝙶\",
    \"latex\": \"\\\\mathtt{G}\",
    \"math\": null
  },
  \"120439\": {
    \"char\": \"𝙷\",
    \"latex\": \"\\\\mathtt{H}\",
    \"math\": null
  },
  \"120440\": {
    \"char\": \"𝙸\",
    \"latex\": \"\\\\mathtt{I}\",
    \"math\": null
  },
  \"120441\": {
    \"char\": \"𝙹\",
    \"latex\": \"\\\\mathtt{J}\",
    \"math\": null
  },
  \"120442\": {
    \"char\": \"𝙺\",
    \"latex\": \"\\\\mathtt{K}\",
    \"math\": null
  },
  \"120443\": {
    \"char\": \"𝙻\",
    \"latex\": \"\\\\mathtt{L}\",
    \"math\": null
  },
  \"120444\": {
    \"char\": \"𝙼\",
    \"latex\": \"\\\\mathtt{M}\",
    \"math\": null
  },
  \"120445\": {
    \"char\": \"𝙽\",
    \"latex\": \"\\\\mathtt{N}\",
    \"math\": null
  },
  \"120446\": {
    \"char\": \"𝙾\",
    \"latex\": \"\\\\mathtt{O}\",
    \"math\": null
  },
  \"120447\": {
    \"char\": \"𝙿\",
    \"latex\": \"\\\\mathtt{P}\",
    \"math\": null
  },
  \"120448\": {
    \"char\": \"𝚀\",
    \"latex\": \"\\\\mathtt{Q}\",
    \"math\": null
  },
  \"120449\": {
    \"char\": \"𝚁\",
    \"latex\": \"\\\\mathtt{R}\",
    \"math\": null
  },
  \"120450\": {
    \"char\": \"𝚂\",
    \"latex\": \"\\\\mathtt{S}\",
    \"math\": null
  },
  \"120451\": {
    \"char\": \"𝚃\",
    \"latex\": \"\\\\mathtt{T}\",
    \"math\": null
  },
  \"120452\": {
    \"char\": \"𝚄\",
    \"latex\": \"\\\\mathtt{U}\",
    \"math\": null
  },
  \"120453\": {
    \"char\": \"𝚅\",
    \"latex\": \"\\\\mathtt{V}\",
    \"math\": null
  },
  \"120454\": {
    \"char\": \"𝚆\",
    \"latex\": \"\\\\mathtt{W}\",
    \"math\": null
  },
  \"120455\": {
    \"char\": \"𝚇\",
    \"latex\": \"\\\\mathtt{X}\",
    \"math\": null
  },
  \"120456\": {
    \"char\": \"𝚈\",
    \"latex\": \"\\\\mathtt{Y}\",
    \"math\": null
  },
  \"120457\": {
    \"char\": \"𝚉\",
    \"latex\": \"\\\\mathtt{Z}\",
    \"math\": null
  },
  \"120458\": {
    \"char\": \"𝚊\",
    \"latex\": \"\\\\mathtt{a}\",
    \"math\": null
  },
  \"120459\": {
    \"char\": \"𝚋\",
    \"latex\": \"\\\\mathtt{b}\",
    \"math\": null
  },
  \"120460\": {
    \"char\": \"𝚌\",
    \"latex\": \"\\\\mathtt{c}\",
    \"math\": null
  },
  \"120461\": {
    \"char\": \"𝚍\",
    \"latex\": \"\\\\mathtt{d}\",
    \"math\": null
  },
  \"120462\": {
    \"char\": \"𝚎\",
    \"latex\": \"\\\\mathtt{e}\",
    \"math\": null
  },
  \"120463\": {
    \"char\": \"𝚏\",
    \"latex\": \"\\\\mathtt{f}\",
    \"math\": null
  },
  \"120464\": {
    \"char\": \"𝚐\",
    \"latex\": \"\\\\mathtt{g}\",
    \"math\": null
  },
  \"120465\": {
    \"char\": \"𝚑\",
    \"latex\": \"\\\\mathtt{h}\",
    \"math\": null
  },
  \"120466\": {
    \"char\": \"𝚒\",
    \"latex\": \"\\\\mathtt{i}\",
    \"math\": null
  },
  \"120467\": {
    \"char\": \"𝚓\",
    \"latex\": \"\\\\mathtt{j}\",
    \"math\": null
  },
  \"120468\": {
    \"char\": \"𝚔\",
    \"latex\": \"\\\\mathtt{k}\",
    \"math\": null
  },
  \"120469\": {
    \"char\": \"𝚕\",
    \"latex\": \"\\\\mathtt{l}\",
    \"math\": null
  },
  \"120470\": {
    \"char\": \"𝚖\",
    \"latex\": \"\\\\mathtt{m}\",
    \"math\": null
  },
  \"120471\": {
    \"char\": \"𝚗\",
    \"latex\": \"\\\\mathtt{n}\",
    \"math\": null
  },
  \"120472\": {
    \"char\": \"𝚘\",
    \"latex\": \"\\\\mathtt{o}\",
    \"math\": null
  },
  \"120473\": {
    \"char\": \"𝚙\",
    \"latex\": \"\\\\mathtt{p}\",
    \"math\": null
  },
  \"120474\": {
    \"char\": \"𝚚\",
    \"latex\": \"\\\\mathtt{q}\",
    \"math\": null
  },
  \"120475\": {
    \"char\": \"𝚛\",
    \"latex\": \"\\\\mathtt{r}\",
    \"math\": null
  },
  \"120476\": {
    \"char\": \"𝚜\",
    \"latex\": \"\\\\mathtt{s}\",
    \"math\": null
  },
  \"120477\": {
    \"char\": \"𝚝\",
    \"latex\": \"\\\\mathtt{t}\",
    \"math\": null
  },
  \"120478\": {
    \"char\": \"𝚞\",
    \"latex\": \"\\\\mathtt{u}\",
    \"math\": null
  },
  \"120479\": {
    \"char\": \"𝚟\",
    \"latex\": \"\\\\mathtt{v}\",
    \"math\": null
  },
  \"120480\": {
    \"char\": \"𝚠\",
    \"latex\": \"\\\\mathtt{w}\",
    \"math\": null
  },
  \"120481\": {
    \"char\": \"𝚡\",
    \"latex\": \"\\\\mathtt{x}\",
    \"math\": null
  },
  \"120482\": {
    \"char\": \"𝚢\",
    \"latex\": \"\\\\mathtt{y}\",
    \"math\": null
  },
  \"120483\": {
    \"char\": \"𝚣\",
    \"latex\": \"\\\\mathtt{z}\",
    \"math\": null
  },
  \"120488\": {
    \"char\": \"𝚨\",
    \"latex\": \"\\\\mathbf{\\\\Alpha}\",
    \"math\": null
  },
  \"120489\": {
    \"char\": \"𝚩\",
    \"latex\": \"\\\\mathbf{\\\\Beta}\",
    \"math\": null
  },
  \"120490\": {
    \"char\": \"𝚪\",
    \"latex\": \"\\\\mathbf{\\\\Gamma}\",
    \"math\": null
  },
  \"120491\": {
    \"char\": \"𝚫\",
    \"latex\": \"\\\\mathbf{\\\\Delta}\",
    \"math\": null
  },
  \"120492\": {
    \"char\": \"𝚬\",
    \"latex\": \"\\\\mathbf{\\\\Epsilon}\",
    \"math\": null
  },
  \"120493\": {
    \"char\": \"𝚭\",
    \"latex\": \"\\\\mathbf{\\\\Zeta}\",
    \"math\": null
  },
  \"120494\": {
    \"char\": \"𝚮\",
    \"latex\": \"\\\\mathbf{\\\\Eta}\",
    \"math\": null
  },
  \"120495\": {
    \"char\": \"𝚯\",
    \"latex\": \"\\\\mathbf{\\\\Theta}\",
    \"math\": null
  },
  \"120496\": {
    \"char\": \"𝚰\",
    \"latex\": \"\\\\mathbf{\\\\Iota}\",
    \"math\": null
  },
  \"120497\": {
    \"char\": \"𝚱\",
    \"latex\": \"\\\\mathbf{\\\\Kappa}\",
    \"math\": null
  },
  \"120498\": {
    \"char\": \"𝚲\",
    \"latex\": \"\\\\mathbf{\\\\Lambda}\",
    \"math\": null
  },
  \"120499\": {
    \"char\": \"𝚳\",
    \"latex\": \"M\",
    \"math\": null
  },
  \"120500\": {
    \"char\": \"𝚴\",
    \"latex\": \"N\",
    \"math\": null
  },
  \"120501\": {
    \"char\": \"𝚵\",
    \"latex\": \"\\\\mathbf{\\\\Xi}\",
    \"math\": null
  },
  \"120502\": {
    \"char\": \"𝚶\",
    \"latex\": \"O\",
    \"math\": null
  },
  \"120503\": {
    \"char\": \"𝚷\",
    \"latex\": \"\\\\mathbf{\\\\Pi}\",
    \"math\": null
  },
  \"120504\": {
    \"char\": \"𝚸\",
    \"latex\": \"\\\\mathbf{\\\\Rho}\",
    \"math\": null
  },
  \"120505\": {
    \"char\": \"𝚹\",
    \"latex\": \"\\\\mathbf{\\\\vartheta}\",
    \"math\": null
  },
  \"120506\": {
    \"char\": \"𝚺\",
    \"latex\": \"\\\\mathbf{\\\\Sigma}\",
    \"math\": null
  },
  \"120507\": {
    \"char\": \"𝚻\",
    \"latex\": \"\\\\mathbf{\\\\Tau}\",
    \"math\": null
  },
  \"120508\": {
    \"char\": \"𝚼\",
    \"latex\": \"\\\\mathbf{\\\\Upsilon}\",
    \"math\": null
  },
  \"120509\": {
    \"char\": \"𝚽\",
    \"latex\": \"\\\\mathbf{\\\\Phi}\",
    \"math\": null
  },
  \"120510\": {
    \"char\": \"𝚾\",
    \"latex\": \"\\\\mathbf{\\\\Chi}\",
    \"math\": null
  },
  \"120511\": {
    \"char\": \"𝚿\",
    \"latex\": \"\\\\mathbf{\\\\Psi}\",
    \"math\": null
  },
  \"120512\": {
    \"char\": \"𝛀\",
    \"latex\": \"\\\\mathbf{\\\\Omega}\",
    \"math\": null
  },
  \"120513\": {
    \"char\": \"𝛁\",
    \"latex\": \"\\\\mathbf{\\\\nabla}\",
    \"math\": null
  },
  \"120514\": {
    \"char\": \"𝛂\",
    \"latex\": \"\\\\mathbf{\\\\Alpha}\",
    \"math\": null
  },
  \"120515\": {
    \"char\": \"𝛃\",
    \"latex\": \"\\\\mathbf{\\\\Beta}\",
    \"math\": null
  },
  \"120516\": {
    \"char\": \"𝛄\",
    \"latex\": \"\\\\mathbf{\\\\Gamma}\",
    \"math\": null
  },
  \"120517\": {
    \"char\": \"𝛅\",
    \"latex\": \"\\\\mathbf{\\\\Delta}\",
    \"math\": null
  },
  \"120518\": {
    \"char\": \"𝛆\",
    \"latex\": \"\\\\mathbf{\\\\Epsilon}\",
    \"math\": null
  },
  \"120519\": {
    \"char\": \"𝛇\",
    \"latex\": \"\\\\mathbf{\\\\Zeta}\",
    \"math\": null
  },
  \"120520\": {
    \"char\": \"𝛈\",
    \"latex\": \"\\\\mathbf{\\\\Eta}\",
    \"math\": null
  },
  \"120521\": {
    \"char\": \"𝛉\",
    \"latex\": \"\\\\mathbf{\\\\theta}\",
    \"math\": null
  },
  \"120522\": {
    \"char\": \"𝛊\",
    \"latex\": \"\\\\mathbf{\\\\Iota}\",
    \"math\": null
  },
  \"120523\": {
    \"char\": \"𝛋\",
    \"latex\": \"\\\\mathbf{\\\\Kappa}\",
    \"math\": null
  },
  \"120524\": {
    \"char\": \"𝛌\",
    \"latex\": \"\\\\mathbf{\\\\Lambda}\",
    \"math\": null
  },
  \"120525\": {
    \"char\": \"𝛍\",
    \"latex\": \"M\",
    \"math\": null
  },
  \"120526\": {
    \"char\": \"𝛎\",
    \"latex\": \"N\",
    \"math\": null
  },
  \"120527\": {
    \"char\": \"𝛏\",
    \"latex\": \"\\\\mathbf{\\\\Xi}\",
    \"math\": null
  },
  \"120528\": {
    \"char\": \"𝛐\",
    \"latex\": \"O\",
    \"math\": null
  },
  \"120529\": {
    \"char\": \"𝛑\",
    \"latex\": \"\\\\mathbf{\\\\Pi}\",
    \"math\": null
  },
  \"120530\": {
    \"char\": \"𝛒\",
    \"latex\": \"\\\\mathbf{\\\\Rho}\",
    \"math\": null
  },
  \"120531\": {
    \"char\": \"𝛓\",
    \"latex\": \"\\\\mathbf{\\\\varsigma}\",
    \"math\": null
  },
  \"120532\": {
    \"char\": \"𝛔\",
    \"latex\": \"\\\\mathbf{\\\\Sigma}\",
    \"math\": null
  },
  \"120533\": {
    \"char\": \"𝛕\",
    \"latex\": \"\\\\mathbf{\\\\Tau}\",
    \"math\": null
  },
  \"120534\": {
    \"char\": \"𝛖\",
    \"latex\": \"\\\\mathbf{\\\\Upsilon}\",
    \"math\": null
  },
  \"120535\": {
    \"char\": \"𝛗\",
    \"latex\": \"\\\\mathbf{\\\\Phi}\",
    \"math\": null
  },
  \"120536\": {
    \"char\": \"𝛘\",
    \"latex\": \"\\\\mathbf{\\\\Chi}\",
    \"math\": null
  },
  \"120537\": {
    \"char\": \"𝛙\",
    \"latex\": \"\\\\mathbf{\\\\Psi}\",
    \"math\": null
  },
  \"120538\": {
    \"char\": \"𝛚\",
    \"latex\": \"\\\\mathbf{\\\\Omega}\",
    \"math\": null
  },
  \"120539\": {
    \"char\": \"𝛛\",
    \"latex\": \"\\\\partial \",
    \"math\": null
  },
  \"120540\": {
    \"char\": \"𝛜\",
    \"latex\": \"\\\\in\",
    \"math\": null
  },
  \"120541\": {
    \"char\": \"𝛝\",
    \"latex\": \"\\\\mathbf{\\\\vartheta}\",
    \"math\": null
  },
  \"120542\": {
    \"char\": \"𝛞\",
    \"latex\": \"\\\\mathbf{\\\\varkappa}\",
    \"math\": null
  },
  \"120543\": {
    \"char\": \"𝛟\",
    \"latex\": \"\\\\mathbf{\\\\phi}\",
    \"math\": null
  },
  \"120544\": {
    \"char\": \"𝛠\",
    \"latex\": \"\\\\mathbf{\\\\varrho}\",
    \"math\": null
  },
  \"120545\": {
    \"char\": \"𝛡\",
    \"latex\": \"\\\\mathbf{\\\\varpi}\",
    \"math\": null
  },
  \"120546\": {
    \"char\": \"𝛢\",
    \"latex\": \"\\\\mathsl{\\\\Alpha}\",
    \"math\": null
  },
  \"120547\": {
    \"char\": \"𝛣\",
    \"latex\": \"\\\\mathsl{\\\\Beta}\",
    \"math\": null
  },
  \"120548\": {
    \"char\": \"𝛤\",
    \"latex\": \"\\\\mathsl{\\\\Gamma}\",
    \"math\": null
  },
  \"120549\": {
    \"char\": \"𝛥\",
    \"latex\": \"\\\\mathsl{\\\\Delta}\",
    \"math\": null
  },
  \"120550\": {
    \"char\": \"𝛦\",
    \"latex\": \"\\\\mathsl{\\\\Epsilon}\",
    \"math\": null
  },
  \"120551\": {
    \"char\": \"𝛧\",
    \"latex\": \"\\\\mathsl{\\\\Zeta}\",
    \"math\": null
  },
  \"120552\": {
    \"char\": \"𝛨\",
    \"latex\": \"\\\\mathsl{\\\\Eta}\",
    \"math\": null
  },
  \"120553\": {
    \"char\": \"𝛩\",
    \"latex\": \"\\\\mathsl{\\\\Theta}\",
    \"math\": null
  },
  \"120554\": {
    \"char\": \"𝛪\",
    \"latex\": \"\\\\mathsl{\\\\Iota}\",
    \"math\": null
  },
  \"120555\": {
    \"char\": \"𝛫\",
    \"latex\": \"\\\\mathsl{\\\\Kappa}\",
    \"math\": null
  },
  \"120556\": {
    \"char\": \"𝛬\",
    \"latex\": \"\\\\mathsl{\\\\Lambda}\",
    \"math\": null
  },
  \"120557\": {
    \"char\": \"𝛭\",
    \"latex\": \"M\",
    \"math\": null
  },
  \"120558\": {
    \"char\": \"𝛮\",
    \"latex\": \"N\",
    \"math\": null
  },
  \"120559\": {
    \"char\": \"𝛯\",
    \"latex\": \"\\\\mathsl{\\\\Xi}\",
    \"math\": null
  },
  \"120560\": {
    \"char\": \"𝛰\",
    \"latex\": \"O\",
    \"math\": null
  },
  \"120561\": {
    \"char\": \"𝛱\",
    \"latex\": \"\\\\mathsl{\\\\Pi}\",
    \"math\": null
  },
  \"120562\": {
    \"char\": \"𝛲\",
    \"latex\": \"\\\\mathsl{\\\\Rho}\",
    \"math\": null
  },
  \"120563\": {
    \"char\": \"𝛳\",
    \"latex\": \"\\\\mathsl{\\\\vartheta}\",
    \"math\": null
  },
  \"120564\": {
    \"char\": \"𝛴\",
    \"latex\": \"\\\\mathsl{\\\\Sigma}\",
    \"math\": null
  },
  \"120565\": {
    \"char\": \"𝛵\",
    \"latex\": \"\\\\mathsl{\\\\Tau}\",
    \"math\": null
  },
  \"120566\": {
    \"char\": \"𝛶\",
    \"latex\": \"\\\\mathsl{\\\\Upsilon}\",
    \"math\": null
  },
  \"120567\": {
    \"char\": \"𝛷\",
    \"latex\": \"\\\\mathsl{\\\\Phi}\",
    \"math\": null
  },
  \"120568\": {
    \"char\": \"𝛸\",
    \"latex\": \"\\\\mathsl{\\\\Chi}\",
    \"math\": null
  },
  \"120569\": {
    \"char\": \"𝛹\",
    \"latex\": \"\\\\mathsl{\\\\Psi}\",
    \"math\": null
  },
  \"120570\": {
    \"char\": \"𝛺\",
    \"latex\": \"\\\\mathsl{\\\\Omega}\",
    \"math\": null
  },
  \"120571\": {
    \"char\": \"𝛻\",
    \"latex\": \"\\\\mathsl{\\\\nabla}\",
    \"math\": null
  },
  \"120572\": {
    \"char\": \"𝛼\",
    \"latex\": \"\\\\mathsl{\\\\Alpha}\",
    \"math\": null
  },
  \"120573\": {
    \"char\": \"𝛽\",
    \"latex\": \"\\\\mathsl{\\\\Beta}\",
    \"math\": null
  },
  \"120574\": {
    \"char\": \"𝛾\",
    \"latex\": \"\\\\mathsl{\\\\Gamma}\",
    \"math\": null
  },
  \"120575\": {
    \"char\": \"𝛿\",
    \"latex\": \"\\\\mathsl{\\\\Delta}\",
    \"math\": null
  },
  \"120576\": {
    \"char\": \"𝜀\",
    \"latex\": \"\\\\mathsl{\\\\Epsilon}\",
    \"math\": null
  },
  \"120577\": {
    \"char\": \"𝜁\",
    \"latex\": \"\\\\mathsl{\\\\Zeta}\",
    \"math\": null
  },
  \"120578\": {
    \"char\": \"𝜂\",
    \"latex\": \"\\\\mathsl{\\\\Eta}\",
    \"math\": null
  },
  \"120579\": {
    \"char\": \"𝜃\",
    \"latex\": \"\\\\mathsl{\\\\Theta}\",
    \"math\": null
  },
  \"120580\": {
    \"char\": \"𝜄\",
    \"latex\": \"\\\\mathsl{\\\\Iota}\",
    \"math\": null
  },
  \"120581\": {
    \"char\": \"𝜅\",
    \"latex\": \"\\\\mathsl{\\\\Kappa}\",
    \"math\": null
  },
  \"120582\": {
    \"char\": \"𝜆\",
    \"latex\": \"\\\\mathsl{\\\\Lambda}\",
    \"math\": null
  },
  \"120583\": {
    \"char\": \"𝜇\",
    \"latex\": \"M\",
    \"math\": null
  },
  \"120584\": {
    \"char\": \"𝜈\",
    \"latex\": \"N\",
    \"math\": null
  },
  \"120585\": {
    \"char\": \"𝜉\",
    \"latex\": \"\\\\mathsl{\\\\Xi}\",
    \"math\": null
  },
  \"120586\": {
    \"char\": \"𝜊\",
    \"latex\": \"O\",
    \"math\": null
  },
  \"120587\": {
    \"char\": \"𝜋\",
    \"latex\": \"\\\\mathsl{\\\\Pi}\",
    \"math\": null
  },
  \"120588\": {
    \"char\": \"𝜌\",
    \"latex\": \"\\\\mathsl{\\\\Rho}\",
    \"math\": null
  },
  \"120589\": {
    \"char\": \"𝜍\",
    \"latex\": \"\\\\mathsl{\\\\varsigma}\",
    \"math\": null
  },
  \"120590\": {
    \"char\": \"𝜎\",
    \"latex\": \"\\\\mathsl{\\\\Sigma}\",
    \"math\": null
  },
  \"120591\": {
    \"char\": \"𝜏\",
    \"latex\": \"\\\\mathsl{\\\\Tau}\",
    \"math\": null
  },
  \"120592\": {
    \"char\": \"𝜐\",
    \"latex\": \"\\\\mathsl{\\\\Upsilon}\",
    \"math\": null
  },
  \"120593\": {
    \"char\": \"𝜑\",
    \"latex\": \"\\\\mathsl{\\\\Phi}\",
    \"math\": null
  },
  \"120594\": {
    \"char\": \"𝜒\",
    \"latex\": \"\\\\mathsl{\\\\Chi}\",
    \"math\": null
  },
  \"120595\": {
    \"char\": \"𝜓\",
    \"latex\": \"\\\\mathsl{\\\\Psi}\",
    \"math\": null
  },
  \"120596\": {
    \"char\": \"𝜔\",
    \"latex\": \"\\\\mathsl{\\\\Omega}\",
    \"math\": null
  },
  \"120597\": {
    \"char\": \"𝜕\",
    \"latex\": \"\\\\partial \",
    \"math\": null
  },
  \"120598\": {
    \"char\": \"𝜖\",
    \"latex\": \"\\\\in\",
    \"math\": null
  },
  \"120599\": {
    \"char\": \"𝜗\",
    \"latex\": \"\\\\mathsl{\\\\vartheta}\",
    \"math\": null
  },
  \"120600\": {
    \"char\": \"𝜘\",
    \"latex\": \"\\\\mathsl{\\\\varkappa}\",
    \"math\": null
  },
  \"120601\": {
    \"char\": \"𝜙\",
    \"latex\": \"\\\\mathsl{\\\\phi}\",
    \"math\": null
  },
  \"120602\": {
    \"char\": \"𝜚\",
    \"latex\": \"\\\\mathsl{\\\\varrho}\",
    \"math\": null
  },
  \"120603\": {
    \"char\": \"𝜛\",
    \"latex\": \"\\\\mathsl{\\\\varpi}\",
    \"math\": null
  },
  \"120604\": {
    \"char\": \"𝜜\",
    \"latex\": \"\\\\mathbit{\\\\Alpha}\",
    \"math\": null
  },
  \"120605\": {
    \"char\": \"𝜝\",
    \"latex\": \"\\\\mathbit{\\\\Beta}\",
    \"math\": null
  },
  \"120606\": {
    \"char\": \"𝜞\",
    \"latex\": \"\\\\mathbit{\\\\Gamma}\",
    \"math\": null
  },
  \"120607\": {
    \"char\": \"𝜟\",
    \"latex\": \"\\\\mathbit{\\\\Delta}\",
    \"math\": null
  },
  \"120608\": {
    \"char\": \"𝜠\",
    \"latex\": \"\\\\mathbit{\\\\Epsilon}\",
    \"math\": null
  },
  \"120609\": {
    \"char\": \"𝜡\",
    \"latex\": \"\\\\mathbit{\\\\Zeta}\",
    \"math\": null
  },
  \"120610\": {
    \"char\": \"𝜢\",
    \"latex\": \"\\\\mathbit{\\\\Eta}\",
    \"math\": null
  },
  \"120611\": {
    \"char\": \"𝜣\",
    \"latex\": \"\\\\mathbit{\\\\Theta}\",
    \"math\": null
  },
  \"120612\": {
    \"char\": \"𝜤\",
    \"latex\": \"\\\\mathbit{\\\\Iota}\",
    \"math\": null
  },
  \"120613\": {
    \"char\": \"𝜥\",
    \"latex\": \"\\\\mathbit{\\\\Kappa}\",
    \"math\": null
  },
  \"120614\": {
    \"char\": \"𝜦\",
    \"latex\": \"\\\\mathbit{\\\\Lambda}\",
    \"math\": null
  },
  \"120615\": {
    \"char\": \"𝜧\",
    \"latex\": \"M\",
    \"math\": null
  },
  \"120616\": {
    \"char\": \"𝜨\",
    \"latex\": \"N\",
    \"math\": null
  },
  \"120617\": {
    \"char\": \"𝜩\",
    \"latex\": \"\\\\mathbit{\\\\Xi}\",
    \"math\": null
  },
  \"120618\": {
    \"char\": \"𝜪\",
    \"latex\": \"O\",
    \"math\": null
  },
  \"120619\": {
    \"char\": \"𝜫\",
    \"latex\": \"\\\\mathbit{\\\\Pi}\",
    \"math\": null
  },
  \"120620\": {
    \"char\": \"𝜬\",
    \"latex\": \"\\\\mathbit{\\\\Rho}\",
    \"math\": null
  },
  \"120621\": {
    \"char\": \"𝜭\",
    \"latex\": \"\\\\mathbit{O}\",
    \"math\": null
  },
  \"120622\": {
    \"char\": \"𝜮\",
    \"latex\": \"\\\\mathbit{\\\\Sigma}\",
    \"math\": null
  },
  \"120623\": {
    \"char\": \"𝜯\",
    \"latex\": \"\\\\mathbit{\\\\Tau}\",
    \"math\": null
  },
  \"120624\": {
    \"char\": \"𝜰\",
    \"latex\": \"\\\\mathbit{\\\\Upsilon}\",
    \"math\": null
  },
  \"120625\": {
    \"char\": \"𝜱\",
    \"latex\": \"\\\\mathbit{\\\\Phi}\",
    \"math\": null
  },
  \"120626\": {
    \"char\": \"𝜲\",
    \"latex\": \"\\\\mathbit{\\\\Chi}\",
    \"math\": null
  },
  \"120627\": {
    \"char\": \"𝜳\",
    \"latex\": \"\\\\mathbit{\\\\Psi}\",
    \"math\": null
  },
  \"120628\": {
    \"char\": \"𝜴\",
    \"latex\": \"\\\\mathbit{\\\\Omega}\",
    \"math\": null
  },
  \"120629\": {
    \"char\": \"𝜵\",
    \"latex\": \"\\\\mathbit{\\\\nabla}\",
    \"math\": null
  },
  \"120630\": {
    \"char\": \"𝜶\",
    \"latex\": \"\\\\mathbit{\\\\Alpha}\",
    \"math\": null
  },
  \"120631\": {
    \"char\": \"𝜷\",
    \"latex\": \"\\\\mathbit{\\\\Beta}\",
    \"math\": null
  },
  \"120632\": {
    \"char\": \"𝜸\",
    \"latex\": \"\\\\mathbit{\\\\Gamma}\",
    \"math\": null
  },
  \"120633\": {
    \"char\": \"𝜹\",
    \"latex\": \"\\\\mathbit{\\\\Delta}\",
    \"math\": null
  },
  \"120634\": {
    \"char\": \"𝜺\",
    \"latex\": \"\\\\mathbit{\\\\Epsilon}\",
    \"math\": null
  },
  \"120635\": {
    \"char\": \"𝜻\",
    \"latex\": \"\\\\mathbit{\\\\Zeta}\",
    \"math\": null
  },
  \"120636\": {
    \"char\": \"𝜼\",
    \"latex\": \"\\\\mathbit{\\\\Eta}\",
    \"math\": null
  },
  \"120637\": {
    \"char\": \"𝜽\",
    \"latex\": \"\\\\mathbit{\\\\Theta}\",
    \"math\": null
  },
  \"120638\": {
    \"char\": \"𝜾\",
    \"latex\": \"\\\\mathbit{\\\\Iota}\",
    \"math\": null
  },
  \"120639\": {
    \"char\": \"𝜿\",
    \"latex\": \"\\\\mathbit{\\\\Kappa}\",
    \"math\": null
  },
  \"120640\": {
    \"char\": \"𝝀\",
    \"latex\": \"\\\\mathbit{\\\\Lambda}\",
    \"math\": null
  },
  \"120641\": {
    \"char\": \"𝝁\",
    \"latex\": \"M\",
    \"math\": null
  },
  \"120642\": {
    \"char\": \"𝝂\",
    \"latex\": \"N\",
    \"math\": null
  },
  \"120643\": {
    \"char\": \"𝝃\",
    \"latex\": \"\\\\mathbit{\\\\Xi}\",
    \"math\": null
  },
  \"120644\": {
    \"char\": \"𝝄\",
    \"latex\": \"O\",
    \"math\": null
  },
  \"120645\": {
    \"char\": \"𝝅\",
    \"latex\": \"\\\\mathbit{\\\\Pi}\",
    \"math\": null
  },
  \"120646\": {
    \"char\": \"𝝆\",
    \"latex\": \"\\\\mathbit{\\\\Rho}\",
    \"math\": null
  },
  \"120647\": {
    \"char\": \"𝝇\",
    \"latex\": \"\\\\mathbit{\\\\varsigma}\",
    \"math\": null
  },
  \"120648\": {
    \"char\": \"𝝈\",
    \"latex\": \"\\\\mathbit{\\\\Sigma}\",
    \"math\": null
  },
  \"120649\": {
    \"char\": \"𝝉\",
    \"latex\": \"\\\\mathbit{\\\\Tau}\",
    \"math\": null
  },
  \"120650\": {
    \"char\": \"𝝊\",
    \"latex\": \"\\\\mathbit{\\\\Upsilon}\",
    \"math\": null
  },
  \"120651\": {
    \"char\": \"𝝋\",
    \"latex\": \"\\\\mathbit{\\\\Phi}\",
    \"math\": null
  },
  \"120652\": {
    \"char\": \"𝝌\",
    \"latex\": \"\\\\mathbit{\\\\Chi}\",
    \"math\": null
  },
  \"120653\": {
    \"char\": \"𝝍\",
    \"latex\": \"\\\\mathbit{\\\\Psi}\",
    \"math\": null
  },
  \"120654\": {
    \"char\": \"𝝎\",
    \"latex\": \"\\\\mathbit{\\\\Omega}\",
    \"math\": null
  },
  \"120655\": {
    \"char\": \"𝝏\",
    \"latex\": \"\\\\partial \",
    \"math\": null
  },
  \"120656\": {
    \"char\": \"𝝐\",
    \"latex\": \"\\\\in\",
    \"math\": null
  },
  \"120657\": {
    \"char\": \"𝝑\",
    \"latex\": \"\\\\mathbit{\\\\vartheta}\",
    \"math\": null
  },
  \"120658\": {
    \"char\": \"𝝒\",
    \"latex\": \"\\\\mathbit{\\\\varkappa}\",
    \"math\": null
  },
  \"120659\": {
    \"char\": \"𝝓\",
    \"latex\": \"\\\\mathbit{\\\\phi}\",
    \"math\": null
  },
  \"120660\": {
    \"char\": \"𝝔\",
    \"latex\": \"\\\\mathbit{\\\\varrho}\",
    \"math\": null
  },
  \"120661\": {
    \"char\": \"𝝕\",
    \"latex\": \"\\\\mathbit{\\\\varpi}\",
    \"math\": null
  },
  \"120662\": {
    \"char\": \"𝝖\",
    \"latex\": \"\\\\mathsfbf{\\\\Alpha}\",
    \"math\": null
  },
  \"120663\": {
    \"char\": \"𝝗\",
    \"latex\": \"\\\\mathsfbf{\\\\Beta}\",
    \"math\": null
  },
  \"120664\": {
    \"char\": \"𝝘\",
    \"latex\": \"\\\\mathsfbf{\\\\Gamma}\",
    \"math\": null
  },
  \"120665\": {
    \"char\": \"𝝙\",
    \"latex\": \"\\\\mathsfbf{\\\\Delta}\",
    \"math\": null
  },
  \"120666\": {
    \"char\": \"𝝚\",
    \"latex\": \"\\\\mathsfbf{\\\\Epsilon}\",
    \"math\": null
  },
  \"120667\": {
    \"char\": \"𝝛\",
    \"latex\": \"\\\\mathsfbf{\\\\Zeta}\",
    \"math\": null
  },
  \"120668\": {
    \"char\": \"𝝜\",
    \"latex\": \"\\\\mathsfbf{\\\\Eta}\",
    \"math\": null
  },
  \"120669\": {
    \"char\": \"𝝝\",
    \"latex\": \"\\\\mathsfbf{\\\\Theta}\",
    \"math\": null
  },
  \"120670\": {
    \"char\": \"𝝞\",
    \"latex\": \"\\\\mathsfbf{\\\\Iota}\",
    \"math\": null
  },
  \"120671\": {
    \"char\": \"𝝟\",
    \"latex\": \"\\\\mathsfbf{\\\\Kappa}\",
    \"math\": null
  },
  \"120672\": {
    \"char\": \"𝝠\",
    \"latex\": \"\\\\mathsfbf{\\\\Lambda}\",
    \"math\": null
  },
  \"120673\": {
    \"char\": \"𝝡\",
    \"latex\": \"M\",
    \"math\": null
  },
  \"120674\": {
    \"char\": \"𝝢\",
    \"latex\": \"N\",
    \"math\": null
  },
  \"120675\": {
    \"char\": \"𝝣\",
    \"latex\": \"\\\\mathsfbf{\\\\Xi}\",
    \"math\": null
  },
  \"120676\": {
    \"char\": \"𝝤\",
    \"latex\": \"O\",
    \"math\": null
  },
  \"120677\": {
    \"char\": \"𝝥\",
    \"latex\": \"\\\\mathsfbf{\\\\Pi}\",
    \"math\": null
  },
  \"120678\": {
    \"char\": \"𝝦\",
    \"latex\": \"\\\\mathsfbf{\\\\Rho}\",
    \"math\": null
  },
  \"120679\": {
    \"char\": \"𝝧\",
    \"latex\": \"\\\\mathsfbf{\\\\vartheta}\",
    \"math\": null
  },
  \"120680\": {
    \"char\": \"𝝨\",
    \"latex\": \"\\\\mathsfbf{\\\\Sigma}\",
    \"math\": null
  },
  \"120681\": {
    \"char\": \"𝝩\",
    \"latex\": \"\\\\mathsfbf{\\\\Tau}\",
    \"math\": null
  },
  \"120682\": {
    \"char\": \"𝝪\",
    \"latex\": \"\\\\mathsfbf{\\\\Upsilon}\",
    \"math\": null
  },
  \"120683\": {
    \"char\": \"𝝫\",
    \"latex\": \"\\\\mathsfbf{\\\\Phi}\",
    \"math\": null
  },
  \"120684\": {
    \"char\": \"𝝬\",
    \"latex\": \"\\\\mathsfbf{\\\\Chi}\",
    \"math\": null
  },
  \"120685\": {
    \"char\": \"𝝭\",
    \"latex\": \"\\\\mathsfbf{\\\\Psi}\",
    \"math\": null
  },
  \"120686\": {
    \"char\": \"𝝮\",
    \"latex\": \"\\\\mathsfbf{\\\\Omega}\",
    \"math\": null
  },
  \"120687\": {
    \"char\": \"𝝯\",
    \"latex\": \"\\\\mathsfbf{\\\\nabla}\",
    \"math\": null
  },
  \"120688\": {
    \"char\": \"𝝰\",
    \"latex\": \"\\\\mathsfbf{\\\\Alpha}\",
    \"math\": null
  },
  \"120689\": {
    \"char\": \"𝝱\",
    \"latex\": \"\\\\mathsfbf{\\\\Beta}\",
    \"math\": null
  },
  \"120690\": {
    \"char\": \"𝝲\",
    \"latex\": \"\\\\mathsfbf{\\\\Gamma}\",
    \"math\": null
  },
  \"120691\": {
    \"char\": \"𝝳\",
    \"latex\": \"\\\\mathsfbf{\\\\Delta}\",
    \"math\": null
  },
  \"120692\": {
    \"char\": \"𝝴\",
    \"latex\": \"\\\\mathsfbf{\\\\Epsilon}\",
    \"math\": null
  },
  \"120693\": {
    \"char\": \"𝝵\",
    \"latex\": \"\\\\mathsfbf{\\\\Zeta}\",
    \"math\": null
  },
  \"120694\": {
    \"char\": \"𝝶\",
    \"latex\": \"\\\\mathsfbf{\\\\Eta}\",
    \"math\": null
  },
  \"120695\": {
    \"char\": \"𝝷\",
    \"latex\": \"\\\\mathsfbf{\\\\Theta}\",
    \"math\": null
  },
  \"120696\": {
    \"char\": \"𝝸\",
    \"latex\": \"\\\\mathsfbf{\\\\Iota}\",
    \"math\": null
  },
  \"120697\": {
    \"char\": \"𝝹\",
    \"latex\": \"\\\\mathsfbf{\\\\Kappa}\",
    \"math\": null
  },
  \"120698\": {
    \"char\": \"𝝺\",
    \"latex\": \"\\\\mathsfbf{\\\\Lambda}\",
    \"math\": null
  },
  \"120699\": {
    \"char\": \"𝝻\",
    \"latex\": \"M\",
    \"math\": null
  },
  \"120700\": {
    \"char\": \"𝝼\",
    \"latex\": \"N\",
    \"math\": null
  },
  \"120701\": {
    \"char\": \"𝝽\",
    \"latex\": \"\\\\mathsfbf{\\\\Xi}\",
    \"math\": null
  },
  \"120702\": {
    \"char\": \"𝝾\",
    \"latex\": \"O\",
    \"math\": null
  },
  \"120703\": {
    \"char\": \"𝝿\",
    \"latex\": \"\\\\mathsfbf{\\\\Pi}\",
    \"math\": null
  },
  \"120704\": {
    \"char\": \"𝞀\",
    \"latex\": \"\\\\mathsfbf{\\\\Rho}\",
    \"math\": null
  },
  \"120705\": {
    \"char\": \"𝞁\",
    \"latex\": \"\\\\mathsfbf{\\\\varsigma}\",
    \"math\": null
  },
  \"120706\": {
    \"char\": \"𝞂\",
    \"latex\": \"\\\\mathsfbf{\\\\Sigma}\",
    \"math\": null
  },
  \"120707\": {
    \"char\": \"𝞃\",
    \"latex\": \"\\\\mathsfbf{\\\\Tau}\",
    \"math\": null
  },
  \"120708\": {
    \"char\": \"𝞄\",
    \"latex\": \"\\\\mathsfbf{\\\\Upsilon}\",
    \"math\": null
  },
  \"120709\": {
    \"char\": \"𝞅\",
    \"latex\": \"\\\\mathsfbf{\\\\Phi}\",
    \"math\": null
  },
  \"120710\": {
    \"char\": \"𝞆\",
    \"latex\": \"\\\\mathsfbf{\\\\Chi}\",
    \"math\": null
  },
  \"120711\": {
    \"char\": \"𝞇\",
    \"latex\": \"\\\\mathsfbf{\\\\Psi}\",
    \"math\": null
  },
  \"120712\": {
    \"char\": \"𝞈\",
    \"latex\": \"\\\\mathsfbf{\\\\Omega}\",
    \"math\": null
  },
  \"120713\": {
    \"char\": \"𝞉\",
    \"latex\": \"\\\\partial \",
    \"math\": null
  },
  \"120714\": {
    \"char\": \"𝞊\",
    \"latex\": \"\\\\in\",
    \"math\": null
  },
  \"120715\": {
    \"char\": \"𝞋\",
    \"latex\": \"\\\\mathsfbf{\\\\vartheta}\",
    \"math\": null
  },
  \"120716\": {
    \"char\": \"𝞌\",
    \"latex\": \"\\\\mathsfbf{\\\\varkappa}\",
    \"math\": null
  },
  \"120717\": {
    \"char\": \"𝞍\",
    \"latex\": \"\\\\mathsfbf{\\\\phi}\",
    \"math\": null
  },
  \"120718\": {
    \"char\": \"𝞎\",
    \"latex\": \"\\\\mathsfbf{\\\\varrho}\",
    \"math\": null
  },
  \"120719\": {
    \"char\": \"𝞏\",
    \"latex\": \"\\\\mathsfbf{\\\\varpi}\",
    \"math\": null
  },
  \"120720\": {
    \"char\": \"𝞐\",
    \"latex\": \"\\\\mathsfbfsl{\\\\Alpha}\",
    \"math\": null
  },
  \"120721\": {
    \"char\": \"𝞑\",
    \"latex\": \"\\\\mathsfbfsl{\\\\Beta}\",
    \"math\": null
  },
  \"120722\": {
    \"char\": \"𝞒\",
    \"latex\": \"\\\\mathsfbfsl{\\\\Gamma}\",
    \"math\": null
  },
  \"120723\": {
    \"char\": \"𝞓\",
    \"latex\": \"\\\\mathsfbfsl{\\\\Delta}\",
    \"math\": null
  },
  \"120724\": {
    \"char\": \"𝞔\",
    \"latex\": \"\\\\mathsfbfsl{\\\\Epsilon}\",
    \"math\": null
  },
  \"120725\": {
    \"char\": \"𝞕\",
    \"latex\": \"\\\\mathsfbfsl{\\\\Zeta}\",
    \"math\": null
  },
  \"120726\": {
    \"char\": \"𝞖\",
    \"latex\": \"\\\\mathsfbfsl{\\\\Eta}\",
    \"math\": null
  },
  \"120727\": {
    \"char\": \"𝞗\",
    \"latex\": \"\\\\mathsfbfsl{\\\\vartheta}\",
    \"math\": null
  },
  \"120728\": {
    \"char\": \"𝞘\",
    \"latex\": \"\\\\mathsfbfsl{\\\\Iota}\",
    \"math\": null
  },
  \"120729\": {
    \"char\": \"𝞙\",
    \"latex\": \"\\\\mathsfbfsl{\\\\Kappa}\",
    \"math\": null
  },
  \"120730\": {
    \"char\": \"𝞚\",
    \"latex\": \"\\\\mathsfbfsl{\\\\Lambda}\",
    \"math\": null
  },
  \"120731\": {
    \"char\": \"𝞛\",
    \"latex\": \"M\",
    \"math\": null
  },
  \"120732\": {
    \"char\": \"𝞜\",
    \"latex\": \"N\",
    \"math\": null
  },
  \"120733\": {
    \"char\": \"𝞝\",
    \"latex\": \"\\\\mathsfbfsl{\\\\Xi}\",
    \"math\": null
  },
  \"120734\": {
    \"char\": \"𝞞\",
    \"latex\": \"O\",
    \"math\": null
  },
  \"120735\": {
    \"char\": \"𝞟\",
    \"latex\": \"\\\\mathsfbfsl{\\\\Pi}\",
    \"math\": null
  },
  \"120736\": {
    \"char\": \"𝞠\",
    \"latex\": \"\\\\mathsfbfsl{\\\\Rho}\",
    \"math\": null
  },
  \"120737\": {
    \"char\": \"𝞡\",
    \"latex\": \"\\\\mathsfbfsl{\\\\vartheta}\",
    \"math\": null
  },
  \"120738\": {
    \"char\": \"𝞢\",
    \"latex\": \"\\\\mathsfbfsl{\\\\Sigma}\",
    \"math\": null
  },
  \"120739\": {
    \"char\": \"𝞣\",
    \"latex\": \"\\\\mathsfbfsl{\\\\Tau}\",
    \"math\": null
  },
  \"120740\": {
    \"char\": \"𝞤\",
    \"latex\": \"\\\\mathsfbfsl{\\\\Upsilon}\",
    \"math\": null
  },
  \"120741\": {
    \"char\": \"𝞥\",
    \"latex\": \"\\\\mathsfbfsl{\\\\Phi}\",
    \"math\": null
  },
  \"120742\": {
    \"char\": \"𝞦\",
    \"latex\": \"\\\\mathsfbfsl{\\\\Chi}\",
    \"math\": null
  },
  \"120743\": {
    \"char\": \"𝞧\",
    \"latex\": \"\\\\mathsfbfsl{\\\\Psi}\",
    \"math\": null
  },
  \"120744\": {
    \"char\": \"𝞨\",
    \"latex\": \"\\\\mathsfbfsl{\\\\Omega}\",
    \"math\": null
  },
  \"120745\": {
    \"char\": \"𝞩\",
    \"latex\": \"\\\\mathsfbfsl{\\\\nabla}\",
    \"math\": null
  },
  \"120746\": {
    \"char\": \"𝞪\",
    \"latex\": \"\\\\mathsfbfsl{\\\\Alpha}\",
    \"math\": null
  },
  \"120747\": {
    \"char\": \"𝞫\",
    \"latex\": \"\\\\mathsfbfsl{\\\\Beta}\",
    \"math\": null
  },
  \"120748\": {
    \"char\": \"𝞬\",
    \"latex\": \"\\\\mathsfbfsl{\\\\Gamma}\",
    \"math\": null
  },
  \"120749\": {
    \"char\": \"𝞭\",
    \"latex\": \"\\\\mathsfbfsl{\\\\Delta}\",
    \"math\": null
  },
  \"120750\": {
    \"char\": \"𝞮\",
    \"latex\": \"\\\\mathsfbfsl{\\\\Epsilon}\",
    \"math\": null
  },
  \"120751\": {
    \"char\": \"𝞯\",
    \"latex\": \"\\\\mathsfbfsl{\\\\Zeta}\",
    \"math\": null
  },
  \"120752\": {
    \"char\": \"𝞰\",
    \"latex\": \"\\\\mathsfbfsl{\\\\Eta}\",
    \"math\": null
  },
  \"120753\": {
    \"char\": \"𝞱\",
    \"latex\": \"\\\\mathsfbfsl{\\\\vartheta}\",
    \"math\": null
  },
  \"120754\": {
    \"char\": \"𝞲\",
    \"latex\": \"\\\\mathsfbfsl{\\\\Iota}\",
    \"math\": null
  },
  \"120755\": {
    \"char\": \"𝞳\",
    \"latex\": \"\\\\mathsfbfsl{\\\\Kappa}\",
    \"math\": null
  },
  \"120756\": {
    \"char\": \"𝞴\",
    \"latex\": \"\\\\mathsfbfsl{\\\\Lambda}\",
    \"math\": null
  },
  \"120757\": {
    \"char\": \"𝞵\",
    \"latex\": \"M\",
    \"math\": null
  },
  \"120758\": {
    \"char\": \"𝞶\",
    \"latex\": \"N\",
    \"math\": null
  },
  \"120759\": {
    \"char\": \"𝞷\",
    \"latex\": \"\\\\mathsfbfsl{\\\\Xi}\",
    \"math\": null
  },
  \"120760\": {
    \"char\": \"𝞸\",
    \"latex\": \"O\",
    \"math\": null
  },
  \"120761\": {
    \"char\": \"𝞹\",
    \"latex\": \"\\\\mathsfbfsl{\\\\Pi}\",
    \"math\": null
  },
  \"120762\": {
    \"char\": \"𝞺\",
    \"latex\": \"\\\\mathsfbfsl{\\\\Rho}\",
    \"math\": null
  },
  \"120763\": {
    \"char\": \"𝞻\",
    \"latex\": \"\\\\mathsfbfsl{\\\\varsigma}\",
    \"math\": null
  },
  \"120764\": {
    \"char\": \"𝞼\",
    \"latex\": \"\\\\mathsfbfsl{\\\\Sigma}\",
    \"math\": null
  },
  \"120765\": {
    \"char\": \"𝞽\",
    \"latex\": \"\\\\mathsfbfsl{\\\\Tau}\",
    \"math\": null
  },
  \"120766\": {
    \"char\": \"𝞾\",
    \"latex\": \"\\\\mathsfbfsl{\\\\Upsilon}\",
    \"math\": null
  },
  \"120767\": {
    \"char\": \"𝞿\",
    \"latex\": \"\\\\mathsfbfsl{\\\\Phi}\",
    \"math\": null
  },
  \"120768\": {
    \"char\": \"𝟀\",
    \"latex\": \"\\\\mathsfbfsl{\\\\Chi}\",
    \"math\": null
  },
  \"120769\": {
    \"char\": \"𝟁\",
    \"latex\": \"\\\\mathsfbfsl{\\\\Psi}\",
    \"math\": null
  },
  \"120770\": {
    \"char\": \"𝟂\",
    \"latex\": \"\\\\mathsfbfsl{\\\\Omega}\",
    \"math\": null
  },
  \"120771\": {
    \"char\": \"𝟃\",
    \"latex\": \"\\\\partial \",
    \"math\": null
  },
  \"120772\": {
    \"char\": \"𝟄\",
    \"latex\": \"\\\\in\",
    \"math\": null
  },
  \"120773\": {
    \"char\": \"𝟅\",
    \"latex\": \"\\\\mathsfbfsl{\\\\vartheta}\",
    \"math\": null
  },
  \"120774\": {
    \"char\": \"𝟆\",
    \"latex\": \"\\\\mathsfbfsl{\\\\varkappa}\",
    \"math\": null
  },
  \"120775\": {
    \"char\": \"𝟇\",
    \"latex\": \"\\\\mathsfbfsl{\\\\phi}\",
    \"math\": null
  },
  \"120776\": {
    \"char\": \"𝟈\",
    \"latex\": \"\\\\mathsfbfsl{\\\\varrho}\",
    \"math\": null
  },
  \"120777\": {
    \"char\": \"𝟉\",
    \"latex\": \"\\\\mathsfbfsl{\\\\varpi}\",
    \"math\": null
  },
  \"120782\": {
    \"char\": \"𝟎\",
    \"latex\": \"\\\\mathbf{0}\",
    \"math\": null
  },
  \"120783\": {
    \"char\": \"𝟏\",
    \"latex\": \"\\\\mathbf{1}\",
    \"math\": null
  },
  \"120784\": {
    \"char\": \"𝟐\",
    \"latex\": \"\\\\mathbf{2}\",
    \"math\": null
  },
  \"120785\": {
    \"char\": \"𝟑\",
    \"latex\": \"\\\\mathbf{3}\",
    \"math\": null
  },
  \"120786\": {
    \"char\": \"𝟒\",
    \"latex\": \"\\\\mathbf{4}\",
    \"math\": null
  },
  \"120787\": {
    \"char\": \"𝟓\",
    \"latex\": \"\\\\mathbf{5}\",
    \"math\": null
  },
  \"120788\": {
    \"char\": \"𝟔\",
    \"latex\": \"\\\\mathbf{6}\",
    \"math\": null
  },
  \"120789\": {
    \"char\": \"𝟕\",
    \"latex\": \"\\\\mathbf{7}\",
    \"math\": null
  },
  \"120790\": {
    \"char\": \"𝟖\",
    \"latex\": \"\\\\mathbf{8}\",
    \"math\": null
  },
  \"120791\": {
    \"char\": \"𝟗\",
    \"latex\": \"\\\\mathbf{9}\",
    \"math\": null
  },
  \"120792\": {
    \"char\": \"𝟘\",
    \"latex\": \"\\\\mathbb{0}\",
    \"math\": null
  },
  \"120793\": {
    \"char\": \"𝟙\",
    \"latex\": \"\\\\mathbb{1}\",
    \"math\": null
  },
  \"120794\": {
    \"char\": \"𝟚\",
    \"latex\": \"\\\\mathbb{2}\",
    \"math\": null
  },
  \"120795\": {
    \"char\": \"𝟛\",
    \"latex\": \"\\\\mathbb{3}\",
    \"math\": null
  },
  \"120796\": {
    \"char\": \"𝟜\",
    \"latex\": \"\\\\mathbb{4}\",
    \"math\": null
  },
  \"120797\": {
    \"char\": \"𝟝\",
    \"latex\": \"\\\\mathbb{5}\",
    \"math\": null
  },
  \"120798\": {
    \"char\": \"𝟞\",
    \"latex\": \"\\\\mathbb{6}\",
    \"math\": null
  },
  \"120799\": {
    \"char\": \"𝟟\",
    \"latex\": \"\\\\mathbb{7}\",
    \"math\": null
  },
  \"120800\": {
    \"char\": \"𝟠\",
    \"latex\": \"\\\\mathbb{8}\",
    \"math\": null
  },
  \"120801\": {
    \"char\": \"𝟡\",
    \"latex\": \"\\\\mathbb{9}\",
    \"math\": null
  },
  \"120802\": {
    \"char\": \"𝟢\",
    \"latex\": \"\\\\mathsf{0}\",
    \"math\": null
  },
  \"120803\": {
    \"char\": \"𝟣\",
    \"latex\": \"\\\\mathsf{1}\",
    \"math\": null
  },
  \"120804\": {
    \"char\": \"𝟤\",
    \"latex\": \"\\\\mathsf{2}\",
    \"math\": null
  },
  \"120805\": {
    \"char\": \"𝟥\",
    \"latex\": \"\\\\mathsf{3}\",
    \"math\": null
  },
  \"120806\": {
    \"char\": \"𝟦\",
    \"latex\": \"\\\\mathsf{4}\",
    \"math\": null
  },
  \"120807\": {
    \"char\": \"𝟧\",
    \"latex\": \"\\\\mathsf{5}\",
    \"math\": null
  },
  \"120808\": {
    \"char\": \"𝟨\",
    \"latex\": \"\\\\mathsf{6}\",
    \"math\": null
  },
  \"120809\": {
    \"char\": \"𝟩\",
    \"latex\": \"\\\\mathsf{7}\",
    \"math\": null
  },
  \"120810\": {
    \"char\": \"𝟪\",
    \"latex\": \"\\\\mathsf{8}\",
    \"math\": null
  },
  \"120811\": {
    \"char\": \"𝟫\",
    \"latex\": \"\\\\mathsf{9}\",
    \"math\": null
  },
  \"120812\": {
    \"char\": \"𝟬\",
    \"latex\": \"\\\\mathsfbf{0}\",
    \"math\": null
  },
  \"120813\": {
    \"char\": \"𝟭\",
    \"latex\": \"\\\\mathsfbf{1}\",
    \"math\": null
  },
  \"120814\": {
    \"char\": \"𝟮\",
    \"latex\": \"\\\\mathsfbf{2}\",
    \"math\": null
  },
  \"120815\": {
    \"char\": \"𝟯\",
    \"latex\": \"\\\\mathsfbf{3}\",
    \"math\": null
  },
  \"120816\": {
    \"char\": \"𝟰\",
    \"latex\": \"\\\\mathsfbf{4}\",
    \"math\": null
  },
  \"120817\": {
    \"char\": \"𝟱\",
    \"latex\": \"\\\\mathsfbf{5}\",
    \"math\": null
  },
  \"120818\": {
    \"char\": \"𝟲\",
    \"latex\": \"\\\\mathsfbf{6}\",
    \"math\": null
  },
  \"120819\": {
    \"char\": \"𝟳\",
    \"latex\": \"\\\\mathsfbf{7}\",
    \"math\": null
  },
  \"120820\": {
    \"char\": \"𝟴\",
    \"latex\": \"\\\\mathsfbf{8}\",
    \"math\": null
  },
  \"120821\": {
    \"char\": \"𝟵\",
    \"latex\": \"\\\\mathsfbf{9}\",
    \"math\": null
  },
  \"120822\": {
    \"char\": \"𝟶\",
    \"latex\": \"\\\\mathtt{0}\",
    \"math\": null
  },
  \"120823\": {
    \"char\": \"𝟷\",
    \"latex\": \"\\\\mathtt{1}\",
    \"math\": null
  },
  \"120824\": {
    \"char\": \"𝟸\",
    \"latex\": \"\\\\mathtt{2}\",
    \"math\": null
  },
  \"120825\": {
    \"char\": \"𝟹\",
    \"latex\": \"\\\\mathtt{3}\",
    \"math\": null
  },
  \"120826\": {
    \"char\": \"𝟺\",
    \"latex\": \"\\\\mathtt{4}\",
    \"math\": null
  },
  \"120827\": {
    \"char\": \"𝟻\",
    \"latex\": \"\\\\mathtt{5}\",
    \"math\": null
  },
  \"120828\": {
    \"char\": \"𝟼\",
    \"latex\": \"\\\\mathtt{6}\",
    \"math\": null
  },
  \"120829\": {
    \"char\": \"𝟽\",
    \"latex\": \"\\\\mathtt{7}\",
    \"math\": null
  },
  \"120830\": {
    \"char\": \"𝟾\",
    \"latex\": \"\\\\mathtt{8}\",
    \"math\": null
  },
  \"120831\": {
    \"char\": \"𝟿\",
    \"latex\": \"\\\\mathtt{9}\",
    \"math\": null
  }
}"
                           )
    "Char table constructed from w3.org."))

(eval-and-compile
  (defvar ref-man-bibtex-ascii-replacement-strings
    '(("í" . "{\\\\'i}")
      ("î" . "\\\\^{\\\\i}")
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
      ("ń" . "{\\\\'n}")
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
      ("α" . "$\\\\alpha$")
      ("β" . "$\\\\beta$")
      ("γ" . "$\\\\gamma$")
      ("ɣ" . "$\\\\gamma$")
      ("δ" . "$\\\\delta$")
      ("η" . "$\\\\eta$")
      ("µ" . "$\\\\mu$")
      ("ɛ" . "$\\\\epsilon$")
      ("λ" . "$\\\\lambda$")
      ("π" . "$\\\\pi$")
      ("∞" . "$\\\\infty$")
      ("χ" . "$\\\\chi$")
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
      ("º" . "degc")
      ("ⅵ" . "\textrm{vi}")
      ("ⅲ" . "\textrm{iii}")
      ("ⅴ" . "\textrm{v}")
      ("Ⅵ" . "\textrm{VI}")
      ("Ⅲ" . "\textrm{III}")
      ("Ⅴ" . "\textrm{V}")
      ("∼" . "\\\\textasciitilde{}")
      ("‑" . "\\\\textemdash{}")
      ("•" . "\\\\textbullet ")
      ("‒" . "\\\\textemdash{}"))
    "Replace non-ascii characters with escaped ones for latex rendering.
The characters here directly borrowed from `org-ref'.
See `org-ref-nonascii-latex-replacements'."))

(eval-and-compile
  (defvar ref-man-bibtex-extended-repacement-strings
    (remove '("}" . "\\\\rbrace ")
            (-concat
             ref-man-bibtex-ascii-replacement-strings
             (mapcar (lambda (x) `(,(a-get (cdr x) 'char) . ,(replace-regexp-in-string "\\\\" (regexp-quote "\\\\")
                                                                                       (a-get (cdr x) 'latex))))
                     (-slice ref-man-char-table  92 400))))
    "Expanded non-ascii character table for latex rendering."))

(defvar ref-man-bibtex-non-invertible-ascii-replacements
  '((" " . " ")
    ("…" . "...")
    (" " . " ")
    (" " . " ")
    (" " . " ")
    ("–" . "-")
    ("−" . "-")
    ("‘" . "'")
    ("’" . "'")
    ("”" . "\"")))

(defvar ref-man-file-fuzzy-link-re
  (rx "[" "[" (group (seq (regexp "file.+?::") "*" (+? nonl))) "]"
      "[" (group (+? any)) "]" "]")
  "Org fuzzy link with file prefix.")

(defvar ref-man-file-custid-link-re
  (rx "[" "[" (group (regexp "file.+?::#[a-zA-Z0-9_-]+?")) "]"
      "[" (group (+? any)) "]" "]")
  "Org custom-id link with file prefix.")

(defvar ref-man-file-fuzzy-custid-link-re
  (rx "[" "[" (group (seq (regexp "file.+?::") (or "*" "#") (+? nonl))) "]"
      "[" (group (+? any)) "]" "]")
  "Org custom-id or fuzzy link with file prefix.")



(defvar ref-man-maybe-file-fuzzy-custid-link-re
  ;; "\\[\\[\\([#*].+?\\)]\\[\\(.+?\\)]]\\|\\[\\[\\(?:file:\\)?\\(.+?\\)::\\([#*].+?\\)]\\[\\(.+?\\)]]"
  ;; (rx "[" "[" (group (seq (opt (opt "file:") (regexp ".+?::"))) (or "*" "#") (regexp ".+?")) "]" "[" (group (+? any)) "]" "]")
  ;; (rx (seq "[[" (opt "file:") (group (*? nonl)) (opt "::") (group (any "#*") (+? nonl)) "][" (group (+? nonl)) "]]"))
  ;; NOTE: The final correct regexp
  (rx (seq "[[" (group (opt "file:") (*? nonl) (opt "::") (any "#*") (+? nonl)) "][" (group (+? nonl)) "]]"))
  "Org custom-id or fuzzy link with optional file prefix.")

(defvar ref-man-maybe-local-fuzzy-custid-link-re
  (rx "[" "[" (group (opt (seq (opt "file:") (regexp ".+?::")))) (group (seq (or "*" "#") (+? nonl))) "]"
      "[" (group (+? any)) "]" "]")
  "Org custom-id or fuzzy file or local link.")

(defun ref-man-pairs-to-alist (pairs)
  "Merge cons PAIRS into an alist with first elements as keys.

The head of the list is the associative element.

Example:
    (pairs-to-alist \\='((a b) (b c d) (a d) (e . f)))
     => \\='((a b d) (b c d) (e f))"
  (when (and (consp pairs) (a-assoc pairs))
    (let (newlist)
      (seq-do (lambda (x)
                (if (a-has-key newlist (car x))
                    (if (consp (cdr x))
                        (setf (alist-get (car x) newlist) (-concat (alist-get (car x) newlist) (cdr x)))
                      (push (cdr x) (alist-get (car x) newlist)))
                  (push x newlist)))
              pairs)
      newlist)))

(defun url-join (&rest elements)
  "Join ELEMENTS with a single \"/\", like a url."
  (declare (pure t) (side-effect-free t))
  (string-join (-remove #'string-empty-p
                        (mapcar (lambda (x)
                                  (string-remove-prefix "/" (string-remove-suffix "/" x)))
                                elements))
               "/"))

(defun path-join (&rest elements)
  "Join ELEMENTS as a path, expects full paths."
  (declare (pure t) (side-effect-free t))
  (concat "/" (mapconcat (lambda (x)
                           (string-remove-prefix "/" (string-remove-suffix "/" x)))
                         elements "/")))

(defun dir-equal-p (dir-a dir-b)
  "Return non-nil if full paths for DIR-A and DIR-B are equal.
They need not exist."
  (declare (pure t) (side-effect-free t))
  (string= (string-remove-suffix "/" (expand-file-name dir-a))
           (string-remove-suffix "/" (expand-file-name dir-b))))

;; TODO: Document this
(defun max-ind (seq)
  (declare (pure t) (side-effect-free t))
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
;; (make-obsolete 'replace-in-string 'replace-regexp-in-string "ref-man 0.3.0")

(defun sort-symbols (reverse beg end)
  "Sort symbols in region alphabetically, in REVERSE if negative.
BEG and END are region markers.  See `sort-words'."
  (interactive "*P\nr")
  (sort-regexp-fields reverse "\\(\\sw\\|\\s_\\)+" "\\&" beg end))

(defun maybe-delete-link (out-dir file)
  "Maybe delete symlink for FILE (or filename) if it exists in OUT-DIR."
  (let ((link (f-join out-dir (f-filename file))))
    (when (f-symlink? link) (f-delete link))))

(defun ref-man-at-link-p ()
  (let ((link (get-text-property (point) 'htmlize-link)))
    (list link  (looking-at ref-man-maybe-local-fuzzy-custid-link-re)
          (looking-back ref-man-maybe-local-fuzzy-custid-link-re (line-beginning-position) t))))

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

(defun ref-man--trim-and-unquote (str)
  "Trim the input string STR and remove surrounding quotes if present.
Identical to `ref-man--trim-whitespace' but remove quotes also."
  (let ((str (replace-regexp-in-string "[ \t]+" " "
                                       (replace-regexp-in-string "\n" "" (string-trim str)))))
    (replace-regexp-in-string "\\(.*?\\)\"\\(.+?\\)\"\\(.*\\)" "\\1\\2\\3" str)))

(defun ref-man--fix-curly (str)
  "Gets text between parentheses for {STR}."
  (declare (pure t) (side-effect-free t))
  (replace-regexp-in-string "\\`{\\(.+\\)}\\'" "\\1" str))

(defun ref-man--bibtex-key-p (item)
  "ITEM is a bibtex key."
  (declare (pure t) (side-effect-free t))
  (string= (car item) "=key="))

(defun ref-man--stop-word-p (x)
  "X is a stop word."
  (declare (pure t) (side-effect-free t))
  (member x ref-man-stop-words))

(defun ref-man--transcribe (str change-list &optional inverse ignore)
  "Transcribe non-ascii characters in STR to ASCII lookalikes.
Argument CHANGE-LIST is an alist of regexps, `(A . B)' changes
from A to B.

With optional non-nil INVERSE, do the opposite and change B to A."
  (let* ((content str)
         (change-list (or change-list bibtex-autokey-transcriptions))
         (keys (if inverse (a-vals change-list) (a-keys change-list)))
         (keys (if ignore (-difference keys ignore) keys))
         (vals (if inverse (a-keys change-list) (a-vals change-list)))
         (alist (-zip keys vals))
         (case-fold-search nil)
         (match-re (string-join keys "\\|")))
    (when (string-match-p match-re str)
      (pcase-dolist (`(,a . ,b) alist)
        (setq content (replace-regexp-in-string a b content t))))
    content))

(defun ref-man-util-regions-contiguous-p (regions)
  "Return t if list of REGIONS are contiguous."
  (declare (pure t) (side-effect-free t))
  (let ((flag t)
        temp)
    (seq-do (lambda (x)
              (if (eq (car x) 'end)
                  (push (cdr x) temp)
                (when (and temp (not (= (- (cdr x) (car temp)) 1)))
                  (setq flag nil))))
            regions)
    flag))

(defun ref-man-delete-blank-lines-in-region (&optional beg end no-trailing-newline)
  "Delete all empty lines in region.
Region is either the active region or optional points BEG and
END.

This function is aliased from URL `https://github.com/akshaybadola/emacs-util'."
  (interactive)
  (when current-prefix-arg
    (setq no-trailing-newline t))
  (save-restriction
    (when (and (called-interactively-p 'any) (region-active-p))
      (setq beg (region-beginning)
            end (region-end)))
    (when (and beg end (< beg end))
      (narrow-to-region beg end)
      (delete-trailing-whitespace)
      (goto-char (point-min))
      (while (re-search-forward "^[ ]+\n" nil t)
        (replace-match ""))
      (goto-char (point-min))
      (when (looking-at "\n")
        (delete-char 1))
      (while (re-search-forward "\r?\n+\n" nil t)
        (replace-match "\n"))
      (goto-char (point-max))
      (when (and no-trailing-newline (looking-back "\n" 1))
        (re-search-backward "\r?\n+\n" nil t)
        (replace-match "")))))

(defun ref-man-delete-blank-lines-in-buffer (&optional buf no-trailing-newline)
  "Delete all empty lines in the entire buffer BUF.
When optional BUF is not given, defaults to current buffer.

This function is aliased from URL `https://github.com/akshaybadola/emacs-util'."
  (interactive)
  (unless buf
    (setq buf (current-buffer)))
  (when current-prefix-arg
    (setq no-trailing-newline t))
  (with-current-buffer buf
    (ref-man-delete-blank-lines-in-region (point-min) (point-max) no-trailing-newline)))

;; (defun test/ref-man-util-pipe-through
;;     (let ((arg '(("venue" . "something")
;;                  ("journal" . "ArXiv"))))
;;       (ref-man-util-pipe-through ref-man-bibtex-clean-pipe arg)))

;; TODO: A better pipe macro based `->>' can be made I think
(defun ref-man-util-pipe-through (pipe args)
  "Pipe the ARGS through a named PIPE.
The PIPE must be a sequence of functions and they are called in
order on ARGS.

The result of the call from each function is passed down to the
next one and the final result is returned."
  (seq-do (lambda (x)
            (setq args (funcall x args)))
          pipe)
  args)

(defun ref-man--replace-non-ascii (str &optional inverse)
  "Replace non-ascii characters in STR with escape codes.

Uses `ref-man-bibtex-ascii-replacement-strings' for replacements.
If `org-ref-nonascii-latex-replacements' exists, then the
replacements are a union of both above alists.

With non-nil optional INVERSE, perform the inverse replacement
from the alist."
  (ref-man--transcribe str (or (and (boundp 'org-ref-nonascii-latex-replacements)
                                    (-union (-concat ref-man-bibtex-extended-repacement-strings
                                                     ref-man-bibtex-non-invertible-ascii-replacements)
                                            org-ref-nonascii-latex-replacements))
                               ref-man-bibtex-extended-repacement-strings)
                       inverse))

(defun ref-man-not-pdf-files (&optional dir)
  "Return list of files which are not pdf files in DIR.
If DIR is not given it defaults to `ref-man-documents-dir'."
  (-filter (lambda (x) (let ((case-fold-search t)
                             (pdf-str (shell-command-to-string (format "file '%s'" x))))
                         (not (string-match-p "pdf document" pdf-str))))
           (f-files (or dir ref-man-documents-dir))))

(defun ref-man--invert-accents (str)
  "Replace escaped ascii characters in STR with non-ascii characters.

Performs inverse of `ref-man--replace-non-ascii'."
  (ref-man--transcribe str ref-man-bibtex-extended-repacement-strings t ref-man-alphabet))

(defun ref-man-replace-multiple-spaces-with-a-single-space ()
  "Replace multiple spaces with a single space unless indentation."
  (goto-char (point-min))
  (while (re-search-forward "[ \t]\\{2,\\}" nil t)
    (unless (save-match-data (looking-back "^[ \t]+" 1))
      (replace-match " "))))

(defun ref-man-save-headings-before-pdf-file-open (arg)
  "Add advice to save headings and paths before calling `org-open-at-point'.
Used to insert ORG_FILE back into the file in case an org buffer
is saved in `ref-man-org-store-dir' from `ref-man-get-references'."
  (if (or (and (integerp arg) (< 0 arg)) arg)
      (advice-add #'org-open-at-point :before #'ref-man-save-heading-before-open-advice)
    (advice-remove #'org-open-at-point #'ref-man-save-heading-before-open-advice)))

(defvar ref-man-headings-before-pdf-open nil
  "Saved org headings and paths when a pdf file was opened from an org buffer.")
(defun ref-man-save-heading-before-open-advice (&optional _arg)
  "Save org heading and file path if a pdf file is opened from PDF_FILE property.
This function is used as advice to `org-open-at-point' and
optional ARG is passed through to it.  Org heading and path for
the link are added to `ref-man-headings-before-pdf-open'."
  (let* ((heading (substring-no-properties (org-get-heading t t t t)))
         (context (org-element-context))
         (maybe-link (cond ((and (eq (car context) 'headline)
                                 (plist-get (cadr context) :PDF_FILE))
                            (plist-get (cadr context) :PDF_FILE))
                           ((eq (car context) 'node-property)
                            (plist-get (cadr context) :value))
                           ;; NOTE: we don't save if link is in text (for now)
                           ;; ((eq (car context) 'link)
                           ;;  context)
                           (t nil)))
         (path (cond ((and maybe-link (stringp maybe-link))
                      (pcase (with-temp-buffer
	                       (let ((org-inhibit-startup nil))
	                         (insert maybe-link)
	                         (org-mode)
	                         (goto-char (point-min))
	                         (org-element-link-parser)))
                        (`nil (user-error "No valid link in %S" heading))
                        (link (and link (plist-get (cadr link) :path)))))
                     ((and (eq (car maybe-link) 'link)
                           (equal (plist-get (cadr maybe-link) :type) "file"))
                      (plist-get (cadr maybe-link) :path))
                     (t nil))))
    (when path
      (add-to-list 'ref-man-headings-before-pdf-open (list :heading heading :path path)))))

(defun ref-man--post-json-synchronous (url data &optional silent)
  "Send an HTTP POST request to URL with DATA.

DATA should be an alist of key-value pairs.  The request is sent
content-type as application/json and DATA is encoded as json.

Argument SILENT is passed on to `url-retrieve-synchronously'."
  (let ((url-request-extra-headers
         `(("Content-Type" . "application/json")))
        (url-request-method "POST")
        (url-request-data
         (encode-coding-string (json-encode-alist data) 'utf-8)))
    (url-retrieve-synchronously url silent)))

(provide 'ref-man-util)

;;; ref-man-util.el ends here
