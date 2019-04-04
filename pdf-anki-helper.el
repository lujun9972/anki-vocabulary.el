;;; pdf-anki-helper.el --- a plugin that help you collect vocabulary for anki when reading pdf

;; Copyright (C) 2019-2019 DarkSun <lujun9972@gmail.com>.

;; Author: DarkSun <lujun9972@gmail.com>
;; Keywords: lisp, anki, pdf, translator

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Source code
;;
;; pdf-anki-helper's code can be found here:
;;   http://github.com/lujun9972/pdf-anki-helper.el

;;; Commentary:

;; pdf-anki-helper is a plugin that help you collect vocabulary for anki when reading pdf
;; Just select the sentence then execute =M-x pah-search=


;;; Code:

(require 'pdf-view)
(require 'youdao-dictionary)
;; (require 'AnkiConnect)
;; (defcustom pah-deck-name 'string
;;   "Which deck would the word stored")
;; (defcustom pah-model-name 'string
;;   "Specify the model name")
;; (defcustom pah-field-alist 'list
;;   "指定field的对应关系")
;; (defun pah-set-ankiconnect ()
;;   ""
;;   (interactive)
;;   (let ((deck-names (AnkiConnect-DeckNames))
;;         (model-names (AnkiConnect-ModelNames)))
;;     (setq pah-deck-name (completing-read "Select the Deck Name:" deck-names))
;;     (setq pah-model-name (completing-read "Select the Model Name:" model-names))
;;     (setq pah-field-alist nil)
;;     (let* ((skip-field " ")
;;            (fields (cons skip-field (AnkiConnect-ModelFieldNames pah-model-name))))
;;       (dolist (element '(expression reading glossary sentence translation))
;;         (let* ((prompt (format "%s" element))
;;                (field (completing-read prompt fields)))
;;           (unless (string= field skip-field)
;;             (setq fields (remove field fields))
;;             (add-to-list 'pah-field-alist (cons field element))))))))

(defgroup pdf-anki-helper nil
  ""
  :prefix "pah"
  :group 'tools
  :link '(url-link :tag "Github" "https://github.com/lujun9972/pdf-anki-helper.el"))

(defvar pah-store-file "~/pdf-anki-helper.txt")

(defun pah--get-active-region-text ()
  "Get the region text."
  (pdf-view-assert-active-region)
  (let* ((txt (pdf-view-active-region-text))
         (txt (string-join txt "\n")))
    (replace-regexp-in-string "[\r\n]" " " txt)))

(defun pah--select-word-in-string (str)
  "Select word in `STR'."
  (let ((words (split-string str "[ \f\t\n\r\v,.:]+")))
    (completing-read "请选择单词: " words)))

(defun pah-search ()
  (interactive)
  (let* ((sentence (pah--get-active-region-text))           ; 原句
         (expression (pah--select-word-in-string sentence)) ; 拼写
         translation                                        ; 翻译
         glossary                                           ; 释义
         us-phonetic                                        ; 发音
         )
    (let* ((json (youdao-dictionary--request sentence)))
      (setq translation (aref (assoc-default 'translation json) 0)))
    (let* ((json (youdao-dictionary--request expression))
           (explains (youdao-dictionary--explains json))
           (basic (cdr (assoc 'basic json)))
           (prompt (format "%s(%s):" translation expression)))
      (setq glossary (completing-read prompt (mapcar #'identity explains)))
      (setq us-phonetic (cdr (assoc 'us-phonetic basic))))
    (with-temp-file pah-store-file
      (when (file-readable-p pah-store-file)
        (insert-file-contents pah-store-file))
      (insert (string-join `(,sentence ,expression ,translation ,glossary ,us-phonetic) "|") "\n"))))

(provide 'pdf-anki-helper)
