;;; anki-vocabulary.el --- Help you to create vocabulary card in Anki

;; Copyright (C) 2019-2019 DarkSun <lujun9972@gmail.com>.

;; Author: DarkSun <lujun9972@gmail.com>
;; Keywords: lisp, anki, translator, chinese
;; Package: anki-vocabulary
;; Version: 1.0
;; Package-Requires: ((emacs "24.4") (s "1.0") (youdao-dictionary "0.4") (AnkiConnect "1.0"))
;; URL: http://github.com/lujun9972/anki-vocabulary.el

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
;; anki-vocabulary's code can be found here:
;;   http://github.com/lujun9972/anki-vocabulary.el

;;; Commentary:

;; anki-vocabulary is a plugin that help you to create vocabulary card in Anki
;; Just select the sentence then execute =M-x anki-vocabulary=


;;; Code:

(require 'pdf-view)
(require 'youdao-dictionary)
(require 'AnkiConnect)

(defgroup anki-vocabulary nil
  ""
  :prefix "anki-vocabulary"
  :group 'tools
  :link '(url-link :tag "Github" "https://github.com/lujun9972/pdf-anki-vocabulary.el"))

(defcustom anki-vocabulary-deck-name ""
  "Which deck would the word stored."
  :type 'string)

(defcustom anki-vocabulary-model-name ""
  "Specify the model name."
  :type 'string)

(defcustom anki-vocabulary-field-alist nil
  "指定field的对应关系."
  :type 'string)

(defcustom anki-vocabulary-audio-fileds nil
  "Specify fields used to store audio."
  :type 'list)

(defcustom anki-vocabulary-before-addnote-functions nil
  "List of hook functions run before add note.

The functions should accept those arguments:
+ expression(单词)
+ sentence(单词所在句子)
+ sentence_bold(单词所在句子,单词加粗)
+ translation(翻译的句子)
+ glossary(单词释义)
+ us-phonetic(美式发音)
+ uk-phonetic(英式发音)"
  :type 'hook)

(defcustom anki-vocabulary-after-addnote-functions nil
  "List of hook functions run after add note.

The functions should accept those arguments:
+ expression(单词)
+ sentence(单词所在句子)
+ sentence_bold(单词所在句子,单词加粗)
+ translation(翻译的句子)
+ glossary(单词释义)
+ us-phonetic(美式发音)
+ uk-phonetic(英式发音)"
  :type 'hook)

;;;###autoload
(defun anki-vocabulary-set-ankiconnect ()
  "Set the correspondence relation of anki card."
  (interactive)
  (let ((deck-names (AnkiConnect-DeckNames))
        (model-names (AnkiConnect-ModelNames)))
    (setq anki-vocabulary-deck-name (completing-read "Select the Deck Name:" deck-names))
    (setq anki-vocabulary-model-name (completing-read "Select the Model Name:" model-names))
    (setq anki-vocabulary-field-alist nil)
    (setq anki-vocabulary-audio-fileds nil)
    (let* ((fields (AnkiConnect-ModelFieldNames anki-vocabulary-model-name))
           (elements '("${单词}" "${释义}" "${美式音标}" "${英式音标}" "${原文例句}" "${标粗的原文例句}" "${翻译例句}" "${发声}" "SKIP")))
      (dolist (field fields)
        (let* ((prompt (format "%s" field))
               (element (completing-read prompt elements)))
          (unless (string= element "SKIP")
            (if (equal "${发声}" element)
                (pushnew field anki-vocabulary-audio-fileds)
              (setq elements (remove element elements))
              (add-to-list 'anki-vocabulary-field-alist (cons field element)))))))))

(defun anki-vocabulary--get-pdf-text ()
  "Get the text in pdf mode."
  (pdf-view-assert-active-region)
  (let* ((txt (pdf-view-active-region-text))
         (txt (string-join txt "\n")))
    (replace-regexp-in-string "[\r\n]+" " " txt)))

(defun anki-vocabulary--get-normal-text ()
  "Get the text in normal mode."
  (let ((txt (if (region-active-p)
                 (buffer-substring-no-properties (region-beginning) (region-end))
               (or (sentence-at-point)
                   (thing-at-point 'line)))))
    (replace-regexp-in-string "[\r\n]+" " " txt)))

(defun anki-vocabulary--get-text ()
  "Get the region text."
  (if (derived-mode-p 'pdf-view-mode)
      (anki-vocabulary--get-pdf-text)
    (anki-vocabulary--get-normal-text)))

(defun anki-vocabulary--select-word-in-string (str &optional default-word)
  "Select word in `STR'.
Optional argument DEFAULT-WORD specify the defauld word."
  (let ((words (split-string str "[ \f\t\n\r\v,.:?;\"<>]+")))
    (completing-read "请选择单词: " words nil nil default-word)))

(defun anki-vocabulary--get-word ()
  "Get the word at point."
  (unless (derived-mode-p 'pdf-view-mode)
    (word-at-point)))

(defun anki-vocabulary--format-meanings (explain)
  "Format the explain to meanings list."
  (let* ((tag (car (split-string explain "\\. ")))
         (meanings (cadr (split-string explain "\\. "))))
    (if meanings                    ;有些explain中不带词性说明，这时就不会有". "分隔符了
        (setq tag (concat tag ". "))
      (setq meanings tag)
      (setq tag ""))
    (setq meanings (split-string meanings "；"))
    (mapcar (lambda (meaning)
              (format "%s%s" tag meaning))
            meanings)))

;;;###autoload
(defun anki-vocabulary (&optional sentence word)
  "Translate SENTENCE and WORD, and then create an anki card."
  (interactive)
  (let* ((sentence (or sentence (anki-vocabulary--get-text))) ; 原句
         (word (or word  (anki-vocabulary--select-word-in-string sentence (anki-vocabulary--get-word))))
         (sentence_bold (replace-regexp-in-string (concat "\\b" (regexp-quote word) "\\b")
                                                  (lambda (word)
                                                    (format "<b>%s</b>" word))
                                                  sentence)) ; 粗体标记的句子
         (json (youdao-dictionary--request sentence))
         (translation (aref (assoc-default 'translation json) 0)) ; 翻译
         (json (youdao-dictionary--request word))
         (explains (youdao-dictionary--explains json))
         (explains (mapcan #'anki-vocabulary--format-meanings explains))
         (web (assoc-default 'web json)) ;array
         (web-explains (mapcar
                        (lambda (k-v)
                          (format "- %s :: %s"
                                  (assoc-default 'key k-v)
                                  (mapconcat 'identity (assoc-default 'value k-v) "; ")))
                        web))
         (basic (or (cdr (assoc 'basic json))
                    ""))
         (expression (cdr (assoc 'query json))) ; 单词
         (prompt (format "%s(%s):" translation expression))
         (glossary (completing-read prompt (or explains
                                               web-explains))) ; 释义
         (us-phonetic (or (cdr (assoc 'us-phonetic basic))
                          ""))          ; 美式音标
         (uk-phonetic (or (cdr (assoc 'uk-phonetic basic))
                          ""))          ; 英式音标
         (audio-url (youdao-dictionary--format-voice-url word))
         (audio-filename (format "youdao-%s.mp3" (md5 audio-url))) ;发声
         (data `((单词 . ,expression)
                 (释义 . ,glossary)
                 (美式音标 . ,us-phonetic)
                 (英式音标 . ,uk-phonetic)
                 (原文例句 . ,sentence)
                 (标粗的原文例句 . ,sentence_bold)
                 (翻译例句 . ,translation)
                 (发声 . ,(format "[sound:%s]" audio-filename))))
         (fileds (mapcar #'car anki-vocabulary-field-alist))
         (elements (mapcar #'cdr anki-vocabulary-field-alist))
         (values (mapcar (lambda (e)
                           (s-format e 'aget data))
                         elements))
         (fields (cl-mapcar #'cons fileds values)))
    (run-hook-with-args 'anki-vocabulary-before-addnote-functions expression sentence sentence_bold translation glossary us-phonetic uk-phonetic)
    (if anki-vocabulary-audio-fileds
        (let* ((audio-fileds (cond ((listp anki-vocabulary-audio-fileds)
                                    (apply #'vector anki-vocabulary-audio-fileds))
                                   ((stringp anki-vocabulary-audio-fileds)
                                    (vector anki-vocabulary-audio-fileds))
                                   (t [])))
               (audio `(("url" . ,audio-url)
                        ("filename" . ,audio-filename)
                        ("fields" . ,audio-fileds))))
          (AnkiConnect-AddNote anki-vocabulary-deck-name anki-vocabulary-model-name fields audio))
      (AnkiConnect-AddNote anki-vocabulary-deck-name anki-vocabulary-model-name fields))
    (run-hook-with-args 'anki-vocabulary-after-addnote-functions expression sentence sentence_bold translation glossary us-phonetic uk-phonetic)))

(provide 'anki-vocabulary)

;;; anki-vocabulary.el ends here
