;;; liquid-tip.el --- show inferred liquid-types

;; Copyright (C) 2014 by Ranjit Jhala

;; Author: Ranjit Jhala <jhala@cs.ucsd.edu>
;; Version: 0.0.2
;; Package-Requires: ((flycheck "0.13") (dash "1.2") (emacs "24.1") (popup "0.5.2") (pos-tip "0.5.0") (flycheck-liquidhs "0.0.1"))
;;; License:
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;; see https://github.com/ucsd-progsys/liquidhaskell#emacs

;;; Code:


(eval-when-compile (require 'cl))
(require 'json)
(require 'popup)
(require 'pos-tip nil t)
(require 'thingatpt)
(require 'button-lock)
(require 'flycheck-liquidhs)

;; ------------------------------------------------------------------------
;; Defcustoms
;; ------------------------------------------------------------------------

(defgroup liquid-tip-popup nil
  " LiquidHaskell type popup tooltip."
  :group 'haskell
  :prefix "liquid-tip/")

(defcustom liquid-tip/style 'ascii
  "Set popup style."
  :type '(choice (const :tag "Plain text" ascii)
                 (const :tag "Balloon" balloon))
  :group 'liquid-tip)

(defcustom liquid-tip/checker-name 'flycheck
  "Checker used by liquid, either 'flycheck or nil.
Prefix for checked files in .liquid"
  :type '(choice (const :tag "Flycheck" flycheck)
                 (const :tag "nil" nil))
  :group 'liquid-tip)

(defcustom liquid-tip/trigger 'S-double-mouse-1
  "Set trigger event for (liquid-tip-show).  Must be a valid Mouse button symbol."
  ;; can either use symbol, choice, radio
  :type '(choice (const :tag "Double click" double-mouse-1)
                 (const :tag "Shift-Double click" S-double-mouse-1)
                 symbol (sexp :tag "Other"))
  :group 'liquid-tip)

;; ------------------------------------------------------------------------
;; A structure to represent positions
;; ------------------------------------------------------------------------

(cl-defstruct position file row col)

;; ------------------------------------------------------------------------
;; Utilities for reading json/files
;; ------------------------------------------------------------------------

(defun liquid-tip/get-string-from-file (filePath)
  "Return FILEPATH's file content."
  (with-temp-buffer
    (insert-file-contents filePath)
    (buffer-string)))

(defun  liquid-tip/get-json-from-file (filePath)
  "Return json object from FILEPATH content."
  (if (file-exists-p filePath)
      (let* ((json-key-type 'string)
             (str ( liquid-tip/get-string-from-file filePath)))
        (json-read-from-string str))
    nil))

;; ------------------------------------------------------------------------
;; get/set annot information
;; ------------------------------------------------------------------------

(defun  liquid-tip/gethash-nil (key table)
  "Return the value of KEY from TABLE."
  (if table
      (gethash key table nil)
    nil))

(defun  liquid-tip/annot-filepath-prefix (mode)
  "Return prefix of annotation file using MODE."
  (if (equal mode 'flycheck)
      "flycheck_"
    nil))

;; (liquid-annot 'flycheck "/path/to/file.hs")
;;    ==> "/path/to/.liquid/flycheck_file.hs.json"
;;
;; (liquid-annot nil       "/path/to/file.hs")
;;    ==> "/path/to/.liquid/file.hs.json"

(defun  liquid-tip/annot-filepath (mode file)
  "Return MODE dependent name of annotation FILE."
  (let* ((dir    (file-name-directory file))
         (name   (file-name-nondirectory file))
         (prefix (liquid-tip/annot-filepath-prefix mode)))
    (concat dir ".liquid/" prefix name ".json")))

(defvar liquid-tip/annot-table (make-hash-table :test 'equal))

;; API
(defun liquid-tip/annot-set (file mode)
  "Load information for FILE (in current MODE) into liquid-annot-table."
  (let* ((file-path        (liquid-tip/annot-filepath mode file))
         (json-object-type 'hash-table)
         (file-info        (liquid-tip/get-json-from-file file-path)))
    (if file-info (puthash file file-info liquid-tip/annot-table))))

;; API
(defun liquid-tip/annot-get (file row col)
  "Get annotation for identifier in FILE at ROW and COL."
  (let* ((table (liquid-tip/gethash-nil file liquid-tip/annot-table))
         (r     (format "%d" row))
         (c     (format "%d" col))
         (err   (liquid-tip/gethash-nil "errors" table))
         (tys   (liquid-tip/gethash-nil "types" table))
         (ro    (liquid-tip/gethash-nil r tys)))
    (if tys
        (liquid-tip/gethash-nil "ann" (liquid-tip/gethash-nil c ro))
      (liquid-tip/gethash-nil "message" (elt err 0)))))

;; ------------------------------------------------------------------------
;; Display Annot in Tooltip
;; ------------------------------------------------------------------------

;; For simple, ascii popups, use:
;;    (setq liquid-tip-style 'ascii)
;;
;; For emacs', balloon based popups, use:
;;    (setq liquid-tip-style 'balloon)

;;(defvar liquid-tip-style 'balloon)

(defun liquid-tip/pad-line (str)
  "Add extra blanks before and after STR."
  (concat " " str " "))

(defun liquid-tip/popup-tip-pad (text)
  "Add extra blanks before and after TEXT."
  (let* ((lines     (split-string text "\n"))
         (pad-lines (mapcar 'liquid-tip/pad-line lines))
         (pad-text  (concat "\n" (mapconcat 'identity pad-lines "\n") "\n")))
    (popup-tip pad-text)))

(defun liquid-tip/popup-balloon (text)
  "Display TEXT in a balloon popup."
  (liquid-tip/popup-tip-pad text))

;; (if (and (functionp 'ac-quick-help-use-pos-tip-p)
;;          (ac-quick-help-use-pos-tip-p))
;;     (pos-tip-show text 'popup-tip-face nil nil 300 popup-tip-max-width)
;;   (popup-tip-pad text)))

(defun liquid-tip/popup-ascii (text)
  "Display TEXT in ascii popup."
  (liquid-tip/popup-tip-pad text))

(defun liquid-tip/popup (text)
  "Display TEXT."
  (let ((f (case liquid-tip/style
             ('ascii 'liquid-tip/popup-ascii)
             (otherwise 'liquid-tip/popup-balloon))))
    (funcall f text)))


;; -- Compute range ---------------------------------------------------------

(defvar liquid-tip/id-regexp
      (rx (one-or-more (not (in " \n\t()[]{}")))))

(defvar liquid-tip/splitters
      ;; List of  identifier splitters.
      '( ?\s  ?\t ?\n ?\( ?\) ?\[ ?\] ))

(defun liquid-tip/is-split (char)
  "Predicate to check if CHAR is a splitter?"
  (member char liquid-tip/splitters))

(defun liquid-tip/id-start-pos (low p)
  "Find the largest position more than LOW but less than P that is a splitter."
  (let* ((ch (char-before p)))
    (if (or (<= p low) (liquid-tip/is-split ch))
        p
      (liquid-tip/id-start-pos low (- p 1)))))

(defun liquid-tip/column-number-at-pos (pos)
  "Find the column of position POS."
  (+ (- pos (line-beginning-position)) 1))

(defun liquid-tip/start-column-number-at-pos (pos)
  "Find the starting column of identifier at POS."
  (let* ((low   (line-beginning-position))
         (start (liquid-tip/id-start-pos low pos)))
    (liquid-tip/column-number-at-pos start)))

(defsubst liquid-tip/get-position ()
  "Return the current cursor position."
  (save-excursion
    (widen)
    (make-position
     :file (expand-file-name (buffer-file-name))
     :row  (line-number-at-pos)
     :col  (liquid-tip/start-column-number-at-pos (point)))))

(defun liquid-tip/position-string (pos)
  "Return the current POS as a string."
  (format "(%s, %s) in [%s]"
          (position-row pos)
          (position-col pos)
          (position-file pos)))

;; DEBUG (defun liquid-annot-at-pos-0 (pos)
;; DEBUG   "Info to display: just the file/line/constant string"
;; DEBUG   (let* ((info  (format "hello!")))
;; DEBUG     (format "the information at %s is %s"
;; DEBUG        (position-string pos)
;; DEBUG        info)))

;; DEBUG (defun liquid-annot-at-pos-1 (pos)
;; DEBUG   "Info to display: the identifier at the position or NONE"
;; DEBUG   (let* ((ident (liquid-ident-at-pos pos)))
;; DEBUG     (format "the identifier at %s is %s"
;; DEBUG        (position-string pos)
;; DEBUG        ident)))

(defun liquid-tip/ident-at-pos (pos)
  "Return the identifier at POS."
  (thing-at-point 'word))

(defun liquid-tip/annot-at-pos (pos)
  "Info to display: type annotation for the identifier at POS or NONE."
  (let* ((file (position-file pos))
         (row  (position-row  pos))
         (col  (position-col  pos)))
    (liquid-tip/annot-get file row col)))

;; (defun liquid-annot-at-pos (pos)
;;   "Determine info to display at POS."
;;   (liquid-annot-at-pos-2 pos))


(defun liquid-tip-show ()
  "Popup help about anything at point."
  (interactive)
  (let* ((pos    (liquid-tip/get-position))
         (ident  (liquid-tip/ident-at-pos pos))
         (sorry  (format "No information for %s" ident))
         (annot  (liquid-tip/annot-at-pos pos)))
    (if annot
        (liquid-tip/popup annot)
      ;; (hdevtools/show-type-info)
      (liquid-tip/popup sorry)
      )))



;; (defun liquid-tip-init (&optional mode)
;;   "Initialize liquid-tip by making all identifiers buttons using MODE."
;;   (interactive)
;;   (progn (if mode (setq liquid-tip-style mode))
;;      (button-lock-mode 1)
;;      (button-lock-set-button liquid-id-regexp
;;                              'liquid-tip-show
;;                              :mouse-face nil
;;                              :face nil
;;                              :face-policy nil
;;                              :mouse-binding liquid-tip-trigger)
;;      ))





(defun liquid-tip/update1 (mode)
  "Update liquid-annot-table by reloading annot file for buffer in MODE."
  (let* ((pos  (liquid-tip/get-position))
         (file (position-file pos)))
    (liquid-tip/annot-set file mode)))

;;;###autoload
(defun liquid-tip-update ()
  (interactive)
  (liquid-tip/update1 liquid-tip/checker-name))

;; For simple, ascii popups, use:
;;    (liquid-tip-init 'ascii)
;; For emacs', balloon based popups, use:
;;    (liquid-tip-init 'balloon)
;; or just
;;    (liquid-tip-init 'balloon)



(defun liquid-tip/toggle (&optional args)
  "Initialize/uninitialize liquid-tip when the minor mode is toggled."
  (let ((liquid-button-lock-off nil))

    (defun liquid-tip-set ()
      ;; turn on button-lock if it isn't already on
      (when (not button-lock-mode)
        (button-lock-mode 1)
        (setq liquid-button-lock-off 1))
      (button-lock-set-button liquid-tip/id-regexp
                              'liquid-tip-show
                              :mouse-face nil
                              :face nil
                              :face-policy nil
                              :mouse-binding liquid-tip/trigger))

    (defun liquid-tip-unset ()
      ;; turn off button-lock if we turned it on
      (when liquid-button-lock-off
        (button-lock-mode 0)
        (setq liquid-button-lock-off nil))
      ;;(button-lock-unset-button liquid-button)
      (button-lock-unset-button liquid-tip/id-regexp
                                'liquid-tip-show
                                :mouse-face nil
                                :face nil
                                :face-policy nil
                                :mouse-binding liquid-tip/trigger))

    (if liquid-tip-mode
        (liquid-tip-set)
      (liquid-tip-unset))))


;;;###autoload
(define-minor-mode liquid-tip-mode
  "Make liquid-tip a minor mode."
  nil                                   ; init-value
  " tip"                                ; lighter
  nil                                   ; keymap
  :global nil
  :group 'liquid-tip
  (liquid-tip/toggle))

(provide 'liquid-tip)

;; Local Variables:
;; coding: utf-8
;; mode: emacs-lisp
;; End:

;;; liquid-tip.el ends here
