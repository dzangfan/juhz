;;; juhz-mode.el --- Major mode of Juhz programming language  -*- lexical-binding: t; -*-

;; Copyright (C) 2023  MOMO

;; Author: MOMO <dzangfan@MOMO>
;; Keywords: languages

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; 

;;; Code:

(require 'cl-lib)
(require 'generic-x)

(defvar juhz-mode-syntax-table
  (let ((st (make-syntax-table)))
    (modify-syntax-entry ?  "-" st)
    (modify-syntax-entry ?\t "-" st)
    (modify-syntax-entry ?\n "-" st)
    (modify-syntax-entry '(?a . ?z) "w" st)
    (modify-syntax-entry '(?A . ?Z) "w" st)
    (modify-syntax-entry ?_ "w" st)
    (modify-syntax-entry '(?0 . ?9) "w" st)
    (dolist (punct '(?. ?, ?/ ?+ ?- ?= ?* ?& ?% ?! ?< ?> ?\; ?| ?:))
      (modify-syntax-entry punct "." st))
    (modify-syntax-entry ?\" "\"" st)
    (modify-syntax-entry ?\( "()")
    (modify-syntax-entry ?\) ")(")
    (modify-syntax-entry ?\[ "(]")
    (modify-syntax-entry ?\] ")[")
    (modify-syntax-entry ?\{ "(}")
    (modify-syntax-entry ?\} "){")
    st))

(defvar juhz-mode--marker-list
  '(?\; ?\{ ?\} ?\[ ?\] ?\( ?\) ?= ?:))

(defun juhz-mode--previous-marker ()
  (while (and (/= 1 (point)) (not (memq (char-before) juhz-mode--marker-list)))
    (backward-char)))

(defun juhz-mode--line-match-p (regexp)
  (string-match-p regexp
		  (buffer-substring (line-beginning-position)
				    (line-end-position))))

(defun juhz-mode--find-char-in-same-level (char-lst)
  (let* ((stack nil)
	 (soft-pop (lambda (n) (when (and stack (= n (car stack))) (pop stack)))))
    (backward-char)
    (while (not (or (= 1 (point)) (and (null stack) (memq (char-before) char-lst))))
      (cl-case (char-before)
	((?\)) (push 0 stack))
	((?\]) (push 1 stack))
	((?\}) (push 2 stack))
	((?\() (funcall soft-pop 0))
	((?\[) (funcall soft-pop 1))
	((?\{) (funcall soft-pop 2)))
      (backward-char))))

(defun juhz-mode--determine-sequential-indentation ()
  (let ((current-line (line-number-at-pos)))
    (juhz-mode--find-char-in-same-level '(?\; ?\{ ?\}))
    (cond ((= 1 (point)) 0)
	  ((eq ?\} (char-before)) (current-indentation))
	  ((eq ?\; (char-before))
	   (if (= (line-number-at-pos) current-line)
	       (current-indentation)
	     (next-line)
	     (current-indentation)))
	  ((eq ?\{ (char-before))
	   (+ (current-indentation) 2)))))

(defun juhz-mode--determine-indentation ()
  (if (juhz-mode--line-match-p "^\\s-*\\s)")
      (progn
	(beginning-of-line)
	(juhz-mode--previous-marker)
	(cl-case (char-before)
	  ((?\{ ?\[ ?\()
	   (backward-char)
	   (juhz-mode--determine-sequential-indentation))
	  ((?\) ?\] ?\}) (- (current-indentation) 2))
	  ((?\;) (- (juhz-mode--determine-sequential-indentation) 2))))
    (let ((current-line (line-number-at-pos)))
      (juhz-mode--previous-marker)
      (while (and (/= 1 (point)) (= current-line (line-number-at-pos)))
	(backward-char)
	(juhz-mode--previous-marker)))
    (if (= 1 (point))
	0
      (cl-case (char-before)
	((?: ?= ?\{ ?\[ ?\() (+ (current-indentation) 2))
	((?\) ?\] ?\}) (current-indentation))
	((?\;) (juhz-mode--determine-sequential-indentation))))))

(defun juhz-mode--indent-line-function ()
  (let ((indentation (save-excursion (juhz-mode--determine-indentation))))
    (if (< indentation 0)
	(indent-line-to 0)
      (indent-line-to indentation))))

(defvar juhz-mode--brackets
  '((?\( ?\))
    (?\[ ?\])
    (?\{ ?\})))

(defun juhz-mode--between-curly-bracket-p ()
  (cl-block check-around
    (dolist (bracket juhz-mode--brackets)
      (when (and (/= 1 (line-number-at-pos))
		 (juhz-mode--line-match-p (format "\\s-*%c" (second bracket)))
		 (save-excursion (previous-line)
				 (juhz-mode--line-match-p (format "%c\\s-*$" (first bracket)))))
	(cl-return-from check-around t)))))

(defun juhz-mode--post-self-insert ()
  (when (and (eq ?\n (char-before)) (juhz-mode--between-curly-bracket-p))
    (indent-for-tab-command)
    (previous-line)
    (end-of-line)
    (newline-and-indent)))

(defun juhz-mode--initialize-buffer ()
  (setq-local indent-line-function 'juhz-mode--indent-line-function)
  (setq-local post-self-insert-hook
	      (if (memq 'juhz-mode--post-self-insert post-self-insert-hook)
		  post-self-insert-hook
		(cons 'juhz-mode--post-self-insert post-self-insert-hook))))

(define-generic-mode juhz-mode
  ()
  '("if" "else" "while" "package" "function" "def" "use")
  '(("\\([a-zA-Z_][a-zA-Z0-9_]*\\)(.*)" 1 font-lock-function-name-face)
    ("\\<[a-zA-Z_][a-zA-Z0-9_]*\\>" . font-lock-variable-name-face)
    ("\\([1-9][0-9]*\\)\\|0\\|\\(\\([1-9][0-9]*\\)?\\.[0-9]+\\)" . font-lock-constant-face)
    ("\"\\(\\\\\"\\|\\\\n\\|\\\\\\\\\\|[^\"\\\\]\\)*\"" . font-lock-string-face)
    ("\\<true\\|false\\>" . font-lock-constant-face))
  '("\\.juhz$")
  '(juhz-mode--initialize-buffer))

(provide 'juhz-mode)
;;; juhz-mode.el ends here
