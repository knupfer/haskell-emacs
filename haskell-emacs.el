;;; emacs-haskell.el --- write emacs extensions in haskell

;; Copyright (C) 2014 Florian Knupfer

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA 02110-1301 USA

;; Author: Florian Knupfer
;; email: (rot13 "sxahcsre@tznvy.pbz")

;;; Commentary:

;; This file provides an easy wrapper for haskell functions.  Every
;; haskell executable in `haskell-emacs-dir' will get a emacs
;; function, with the name of the executable.  It is recommended to
;; use interactive in the haskell programm, strings will be piped and
;; output of the program will be returned.  The function definitions
;; take place with `haskell-emacs-init', so every function, which is
;; already defined at that moment is getting overridden.  It can be
;; used to piece-wise replace builtins or extend emacs.

;;; Code:

(defvar haskell-emacs-dir "~/.emacs.d/haskell-fun/")

(defun haskell-emacs-init ()
  "Generate function wrappers from `haskell-emacs-dir'.
When `haskell-emacs-dir' doesn't exist, it will be created."
  (interactive)
  (unless (file-directory-p haskell-emacs-dir)
    (make-directory haskell-emacs-dir t))
  (let ((funs (directory-files haskell-emacs-dir t "^[^.]+$")))
    (with-temp-buffer
      (mapc
       (lambda (fun)
	 (when (file-regular-p fun)
	   (insert
	    "(defun " (file-name-base fun) " (string)\n"
	    "  \""(file-name-base fun)" is a haskell-function which is feeded\n"
	    "with a STRING which is piped to "
	    (if (file-exists-p (concat fun ".hs"))
		(concat
		 "this program:\n\n"
		 (with-temp-buffer
		   (insert-file-contents (concat fun ".hs"))
		   (buffer-string)))
	      "a binary.") "\""
	      "  (with-temp-buffer\n"
	      "    (insert string)\n"
	      "    (call-process-region (point-min) (point-max)\n"
	      "    \""fun"\" t t)\n"
	      "    (buffer-string)))\n\n"
	      "(advice-add '" (file-name-base fun)
	      " :before (lambda (x) \"Haskell function\" x))\n\n"
	      ))) funs)
      (eval-buffer))))

(provide 'haskell-emacs)

;;; haskell-emacs.el ends here
