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

;;; Usage:

;; Put this file into your load path and put (require 'haskell-emacs)
;; and (haskell-emacs-init) into your .emacs.  Afterwards just
;; populate your `haskell-emacs-dir' with code.

;;; Code:

(defvar haskell-emacs-dir "~/.emacs.d/haskell-fun/")
(defvar haskell-emacs--hash-table (make-hash-table))

(defun haskell-emacs-init ()
  "Generate function wrappers from `haskell-emacs-dir'.
When `haskell-emacs-dir' doesn't exist, it will be created."
  (interactive)
  (unless (file-directory-p haskell-emacs-dir)
    (make-directory haskell-emacs-dir t))
  (let ((funs (directory-files haskell-emacs-dir t "^[^.]+$")))
    (mapc (lambda (fun) (when (file-regular-p fun)
                          (eval (haskell-emacs--wrapper-fun fun)))) funs)))

(defun haskell-emacs--wrapper-fun (fun)
  "Take FUN and return a wrapper in elisp."
  `(progn
     (defun ,(car (read-from-string (file-name-base fun)))
         (&optional string &rest args)
       ,(concat (file-name-base fun)
                " is a haskell-function which is started with ARGS
and feeded with a STRING which is piped to "
                (if (file-exists-p (concat fun ".hs"))
                    (concat
                     "this program:\n\n"
                     (with-temp-buffer
                       (insert-file-contents (concat fun ".hs"))
                       (substring (format "%S" (buffer-string)) 1 -1)))
                  "a binary."))
       (let* ((hash (sxhash (list ,(file-name-base fun) string args)))
              (value (gethash hash haskell-emacs--hash-table)))
         (if value
             value
           (with-temp-buffer
             (when string (insert string))
             (apply (function call-process-region) (point-min) (point-max)
                    ,fun t t nil args)
             (puthash hash (buffer-string) haskell-emacs--hash-table)))))
     (advice-add ',(car (read-from-string (file-name-base fun))) :before
                 (lambda (&optional string &rest args) "Haskell function"))))

(provide 'haskell-emacs)

;;; haskell-emacs.el ends here
