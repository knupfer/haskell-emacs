;;; haskell-emacs.el --- write emacs extensions in haskell

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
;; Version: 1.0
;; email: (rot13 "sxahcsre@tznvy.pbz")
;; Keywords: haskell, emacs, ffi
;; URL: https://github.com/knupfer/haskell-emacs

;;; Commentary:

;; haskell-emacs is a library which allows extending emacs in haskell.
;; It provides an FFI (foreign function interface) for haskell functions.

;; Run `haskell-emacs-init' or put it into your .emacs.  Afterwards just
;; populate your `haskell-emacs-dir' with haskell modules, which
;; export functions.  These functions will be wrapped automatically into
;; an elisp function with the name Module.function.

;; See documentation for `haskell-emacs-init' for a detailed example
;; of usage.

;;; Code:

(defgroup haskell-emacs nil
  "FFI for using haskell in emacs."
  :group 'haskell)

(defcustom haskell-emacs-dir "~/.emacs.d/haskell-fun/"
  "Directory with haskell modules."
  :group 'haskell-emacs
  :type 'string)

(defcustom haskell-emacs-cores 2
  "Number of cores used for haskell Emacs."
  :group 'haskell-emacs
  :type 'integer)

(defvar he/load-dir (file-name-directory load-file-name))
(defvar he/response nil)
(defvar he/count 0)
(defvar he/table (make-hash-table))
(defvar he/proc nil)

;;;###autoload
(defun haskell-emacs-init ()
  "Initialize haskell FFI or reload it to reflect changed functions.

It will try to wrap all exported functions within
`haskell-emacs-dir' into an synchronous and an asynchronous elisp
function.

Dependencies:
 - GHC
 - attoparsec
 - atto-lisp
 - text-show

Consider that you've got the following toy program:

---- ~/.emacs.d/haskell-fun/Matrix.hs
module Matrix (transpose, dyadic) where

import qualified Data.List as L

transpose :: [[Int]] -> [[Int]]
transpose = L.transpose

dyadic :: [Int] -> [Int] -> [[Int]]
dyadic xs ys = map (\\x -> map (x*) ys) xs
----

Now call `haskell-emacs-init' to provide the elisp wrappers.

  (Matrix.transpose '((1 2) (3 4) (5 6)))
    => ((1 3 5) (2 4 6))

  (Matrix.dyadic '(1 2 3) '(4 5 6))
    => ((4 5 6) (8 10 12) (12 15 18))

If you provide bad input, a description of the type error will be
shown to you.

If you call the async pendant of your functions, you'll get a
future which will block on evaluation if the result is not already present.

  (Matrix.transpose-async '((1 2) (3 4) (5 6)))
    => (he/get 7)

  (eval (he/get 7))
    => ((1 3 5) (2 4 6))

Or perhaps more convenient:

  (let ((tr (Matrix.transpose-async '((1 2) (3 4) (5 6)))))

       ;; other elisp stuff, or more asyncs

       (eval tr))

Haskell-emacs can handle functions of arbitrary arity (including
0), but you should note, that only monomorphic functions are
supported, and only about ten different types."
  (interactive)
  (unless (file-directory-p haskell-emacs-dir)
    (mkdir haskell-emacs-dir t))
  (let ((funs (directory-files haskell-emacs-dir nil "^[^.].*\.hs$"))
        (process-connection-type nil)
        (arity-list)
        (heF ".HaskellEmacs.hs")
        (heE (concat haskell-emacs-dir ".HaskellEmacs"))
        (code (with-temp-buffer
                (insert-file-contents (concat he/load-dir "HaskellEmacs.hs"))
                (buffer-string)))
        (start-proc '(progn (when he/proc
                              (set-process-sentinel he/proc nil)
                              (delete-process he/proc))
                            (setq he/proc (start-process "hask" nil heE))
                            (set-process-filter he/proc 'he/filter))))
    (unless (file-exists-p heE)
      (he/compile code))
    (eval start-proc)
    (setq funs (mapcar (lambda (f) (with-temp-buffer
                                     (insert-file-contents
                                      (concat haskell-emacs-dir f))
                                     (buffer-string)))
                       funs)
          funs (eval (he/fun-body "allExports" (list funs))))
    (dotimes (a 2)
      (setq arity-list (eval (he/fun-body "arityList" nil)))
      (he/compile
       (eval (he/fun-body "formatCode"
                          (list (list (car funs)
                                      (car arity-list)
                                      (eval (he/fun-body "arityFormat"
                                                         (cdr funs))))
                                code))))
      (eval start-proc))
    (set-process-sentinel he/proc (lambda (proc sign)
                                    (setq he/response nil)
                                    (haskell-emacs-init)
                                    (let ((debug-on-error t))
                                      (error "Haskell-emacs crashed"))))
    (set-process-query-on-exit-flag he/proc nil)
    (let ((arity (cadr arity-list)))
      (mapc (lambda (func) (eval (he/fun-wrapper func (pop arity))))
            (cadr funs))))
  (message "Finished compiling."))

(defun he/filter (process output)
  "Haskell PROCESS filter for OUTPUT from functions."
  (setq he/response (concat he/response output))
  (let* ((header (read he/response))
         (headLen (+ (car header) (length (format "%s" header)))))
    (while (<= headLen (length he/response))
      (let ((content (substring he/response (- headLen (car header)) headLen)))
        (setq he/response (substring he/response headLen))
        (when (eq 3 (length header)) (error content))
        (puthash (cadr header) content he/table)
        (when (> (length he/response) 7)
          (setq header (read he/response)
                headLen (+ (car header) (length (format "%s" header)))))))))

(defun he/fun-body (fun args)
  "Generate function body for FUN with ARGS."
  (let ((arguments))
    (setq he/count (+ 1 he/count))
    (if (not args)
        (setq arguments "0")
      (setq arguments
            (mapcar
             (lambda (ARG)
               (if (stringp ARG)
                   (format "%S" (substring-no-properties ARG))
                 (if (or (listp ARG) (arrayp ARG))
                     (concat "(" (apply 'concat
                                        (mapcar (lambda (x)
                                                  (concat (format "%S" x) "\n"))
                                                (he/array-to-list ARG))) ")")
                   (format "%S" ARG))))
             args))
      (if (eql 1 (length arguments))
          (setq arguments (car arguments))
        (setq arguments (mapcar (lambda (x) (concat x " ")) arguments)
              arguments (concat "(" (apply 'concat arguments) ")"))))
    (process-send-string
     he/proc (concat fun " " (number-to-string he/count) " " arguments "\n")))
  (list 'he/get he/count))

(defun he/fun-wrapper (fun args)
  "Take FUN with ARGS and return wrappers in elisp."
  (let ((body `(he/fun-body ,fun ,(read (concat "(list " (substring args 1))))))
    `(progn (byte-compile (defun ,(intern fun) ,(read args)
                            (let ((he/count -1)) (eval ,body))))
            (byte-compile (defun ,(intern (concat fun "-async")) ,(read args)
                            ,body)))))

(defun he/get (id)
  "Retrieve result from haskell process with ID."
  (while (not (gethash id he/table))
    (accept-process-output he/proc))
  (let ((res (gethash id he/table)))
    (remhash id he/table)
    (read res)))

(defun he/array-to-list (array)
  "Take a sequence and turn all ARRAY to lists."
  (mapcar (lambda (x) (if (and (not (stringp x)) (or (arrayp x) (listp x)))
                          (he/array-to-list x) x))
          array))

(defun he/compile (code)
  "Inject into CODE a list of IMPORT and of EXPORT and compile it."
  (with-temp-buffer
    (let ((heB "*HASKELL-BUFFER*")
          (heF ".HaskellEmacs.hs"))
      (cd haskell-emacs-dir)
      (unless (and (file-exists-p heF)
                   (equal code (with-temp-buffer (insert-file-contents heF)
                                                 (buffer-string))))
        (insert code)
        (write-file heF)
        (message "Compiling ..."))
      (if (eql 0 (call-process "ghc" nil heB nil "-O2" "-threaded" "--make"
                               (concat "-with-rtsopts=-N"
                                       (number-to-string haskell-emacs-cores))
                               heF))
          (kill-buffer heB)
        (let ((bug (with-current-buffer heB (buffer-string))))
          (kill-buffer heB)
          (error bug))))))

(provide 'haskell-emacs)

;;; haskell-emacs.el ends here
