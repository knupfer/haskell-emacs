;;; haskell-emacs.el --- write emacs extensions in haskell

;; Copyright (C) 2014-2015 Florian Knupfer

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
;; email: fknupfer@gmail.com
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

(defconst haskell-emacs-api-hash
  (with-temp-buffer
    (insert-file-contents load-file-name)
    (insert-file-contents
     (concat (file-name-directory load-file-name) "HaskellEmacs.hs"))
    (md5 (buffer-string))))

(defgroup haskell-emacs nil
  "FFI for using haskell in emacs."
  :group 'haskell)

(defcustom haskell-emacs-dir "~/.emacs.d/haskell-fun/"
  "Directory with haskell modules."
  :group 'haskell-emacs
  :type 'string)

(defcustom haskell-emacs-ghc-flags '("-O2" "-threaded" "--make"
                                     "-with-rtsopts=-N")
  "Flags which are used for compilation."
  :group 'haskell-emacs
  :type '(repeat string))

(defcustom haskell-emacs-ghc-executable "ghc"
  "Executable used for compilation."
  :group 'haskell-emacs
  :type 'string)

(defvar haskell-emacs--load-dir (file-name-directory load-file-name))
(defvar haskell-emacs--response nil)
(defvar haskell-emacs--count 0)
(defvar haskell-emacs--table (make-hash-table))
(defvar haskell-emacs--proc nil)
(defvar haskell-emacs--fun-list nil)
(defvar haskell-emacs--module-list nil)

;;;###autoload
(defun haskell-emacs-register-module ()
  "Register an external module.

 If you want to distribute a haskell library for haskell-emacs
 you'll have to write a elisp file which requires haskell-emacs
 and registers your module which resides in the same directory.

Example:

  ;;; haskell-emacs-foo.el --- foo it!

  ;;; Code:
  (require 'haskell-emacs)
  (haskell-emacs-register-module)
  (provide 'haskell-emacs-foo)
  ;;; haskell-emacs-foo.el ends here"
  (add-to-list 'haskell-emacs--module-list (file-name-directory load-file-name))
  (when haskell-emacs--proc (haskell-emacs-init)))

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
    => (haskell-emacs--get 7)

  (eval (haskell-emacs--get 7))
    => ((1 3 5) (2 4 6))

Or perhaps more convenient:

  (let ((tr (Matrix.transpose-async '((1 2) (3 4) (5 6)))))

       ;; other elisp stuff, or more asyncs

       (eval tr))

Haskell-emacs can handle functions of arbitrary arity (including
0), but you should note, that only monomorphic functions are
supported, and only about ten different types.

Functions that take only one argument will be fused on Emacs
side, this allows executing a chain of functions asynchronously:

  (let ((result (Matrix.transpose-async (Matrix.transpose '((1 2) (3 4))))))

    ;; other stuff

    (eval result))
     => ((1 2) (3 4))

Furthermore, it nullifies the small performance overhead (0.07 ms
per function call) between fused functions which allows more
modularity and using haskell for even more basic tasks."
  (interactive)
  (unless (file-directory-p haskell-emacs-dir)
    (mkdir haskell-emacs-dir t))
  (let* ((funs (apply 'append
                      (mapcar (lambda (x) (directory-files x t "^[^.].*\.hs$"))
                              (apply 'list haskell-emacs-dir
                                     haskell-emacs--module-list))))
         (process-connection-type nil)
         (arity-list)
         (docs)
         (heF ".HaskellEmacs.hs")
         (heE (concat haskell-emacs-dir ".HaskellEmacs"
                      (when (eq system-type 'windows-nt) ".exe")))
         (code (with-temp-buffer
                 (insert-file-contents
                  (concat haskell-emacs--load-dir "HaskellEmacs.hs"))
                 (buffer-string)))
         (stop-proc '(when haskell-emacs--proc
                       (set-process-sentinel haskell-emacs--proc nil)
                       (delete-process haskell-emacs--proc)))
         (start-proc '(progn
                        (setq haskell-emacs--proc
                              (start-process "hask" nil heE))
                        (set-process-filter haskell-emacs--proc
                                            'haskell-emacs--filter))))
    (eval stop-proc)
    (setq haskell-emacs--response nil)
    (unless
        (and (file-exists-p heE)
             (with-temp-buffer
               (insert-file-contents (concat haskell-emacs-dir heF))
               (re-search-forward
                (concat "-- " haskell-emacs-api-hash "\n"
                        "-- " (md5 (apply 'concat haskell-emacs--module-list)))
                nil t)))
      (haskell-emacs--compile code))
    (eval start-proc)
    (setq funs (mapcar (lambda (f) (with-temp-buffer
                                     (insert-file-contents f)
                                     (buffer-string)))
                       funs)
          docs (apply 'concat funs)
          funs (haskell-emacs--fun-body "allExports" (apply 'list "" "" funs))
          docs (haskell-emacs--fun-body
                "getDocumentation"
                (list (mapcar (lambda (x) (cadr (split-string x "\\.")))
                              (cadr funs))
                      docs)))
    (dotimes (a 2)
      (setq arity-list (haskell-emacs--fun-body "arityList" '(0)))
      (haskell-emacs--compile
       (haskell-emacs--fun-body
        "formatCode"
        (list (list (car funs)
                    (car arity-list)
                    (haskell-emacs--fun-body "arityFormat"
                                             (car (cdr funs))))
              code))))
    (let ((arity (cadr arity-list))
          (table-of-funs (make-hash-table :test 'equal)))
      (mapc (lambda (func)
              (let ((id (car (split-string func "\\."))))
                (puthash id
                         (concat (gethash id table-of-funs)
                                 (format "%S" (haskell-emacs--fun-wrapper
                                               func (pop arity) (pop docs))))
                         table-of-funs)))
            (cadr funs))
      (maphash (lambda (key value)
                 (with-temp-buffer
                   (let ((buffer-file-name (concat haskell-emacs-dir key ".hs")))
                     (insert value)
                     (eval-buffer))))
               table-of-funs)))
  (message "Finished compiling."))

(defun haskell-emacs--filter (process output)
  "Haskell PROCESS filter for OUTPUT from functions."
  (unless (= 0 (length haskell-emacs--response))
    (setq output (concat haskell-emacs--response output)
          haskell-emacs--response nil))
  (let ((header)
        (dataLen)
        (p))
    (while (and (setq p (string-match ")" output))
                (<= (setq header (read output)
                          dataLen (+ (car header) 1 p))
                    (length output)))
      (let ((content (substring output (- dataLen (car header)) dataLen)))
        (setq output (substring output dataLen))
        (when (= 3 (length header)) (error content))
        (puthash (cadr header) content haskell-emacs--table))))
  (unless (= 0 (length output))
    (setq haskell-emacs--response output)))

(defun haskell-emacs--fun-body (fun args)
  "Generate function body for FUN with ARGS."
  (process-send-string
   haskell-emacs--proc (concat "(" fun " "
                               (substring (haskell-emacs--optimize-ast args)
                                          1)))
  (haskell-emacs--get 0))

(defun haskell-emacs--optimize-ast (lisp)
  "Optimize the ast of LISP."
  (if (stringp lisp)
      (format "%S" (substring-no-properties lisp))
    (if (or (listp lisp) (arrayp lisp))
        (if (and (symbolp (car lisp))
                 (not (member (car lisp) haskell-emacs--fun-list))
                 (not (equal t (car lisp)))
                 (not (equal nil (car lisp))))
            (haskell-emacs--optimize-ast (eval lisp))
          (concat "("
                  (apply 'concat
                         (mapcar (lambda (x)
                                   (concat (haskell-emacs--optimize-ast x) "\n"))
                                 lisp))
                  ")"))
      (if (and (symbolp lisp)
               (not (member lisp haskell-emacs--fun-list)))
          (format "%S" (eval lisp))
        (format "%s" lisp)))))

(defun haskell-emacs--fun-wrapper (fun args docs)
  "Take FUN with ARGS and return wrappers in elisp with the DOCS."
  `(progn (add-to-list
           'haskell-emacs--fun-list
           (defmacro ,(intern fun) ,(read args)
             ,docs
             `(progn
                (process-send-string
                 haskell-emacs--proc
                 (concat
                  ,(haskell-emacs--optimize-ast
                    ,(cons 'list (cons `',(read fun) (read args))))))
                (haskell-emacs--get 0))))
          (defmacro ,(intern (concat fun "-async")) ,(read args)
            ,docs
            `(progn (process-send-string
                     haskell-emacs--proc
                     ,(concat
                       (number-to-string (setq haskell-emacs--count
                                               (+ haskell-emacs--count 1)))
                       (haskell-emacs--optimize-ast
                        ,(cons 'list (cons `',(read fun) (read args))))))
                    (quote (haskell-emacs--get ,haskell-emacs--count))))))

(defun haskell-emacs--get (id)
  "Retrieve result from haskell process with ID."
  (while (not (gethash id haskell-emacs--table))
    (accept-process-output haskell-emacs--proc))
  (let ((res (gethash id haskell-emacs--table)))
    (remhash id haskell-emacs--table)
    (read res)))

(defun haskell-emacs--compile (code)
  "Use CODE to compile a new haskell Emacs programm."
  (when haskell-emacs--proc
    (set-process-sentinel haskell-emacs--proc nil)
    (delete-process haskell-emacs--proc))
  (with-temp-buffer
    (let ((heB "*HASKELL-BUFFER*")
          (heF ".HaskellEmacs.hs")
          (code (concat "-- " haskell-emacs-api-hash "\n"
                        "-- " (md5 (apply 'concat haskell-emacs--module-list))
                        "\n" code)))
      (cd haskell-emacs-dir)
      (unless (and (file-exists-p heF)
                   (equal code (with-temp-buffer (insert-file-contents heF)
                                                 (buffer-string))))
        (insert code)
        (write-file heF))
      (message "Compiling ...")
      (if (eql 0 (apply 'call-process haskell-emacs-ghc-executable
                        nil heB nil heF
                        (if haskell-emacs--module-list
                            (cons
                             (concat "-i"
                                     (substring
                                      (apply 'concat
                                             (mapcar (lambda (x) (concat ":" x))
                                                     haskell-emacs--module-list))
                                      1))
                             haskell-emacs-ghc-flags)
                          haskell-emacs-ghc-flags)))
          (kill-buffer heB)
        (let ((bug (with-current-buffer heB (buffer-string))))
          (kill-buffer heB)
          (error bug)))))
  (setq haskell-emacs--proc
        (start-process "hask" nil
                       (concat haskell-emacs-dir ".HaskellEmacs"
                               (when (eq system-type 'windows-nt) ".exe"))))
  (set-process-filter haskell-emacs--proc 'haskell-emacs--filter)
  (set-process-query-on-exit-flag haskell-emacs--proc nil)
  (set-process-sentinel haskell-emacs--proc
                        (lambda (proc sign)
                          (let ((debug-on-error t))
                            (error "Haskell-emacs crashed")))))

(provide 'haskell-emacs)

;;; haskell-emacs.el ends here
