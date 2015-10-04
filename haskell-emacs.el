;;; haskell-emacs.el --- Write emacs extensions in haskell

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
;; Email: fknupfer@gmail.com
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

(if (version< emacs-version "24")
    (progn (require 'cl)
           (defalias 'cl-flet 'flet))
  (require 'cl-macs))

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

(defcustom haskell-emacs-nix-shell-args
  '("-p")
  "Environment used to compile the modules when on an nixos machine."
  :group 'haskell-emacs
  :type '(repeat string))

(defcustom haskell-emacs-ghc-executable "ghc"
  "Executable used for compilation."
  :group 'haskell-emacs
  :type 'string)

(defvar haskell-emacs--api-hash)
(defvar haskell-emacs--count 0)
(defvar haskell-emacs--function-hash nil)
(defvar haskell-emacs--fun-list nil)
(defvar haskell-emacs--is-nixos
  (when (eq system-type 'gnu/linux)
    (string-match " nixos " (shell-command-to-string "uname -a"))))
(defvar haskell-emacs--load-dir (file-name-directory load-file-name))
(defvar haskell-emacs--module-list nil)
(defvar haskell-emacs--proc nil)
(defvar haskell-emacs--response nil)
(defvar haskell-emacs--table (make-hash-table))

(defun haskell-emacs--search-modules ()
  "Search haskell-emacs modules in the `load-path'."
  (let ((modules))
    (mapc (lambda (x)
            (when (file-directory-p x)
              (mapc (lambda (y) (add-to-list 'modules (file-name-directory y)))
                    (directory-files x t "^haskell-emacs-.*\.el"))))
            load-path)
    modules))

;;;###autoload
(defun haskell-emacs-help ()
  "Display the documentation for haskell-emacs."
  (interactive)
  (find-file-read-only-other-window (concat haskell-emacs--load-dir "README.org"))
  (narrow-to-region (save-excursion (goto-char (point-min))
                                    (re-search-forward "^*")
                                    (match-beginning 0)) (point-max))
  (message "Press tab to cycle visibility"))

;;;###autoload
(defun haskell-emacs-init (&optional arg)
  "Initialize haskell FFI or reload it to reflect changed functions.

When ARG, force installation dialog.
Call `haskell-emacs-help' to read the documentation."
  (interactive "p")
  (setq haskell-emacs--module-list (haskell-emacs--search-modules))
  (setq haskell-emacs--api-hash
        (with-temp-buffer
          (mapc (lambda (x) (insert-file-contents (concat haskell-emacs--load-dir x)))
                '("haskell-emacs.el"
                  "HaskellEmacs.hs"
                  "Foreign/Emacs.hs"
                  "Foreign/Emacs/Internal.hs"))
          (sha1 (buffer-string))))
  (let* ((first-time (unless (file-directory-p haskell-emacs-dir)
                       (if arg (haskell-emacs--install-dialog)
                         (mkdir haskell-emacs-dir t))))
         (funs (apply 'append
                      (mapcar (lambda (x) (directory-files x t "^[^.].*\.hs$"))
                              (apply 'list haskell-emacs-dir
                                     haskell-emacs--module-list))))
         (process-connection-type nil)
         (arity-list)
         (docs)
         (has-changed t)
         (heF ".HaskellEmacs.hs")
         (heE (concat haskell-emacs-dir ".HaskellEmacs"
                      (when (eq system-type 'windows-nt) ".exe")))
         (code (with-temp-buffer
                 (insert-file-contents
                  (concat haskell-emacs--load-dir "HaskellEmacs.hs"))
                 (buffer-string)))
         (stop-proc
          '(when haskell-emacs--proc
             (set-process-sentinel haskell-emacs--proc nil)
             (delete-process haskell-emacs--proc)))
         (start-proc
          '(progn
             (setq haskell-emacs--proc
                   (start-process "hask" nil heE))
             (set-process-filter haskell-emacs--proc
                                 'haskell-emacs--filter)
             (set-process-query-on-exit-flag haskell-emacs--proc nil)
             (set-process-sentinel haskell-emacs--proc
                                   (lambda (proc sign)
                                     (let ((debug-on-error t))
                                       (error "Haskell-emacs crashed")))))))
    (eval stop-proc)
    (setq haskell-emacs--response nil)
    (setq haskell-emacs--function-hash
          (with-temp-buffer (mapc 'insert-file-contents funs)
                            (insert haskell-emacs-dir
                                    (format "%S" haskell-emacs-ghc-flags)
                                    (format "%S" haskell-emacs-nix-shell-args)
                                    haskell-emacs-ghc-executable)
                            (sha1 (buffer-string))))
    (setq has-changed
          (not (and (file-exists-p heE)
                    (with-temp-buffer
                      (insert-file-contents (concat haskell-emacs-dir heF))
                      (and (re-search-forward haskell-emacs--api-hash
                                              nil t)
                           (re-search-forward haskell-emacs--function-hash
                                              nil t))))))
    (when has-changed (haskell-emacs--compile code))
    (eval start-proc)
    (setq funs (mapcar (lambda (f) (with-temp-buffer
                                     (insert-file-contents f)
                                     (buffer-string)))
                       funs)
          docs (apply 'concat funs)
          funs (haskell-emacs--fun-body 'allExports (apply 'list "" "" funs)))
    (when (stringp funs)
      (eval stop-proc)
      (error funs))
    (setq docs (haskell-emacs--fun-body
                'getDocumentation
                (list (mapcar (lambda (x) (cadr (split-string x "\\.")))
                              (cadr funs))
                      docs)))
    (dotimes (a 2)
      (setq arity-list (haskell-emacs--fun-body 'arityList '()))
      (when has-changed
        (haskell-emacs--compile
         (haskell-emacs--fun-body
          'formatCode
          (list (list (car funs)
                      (car arity-list)
                      (haskell-emacs--fun-body 'arityFormat
                                               (car (cdr funs))))
                code)))))
    (let ((arity (cadr arity-list))
          (table-of-funs (make-hash-table :test 'equal)))
      (mapc (lambda (func)
              (let ((id (car (split-string func "\\."))))
                (puthash id
                         (concat (gethash id table-of-funs)
                                 (format "%S" (haskell-emacs--fun-wrapper
                                               (read func)
                                               (read (pop arity))
                                               (pop docs))))
                         table-of-funs)))
            (cadr funs))
      (maphash (lambda (key value)
                 (with-temp-buffer
                   (let ((buffer-file-name (concat haskell-emacs-dir key ".hs")))
                     (insert value)
                     (eval-buffer))))
               table-of-funs))
    (when arg
      (if (equal first-time "example")
          (message
           "Now you can run the examples from C-h f haskell-emacs-init.
For example (Matrix.transpose '((1 2 3) (4 5 6)))")
        (if (equal first-time "no-example")
            (message
             "Now you can populate your `haskell-emacs-dir' with haskell modules.
Read C-h f haskell-emacs-init for more instructions")
          (message "Finished compiling haskell-emacs."))))))

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
        (if (= 1 (length header)) (eval (read content))
          (puthash (cadr header) content haskell-emacs--table)))))
  (unless (= 0 (length output))
    (setq haskell-emacs--response output)))

(defun haskell-emacs--fun-body (fun args)
  "Generate function body for FUN with ARGS."
  (process-send-string
   haskell-emacs--proc (format "%S" (cons fun args)))
  (haskell-emacs--get 0))

(defun haskell-emacs--optimize-ast (lisp)
  "Optimize the ast of LISP."
  (if (and (listp lisp)
           (member (car lisp) haskell-emacs--fun-list))
      (cons (car lisp) (mapcar 'haskell-emacs--optimize-ast (cdr lisp)))
    (haskell-emacs--no-properties (eval lisp))))

(defun haskell-emacs--no-properties (xs)
  "Take XS and remove recursively all text properties."
  (if (stringp xs)
      (substring-no-properties xs)
    (if (ring-p xs)
        (haskell-emacs--no-properties (ring-elements xs))
      (if (or (listp xs) (vectorp xs) (bool-vector-p xs))
          (mapcar 'haskell-emacs--no-properties xs)
        (if (hash-table-p xs)
            (let ((pairs))
              (maphash (lambda (k v) (push (list k v) pairs)) xs)
              (haskell-emacs--no-properties pairs))
          xs)))))

(defun haskell-emacs--fun-wrapper (fun args docs)
  "Take FUN with ARGS and return wrappers in elisp with the DOCS."
  `(progn (add-to-list
           'haskell-emacs--fun-list
           (defmacro ,fun ,args
             ,docs
             `(progn (process-send-string
                      haskell-emacs--proc
                      (format "%S" (haskell-emacs--optimize-ast
                                    ',(cons ',fun (list ,@args)))))
                     (haskell-emacs--get 0))))
          (defmacro ,(read (concat (format "%s" fun) "-async")) ,args
            ,docs
            `(progn (process-send-string
                     haskell-emacs--proc
                     (format (concat (number-to-string
                                      (setq haskell-emacs--count
                                            (+ haskell-emacs--count 1))) "%S")
                             (haskell-emacs--optimize-ast
                              ',(cons ',fun (list ,@args)))))
                    (list 'haskell-emacs--get haskell-emacs--count)))))

(defun haskell-emacs--install-dialog ()
  "Run the installation dialog."
  (cl-flet ((cabal (&rest ARGS)
                   (with-temp-buffer
                     (when (file-exists-p haskell-emacs-dir)
                       (cd haskell-emacs-dir))
                     (if (= 0 (apply 'call-process "cabal" nil t nil ARGS))
                         (buffer-string)
                       (error (buffer-string))))))
    (let* ((sandbox (unless (version< (substring (cabal "--numeric-version") 0 -1)
                                      "1.18")
                        (yes-or-no-p "Create a cabal sandbox? ")))
             (install (yes-or-no-p "Cabal install the dependencies? "))
             (update  (when install (yes-or-no-p "Update cabal packages? ")))
             (example (yes-or-no-p "Add a simple example? ")))
        (mkdir haskell-emacs-dir t)
        (when sandbox
          (message "Creating sandbox...")
          (cabal "sandbox" "init"))
        (when update
          (message "Updating cabal packages...")
          (cabal "update"))
        (when install
          (message "Installing dependencies...")
          (mapc (lambda (x) (message (concat "Installing " x "..."))
                  (cabal "install" x))
                '("happy" "haskell-src-exts" "parallel" "utf8-string"))
          (message "Installing atto-lisp...")
          (if (version< (substring (shell-command-to-string
                                      "ghc --numeric-version") 0 -1)
                          "7.10")
                (cabal "install" "atto-lisp")
              (cabal "install" "--allow-newer=deepseq,blaze-builder" "atto-lisp")))
        (if example
            (with-temp-buffer
              (insert "
module Matrix where

import qualified Data.List as L

-- | Takes a matrix (a list of lists of ints) and returns its transposition.
transpose :: [[Int]] -> [[Int]]
transpose = L.transpose

-- | Returns an identity matrix of size n.
identity :: Int -> [[Int]]
identity n
  | n > 1 = L.nub $ L.permutations $ 1 : replicate (n-1) 0
  | otherwise = [[1]]

-- | Check whether a given matrix is a identity matrix.
isIdentity :: [[Int]] -> Bool
isIdentity xs = xs == identity (length xs)

-- | Compute the dyadic product of two vectors.
dyadic :: [Int] -> [Int] -> [[Int]]
dyadic xs ys = map (\\x -> map (x*) ys) xs")
              (write-file (concat haskell-emacs-dir "Matrix.hs"))
              "example")
          "no-example"))))

(defun haskell-emacs--get (id)
  "Retrieve result from haskell process with ID."
  (while (not (gethash id haskell-emacs--table))
    (accept-process-output haskell-emacs--proc))
  (let ((res (read (gethash id haskell-emacs--table))))
    (remhash id haskell-emacs--table)
    (if (and (listp res)
             (or (functionp (car res))
                 (and (not (version< emacs-version "24"))
                      (or (special-form-p (car res))
                          (macrop (car res))))))
        (eval res)
      res)))

(defun haskell-emacs--find-package-db ()
  "Search for the package dir in a cabal sandbox."
  (let ((sandbox (concat haskell-emacs-dir ".cabal-sandbox")))
    (when (file-directory-p sandbox)
      (car (directory-files sandbox t "packages\.conf\.d$")))))

(defun haskell-emacs--compile (code)
  "Use CODE to compile a new haskell Emacs programm."
  (when haskell-emacs--proc
    (set-process-sentinel haskell-emacs--proc nil)
    (delete-process haskell-emacs--proc))
  (with-temp-buffer
    (let* ((heB "*HASKELL-BUFFER*")
           (heF ".HaskellEmacs.hs")
           (code (concat
                  "-- hash of haskell-emacs: " haskell-emacs--api-hash "\n"
                  "-- hash of all functions: " haskell-emacs--function-hash
                  "\n" code))
           (package-db (haskell-emacs--find-package-db))
           (haskell-emacs-ghc-flags
            (if package-db
                (cons (concat "-package-db=" package-db) haskell-emacs-ghc-flags)
              haskell-emacs-ghc-flags)))
      (cd haskell-emacs-dir)
      (unless (and (file-exists-p heF)
                   (equal code (with-temp-buffer (insert-file-contents heF)
                                                 (buffer-string))))
        (insert code)
        (write-file heF)
        (mkdir (concat haskell-emacs-dir "Foreign/Emacs/") t)
        (with-temp-buffer
          (insert-file-contents (concat (file-name-directory haskell-emacs--load-dir)
                                        "Foreign/Emacs.hs"))
          (write-file (concat haskell-emacs-dir "Foreign/Emacs.hs")))
        (with-temp-buffer
          (insert-file-contents (concat (file-name-directory haskell-emacs--load-dir)
                                        "Foreign/Emacs/Internal.hs"))
          (write-file (concat haskell-emacs-dir "Foreign/Emacs/Internal.hs"))))
      (message "Compiling ...")
      (haskell-emacs--compile-command heF heB)))
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

(defun haskell-emacs--compile-command (heF heB)
  "Run the compilation for file HEF with buffer HEB."
  (let ((args (cons heF (if haskell-emacs--module-list
                            (cons (concat
                                   "-i"
                                   (substring
                                    (apply
                                     'concat
                                     (mapcar (lambda (x) (concat ":" x))
                                             haskell-emacs--module-list)) 1))
                                  haskell-emacs-ghc-flags)
                          haskell-emacs-ghc-flags))))
    (if (eql 0 (apply 'call-process (if haskell-emacs--is-nixos "nix-shell"
                                      haskell-emacs-ghc-executable)
                      nil heB nil
                      (if haskell-emacs--is-nixos
                          (append haskell-emacs-nix-shell-args
                                  (list
                                   "--command"
                                   (apply 'concat haskell-emacs-ghc-executable
                                          (mapcar (lambda (x) (concat " " x))
                                                  args))))
                        args)))
        (kill-buffer heB)
      (let ((bug (with-current-buffer heB (buffer-string))))
        (kill-buffer heB)
        (error bug)))))

(provide 'haskell-emacs)

;;; haskell-emacs.el ends here
