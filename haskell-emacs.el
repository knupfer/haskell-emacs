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

;; Put this file into your load path and put (require 'haskell-emacs)
;; and (haskell-emacs-init) into your .emacs.  Afterwards just
;; populate your `haskell-emacs-dir' with code.

;;; Example:

;;; Code:

(defvar haskell-emacs-load-dir (file-name-directory load-file-name))
(defvar haskell-emacs-dir "~/.emacs.d/haskell-fun/")

(defvar he/response nil)
(defvar he/count 0)
(defvar he/table (make-hash-table))
(defvar he/proc nil)

(defun haskell-emacs-init ()
  "Initialize haskell FFI or reload it to reflect changed functions."
  (interactive)
  (unless (file-directory-p haskell-emacs-dir)
    (mkdir haskell-emacs-dir t))
  (let ((funs (directory-files haskell-emacs-dir nil "^[^.].*\.hs$")))
    (let ((imports)
          (exports))
      (setq imports (apply 'concat
                           (mapcar (lambda (f)
                                     (let ((ex (he/exports-get f)))
                                       (when ex
                                         (add-to-list 'exports ex)
                                         (concat "import qualified "
                                                 (substring f 0 -3) "\n"))))
                                   funs)))
      (he/compile (with-temp-buffer
                    (insert-file-contents
                     (concat haskell-emacs-load-dir "HaskellEmacs.hs"))
                    (buffer-string))
                  imports (he/exports-format exports))
      (when he/proc
        (delete-process he/proc))
      (let ((process-connection-type nil))
        (setq he/proc
              (start-process "hask" nil
                             (concat haskell-emacs-dir ".HaskellEmacs"))))
      (set-process-query-on-exit-flag he/proc nil)
      (set-process-filter he/proc 'he/filter)
      (mapc (lambda (fi) (mapc (lambda (fu) (eval (he/fun-wrapper fu))) fi))
            exports))))

(defun he/filter (process output)
  "Haskell PROCESS filter for OUTPUT from functions."
  (setq he/response (concat he/response output))
  (let ((header (read he/response)))
    (while (<= (+ (car header) (length (format "%s" header)))
               (length he/response))
      (unless (car (last header))
        (let ((bug (substring he/response
                              (length (format "%s" header))
                              (+ (length (format "%s" header)) (car header)))))
          (setq he/response (substring he/response
                                       (+ (length (format "%s" header))
                                          (car header))))
          (error bug)))
      (puthash (cadr header)
               (substring he/response
                          (length (format "%s" header))
                          (+ (length (format "%s" header)) (car header)))
               he/table)
      (setq he/response (substring he/response (+ (length (format "%s" header))
                                                  (car header))))
      (when (> (length he/response) 7)
        (setq header (read he/response))))))

(defun he/fun-body (fun)
  "Generate function body for FUN."
  (setq ARG
        (if (stringp ARG)
            (format "%S" (substring-no-properties ARG))
          (if (or (listp ARG) (arrayp ARG))
              (concat "("
                      (apply 'concat (mapcar (lambda (x)
                                               (concat (format "%S" x) "\n"))
                                             (he/array-to-list ARG))) ")")
            (format "%S" ARG))))
  (setq he/count (+ 1 he/count))
  (process-send-string
   he/proc (concat fun " " (number-to-string he/count) " "
                   (number-to-string
                    (with-temp-buffer (insert ARG)
                                      (let ((lines 1))
                                        (goto-char (point-min))
                                        (while (re-search-forward "\n" nil t)
                                          (setq lines (+ lines 1)))
                                        lines)))
                   "\n" ARG "\n")))

(defun he/fun-wrapper (fun)
  "Take FUN and return wrappers in elisp."
  (let ((body `(he/fun-body ,fun)))
    `(progn (byte-compile (defun ,(intern fun) (ARG) ,body
                                 (he/get he/count)))
            (byte-compile (defun ,(intern (concat fun "-async")) (ARG) ,body
                                 `(he/get ,he/count))))))

(defun he/get (id)
  "Retrieve result from haskell process with ID."
  (while (not (gethash id he/table))
    (accept-process-output he/proc))
  (read (gethash id he/table)))

(defun he/array-to-list (array)
  "Take a sequence and turn all ARRAY to lists."
  (mapcar (lambda (x) (if (and (not (stringp x)) (or (arrayp x) (listp x)))
                          (he/array-to-list x) x))
          array))

(defun he/exports-format (list-of-exports)
  "Take a LIST-OF-EXPORTS and format it into haskell syntax."
  (let* ((tr '(mapcar (lambda (y) (concat "(\""y"\",transform "y"),")) x))
         (result (apply 'concat (mapcar (lambda (x) (apply 'concat (eval tr)))
                                        list-of-exports))))
    (if (> (length result) 0)
        (substring result 0 -1)
      "")))

(defun he/exports-get (file)
  "Get a list of exports from FILE."
  (let ((f-base (file-name-base file))
        (w "[ \t\n\r]"))
    (with-temp-buffer
      (insert-file-contents (concat haskell-emacs-dir file))
      (when (re-search-forward
             (concat "^module" w "+" f-base
                     w "*(" w "*\\(\\(?:[^)]\\|" w "\\)+\\)" w "*)")
             nil t)
        (mapcar (lambda (fun) (concat f-base "."
                                      (car (last (split-string fun "\\.")))))
                (split-string (match-string 1) "[ \n\r,]+"))))))

(defun he/compile (code import export)
  "Inject into CODE a list of IMPORT and of EXPORT and compile it."
  (with-temp-buffer
    (let ((heF ".HaskellEmacs.hs")
          (heB "*HASKELL-BUFFER*"))
      (insert code)
      (goto-char (point-min))
      (when (re-search-forward "---- <<import>> ----" nil t)
        (replace-match import))
      (when (re-search-forward "---- <<export>> ----" nil t)
        (replace-match export))
      (cd haskell-emacs-dir)
      (unless (and (file-exists-p heF)
                   (equal (buffer-string)
                          (with-temp-buffer (insert-file-contents heF)
                                            (buffer-string))))
        (write-file heF))
      (unless (equal 0 (call-process "ghc" nil heB nil
                                     "-O2" "-threaded" "--make"
                                     "-with-rtsopts=-N2" heF))
        (let ((bug (with-current-buffer heB (buffer-string))))
          (kill-buffer heB)
          (error bug)))
      (kill-buffer heB))))

(provide 'haskell-emacs)

;;; haskell-emacs.el ends here
