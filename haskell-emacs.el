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
(defvar haskell-emacs--response nil)
(defvar haskell-emacs--fun-count 0)
(defvar haskell-emacs--hash-table (make-hash-table))
(defvar haskell-emacs-process nil)
(defvar haskell-emacs-dir "~/.emacs.d/haskell-fun/")
(defun haskell-emacs-init ()
  (interactive)
  (save-excursion
    (unless (file-directory-p haskell-emacs-dir)
      (mkdir haskell-emacs-dir t))
    (let ((funs (directory-files haskell-emacs-dir nil "^[^.].*\.hs$")))
      (let ((imports)
            (exports))
        (setq imports
              (apply 'concat
                     (mapcar
                      (lambda (f)
                        (let ((ex (haskell-emacs-retrieve-exports f)))
                          (when ex
                            (add-to-list 'exports ex)
                            (concat "import qualified "
                                    (substring f 0 -3) "\n"))))
                      funs)))
        (haskell-emacs-compile
         (with-temp-buffer
           (insert-file-contents
            (concat haskell-emacs-load-dir "HaskellEmacs.hs"))
           (buffer-string))
         imports (haskell-emacs-format-exports exports))
        (when haskell-emacs-process
          (delete-process haskell-emacs-process))
        (let ((process-connection-type nil))
          (setq haskell-emacs-process
                (start-process "hask" nil
                               (concat haskell-emacs-dir ".HaskellEmacs"))))
        (set-process-query-on-exit-flag haskell-emacs-process nil)
        (set-process-filter
         haskell-emacs-process
         (lambda (proc str)
           (setq haskell-emacs--response
                 (concat haskell-emacs--response str))
           (let ((header (read haskell-emacs--response)))
             (while (<= (+ (car header) (length (format "%s" header)))
                        (length haskell-emacs--response))
               (unless (car (last header))
                 (let ((bug (substring haskell-emacs--response
                                       (length (format "%s" header))
                                       (+ (length (format "%s" header)) (car header)))))
                   (setq haskell-emacs--response (substring haskell-emacs--response
                                                            (+ (length (format "%s" header))
                                                               (car header))))
                   (error bug)))
               (puthash (cadr header) (substring haskell-emacs--response
                                                 (length (format "%s" header))
                                                 (+ (length (format "%s" header)) (car header)))
                        haskell-emacs--hash-table)
               (setq haskell-emacs--response (substring haskell-emacs--response
                                                        (+ (length (format "%s" header))
                                                           (car header))))
               (when (> (length haskell-emacs--response) 7)
                 (setq header (read haskell-emacs--response)))))))
        (mapc (lambda (fi)
                (mapc (lambda (fu) (eval (haskell-emacs--wrapper-fun fu)))
                      fi))
              exports)))))

(defun haskell-emacs--wrapper-fun (fun)
  "Take FUN and return a wrapper in elisp."
  (let ((skeleton
         `(progn (if (stringp OBJECT)
                     (setq OBJECT (format "%S" (substring-no-properties OBJECT)))
                   (if (or (listp OBJECT) (arrayp OBJECT))
                       (progn
                         (setq OBJECT (haskell-emacs--array-to-list OBJECT))
                         (setq OBJECT
                               (concat "("
                                       (apply 'concat
                                              (mapcar (lambda (x)
                                                        (concat (format "%S" x) "\n"))
                                                      OBJECT))
                                       ")")))
                     (setq OBJECT (format "%S" OBJECT))))
                 (setq haskell-emacs--fun-count (+ 1 haskell-emacs--fun-count))
                 (process-send-string haskell-emacs-process
                                      (concat ,fun " "
                                              (number-to-string haskell-emacs--fun-count)
                                              " " (number-to-string
                                                   (with-temp-buffer (insert OBJECT)
                                                                     (let ((lines 1))
                                                                       (goto-char (point-min))
                                                                       (while (re-search-forward "\n" nil t)
                                                                         (setq lines (+ lines 1)))
                                                                       lines)))
                                              "\n" OBJECT "\n")))))
    `(progn
       (defun ,(intern fun) (OBJECT)
         ,skeleton
         (haskell-emacs--get-result haskell-emacs--fun-count))

       (defun ,(intern (concat fun "-async")) (OBJECT)
         ,skeleton
         `(haskell-emacs--get-result ,haskell-emacs--fun-count))
       (byte-compile ',(intern fun))
       (byte-compile ',(intern (concat fun "-async"))))))

(defun haskell-emacs--get-result (key)
  (while (not (gethash key haskell-emacs--hash-table))
    (accept-process-output haskell-emacs-process))
  (read (gethash key haskell-emacs--hash-table)))

(defun haskell-emacs--array-to-list (a)
  (mapcar (lambda (x) (if (and (not (stringp x))
                               (or (arrayp x) (listp x)))
                          (haskell-emacs--array-to-list x)
                        x))
          a))

(defun haskell-emacs-format-exports (l)
  (let ((result
         (apply 'concat
                (mapcar
                 (lambda (x)
                   (if x
                       (apply 'concat
                              (mapcar
                               (lambda (y) (concat "(\""y "\",transform "y "),"))
                               x))
                     ""))
                 l))))
    (if (> (length result) 0)
        (substring result 0 -1)
      "")))

(defun haskell-emacs-retrieve-exports (file-name)
  (let ((f-base (file-name-base file-name)))
    (with-temp-buffer
      (insert-file-contents (concat haskell-emacs-dir file-name))
      (when (re-search-forward
             (concat "^module[ \t\n\r]+" f-base
                     "[ \t\n\r]*([ \n\t\r]*\\(\\(?:[^)]\\|[ \n\t\r]\\)+\\)[ \n\t\r]*)")
             nil t)
        (mapcar (lambda (fu) (concat f-base "." (car (last (split-string fu "\\.")))))
                (split-string (match-string 1) "[ \n\r,]+"))))))

(defun haskell-emacs-compile (code import export)
  (with-temp-buffer
    (insert code)
    (goto-char (point-min))
    (when (re-search-forward "---- <<import>> ----" nil t)
      (replace-match import))
    (when (re-search-forward "---- <<export>> ----" nil t)
      (replace-match export))
    (cd haskell-emacs-dir)
    (unless (and (file-exists-p ".HaskellEmacs.hs")
                 (equal (buffer-string)
                        (with-temp-buffer
                          (insert-file-contents ".HaskellEmacs.hs")
                          (buffer-string))))
      (write-file ".HaskellEmacs.hs"))
    (unless (equal 0 (call-process "ghc" nil "*HASKELL-EMACS*"
                                   nil "-O2" "--make" ".HaskellEmacs.hs"))
      (let ((bug (with-current-buffer "*HASKELL-EMACS*" (buffer-string))))
        (kill-buffer "*HASKELL-EMACS*")
        (error bug)))
    (kill-buffer "*HASKELL-EMACS*")))

(provide 'haskell-emacs)

;;; haskell-emacs.el ends here
