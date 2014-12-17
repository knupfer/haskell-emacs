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
(defvar haskell-emacs-process nil)
(defvar haskell-emacs-dir "~/.emacs.d/haskell-fun/")
(defun haskell-emacs-init ()
  (interactive)
  (save-excursion
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
        (setq haskell-emacs-process
              (start-process "hask" nil
                             (concat haskell-emacs-dir ".HaskellEmacs")))
        (set-process-query-on-exit-flag haskell-emacs-process nil)
        (set-process-filter
         haskell-emacs-process (lambda (proc str)
                                 (setq haskell-emacs--response
                                       (concat haskell-emacs--response str))))
        (mapc (lambda (fi)
                (mapc (lambda (fu) (eval (haskell-emacs--wrapper-fun fu)))
                      fi))
              exports)))))

(defun haskell-emacs--wrapper-fun (fun)
  "Take FUN and return a wrapper in elisp."
  `(progn
     (defun ,(intern fun)
         (OBJECT)
       ,(concat
         (with-temp-buffer (concat fun
                                   " is a haskell-function which
receives the input OBJECT.")
                           (let ((fill-column 60))
                             (fill-region (point-min) (point-max)))
                           (buffer-string)))
       (setq haskell-emacs--response nil)
       (process-send-string haskell-emacs-process
                            ,(concat fun "\n"))
       (accept-process-output haskell-emacs-process)
       (if (string-prefix-p "=:OK:=" haskell-emacs--response)
           (setq haskell-emacs--response nil)
         (error haskell-emacs--response))
       (process-send-string haskell-emacs-process
                            (concat (format "%S" OBJECT) "\n"))
       (process-send-string haskell-emacs-process
                            (concat "49e3524a756a100a5cf3d27ede74ea95" "\n"))
       (while (not (string-suffix-p "=:DONE:=" haskell-emacs--response))
         (accept-process-output haskell-emacs-process))
       (when (string-prefix-p "=:FAIL:=" haskell-emacs--response)
         (error (substring haskell-emacs--response 9 -9)))
       (setq haskell-emacs--response (read (substring haskell-emacs--response 9 -9))))
     (byte-compile ',(intern fun))))

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
                     "[ \t\n\r]*(\\(\\(?:[^)]\\|[ \n\t\r]\\)+\\))")
             nil t)
        (mapcar (lambda (fu) (concat f-base "." fu))
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
    (call-process "ghc" nil nil nil "-O2" "--make" ".HaskellEmacs.hs")))

(provide 'haskell-emacs)

;;; haskell-emacs.el ends here
