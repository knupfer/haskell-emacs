;;; haskell-emacs-text.el --- Haskell functions from Data.Text

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
;; Package-Requires: ((haskell-emacs "2.4.0"))
;; Email: fknupfer@gmail.com
;; Keywords: haskell, emacs, ffi
;; URL: https://github.com/knupfer/haskell-emacs/modules/text

;;; Commentary:

;; haskell-emacs-text.el provides nearly all haskell functions from
;; Data.Text.  It uses `haskell-emacs' to register these functions.

;; If you haven't installed this package via melpa, then add the path
;; to this package to your `load-path' (for example in your .emacs).
;; Afterwards run M-x haskell-emacs-init.

;; (Text.tails "EMACS")
;;   => ("EMACS" "MACS" "ACS" "CS" "S" "")

;; If you want to use these functions in your library, put there the
;; following:

;; (require 'haskell-emacs-text)
;; (eval-when-compile (haskell-emacs-init))

;; See documentation for `haskell-emacs-init' for more info.

;;; Code:

(require 'haskell-emacs)
(provide 'haskell-emacs-text)

;;; haskell-emacs-text.el ends here
