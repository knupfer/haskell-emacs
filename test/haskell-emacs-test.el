;;; haskell-emacs-test.el --- test for haskell-emacs

;;; Commentary:

;; To execute this test-suite and benchmark-suite:

;; emacs -Q --batch -l haskell-emacs-test.el

;; Don't call this in another way.

;;; Code:

(let ((load-dir (file-name-directory load-file-name)))
  (add-to-list 'load-path load-dir)
  (add-to-list 'load-path (progn (string-match ".*haskell-emacs/" load-dir)
                                 (match-string 0 load-dir)))
  (setq haskell-emacs-dir
        (concat load-dir
                (format "out%s/"
                        (+ 1 (length (directory-files load-dir
                                                      nil
                                                      "^out[0-9]+$")))))))

(require 'haskell-emacs)

(defalias 'yes-or-no-p (lambda (x) t))
(haskell-emacs-init 'install)

(let ((err)
      (nothing)
      (long)
      (nothingMulti)
      (fuse)
      (serial)
      (parallel)
      (num 5)
      (txt "test")
      (emptyM)
      (emacsEval)
      (emacsEval_)
      (now (current-time)))
  (mapc
   (lambda (x)
     (unless (equal (eval (car x)) (cadr x))
       (setq err (concat err (format "%s" (car x)) "\n"
                         "    results in: " (format "%s" (eval (car x))) "\n"
                         "    instead of: " (format "%s" (cadr x)) "\n"))))
   '(((HaskellEmacsTest.nothing "a") "")
     ((HaskellEmacsTest.unicode) "ˈiːmæksإيماكسایمکس이맥스И́макс")
     ((HaskellEmacsTest.unicodeText) "ˈiːmæksإيماكسایمکس이맥스И́макс")
     ((HaskellEmacsTest.constantText) "test")
     ((HaskellEmacsTest.concatString '("a" "bc" "ABC")) "abcABC")
     ((HaskellEmacsTest.concatText '("a" "bc" "ABC")) "abcABC")
     ((HaskellEmacsTest.constantString) "test")
     ((HaskellEmacsTest.constantTrue) t)
     ((HaskellEmacsTest.constantFalse) nil)
     ((HaskellEmacsTest.notBool nil) t)
     ((HaskellEmacsTest.notBool t) nil)
     ((HaskellEmacsTest.allTrue '(t t t)) t)
     ((HaskellEmacsTest.allTrue '(t nil t)) nil)
     ((HaskellEmacsTest.anyTrue '(nil nil nil)) nil)
     ((HaskellEmacsTest.anyTrue '(nil nil t)) t)
     ((HaskellEmacsTest.nextNum 7) 8)
     ((HaskellEmacsTest.nextNum num) 6)
     ((HaskellEmacsTest.nothing txt) "")
     ((HaskellEmacsTest.summation '(1 2 3 10)) 16)
     ((HaskellEmacsTest.constant) 10.5)
     ((HaskellEmacsTest.squareRoot 4) 2.0)
     ((HaskellEmacsTest.squareRoot -4) NaN)
     ((HaskellEmacsTest.bothTrue nil t) nil)
     ((HaskellEmacsTest.bothTrue t t) t)
     ((HaskellEmacsTest.multiply 4 5) 20.0)
     ((HaskellEmacsTest.takeSome 4 "abcde") "abcd")
     ((HaskellEmacsTest.switch '("abc" 4)) (4 "abc"))
     ((HaskellEmacsTest.concatFst '(("a" 1) ("b" 2) ("c" 3))) "abc")
     ((HaskellEmacsTest.longAnswer 2) "aaaa")
     ((HaskellEmacsTest.nthFib 10) 55)
;;;; Supported types,  look at #38
     ((HaskellEmacsTest.nextNum 7) 8)
     ((HaskellEmacsTest.multiply 4.5 2.0) 9.0)
     ((HaskellEmacsTest.nextChar "a") "b")
     ((HaskellEmacsTest.symbolReverse 'abcd) dcba)
     ((HaskellEmacsTest.summation [1 2 3]) 6)
     ((HaskellEmacsTest.allTrue (make-bool-vector 3 t)) t)
     ((HaskellEmacsTest.summation (let ((r (make-ring 5)))
                                    (ring-insert r 1)
                                    (ring-insert r 2)
                                    r)) 3)
     ((HaskellEmacsTest.concatFst (let ((h (make-hash-table)))
                                    (puthash "c" 12 h)
                                    (puthash "b" 17 h)
                                    (puthash "a" 22 h)
                                    h)) "abc")
;;;;
     ((mapcar 'eval (list (HaskellEmacsTest.multiply-async 2 4)
                          (HaskellEmacsTest.multiply-async 1 9)
                          (HaskellEmacsTest.multiply-async 10 15)))
      (8.0 9.0 150.0))))
  (if err (error err)
    (message "No errors were found.\n"))
  (setq nothing (/ (car (benchmark-run 15000 (HaskellEmacsTest.nothing "")))
                   15000))
  (message (concat "Sync  fun call : "
                   (format "%.1e" nothing)))
  (setq nothingMulti
        (/ (car (benchmark-run 3000
                  (mapc 'eval (list (HaskellEmacsTest.nothing-async "a")
                                    (HaskellEmacsTest.nothing-async "a")
                                    (HaskellEmacsTest.nothing-async "a")
                                    (HaskellEmacsTest.nothing-async "a")
                                    (HaskellEmacsTest.nothing-async "a")
                                    ;;
                                    (HaskellEmacsTest.nothing-async "a")
                                    (HaskellEmacsTest.nothing-async "a")
                                    (HaskellEmacsTest.nothing-async "a")
                                    (HaskellEmacsTest.nothing-async "a")
                                    (HaskellEmacsTest.nothing-async "a")))))
           30000))
  (message (concat "Async fun call : "
                   (format "%.1e" nothingMulti)))
  (setq long (/ (car (benchmark-run 1000 (HaskellEmacsTest.longAnswer 13)))
                (expt 2 13) 1000))
  (message (concat "Costs per char : "
                   (format "%.1e" long)))
  (setq emptyM (/ (car (benchmark-run 20000
                         (HaskellEmacsTest.emptyEmacsMonad)))
                  20000))
  (message (concat "Do emacs monad : "
                   (format "%.1e" emptyM)))
  (setq emacsEval (/ (car (benchmark-run 1
                            (HaskellEmacsTest.emacsMonad 20000)))
                     20000))
  (message (concat "Costs per eval : "
                   (format "%.1e" emacsEval)))
  (setq emacsEval_ (/ (car (benchmark-run 1
                             (HaskellEmacsTest.emacsMonad 20000)))
                      20000))
  (message (concat "Costs per eval_: "
                   (format "%.1e" emacsEval_)))
  (setq serial (car (benchmark-run 8
                      (HaskellEmacsTest.doNBody 5000000))))
  (message (concat "Sync  workload : "
                   (format "%.2f" serial)))
  (setq parallel
        (car (benchmark-run 1
               (mapcar 'eval (list
                              (HaskellEmacsTest.doNBody-async 5000000)
                              (HaskellEmacsTest.doNBody-async 5000000)
                              (HaskellEmacsTest.doNBody-async 5000000)
                              (HaskellEmacsTest.doNBody-async 5000000)
                              ;;
                              (HaskellEmacsTest.doNBody-async 5000000)
                              (HaskellEmacsTest.doNBody-async 5000000)
                              (HaskellEmacsTest.doNBody-async 5000000)
                              (HaskellEmacsTest.doNBody-async 5000000))))))
  (message (concat "Parallel speed : "
                   (format "%.2f" parallel)
                   (format " (x%.2f)" (/ serial parallel))))
  (setq fuse
        (car (benchmark-run 1
               (HaskellEmacsTest.multiply
                (HaskellEmacsTest.multiply
                 (HaskellEmacsTest.multiply
                  (HaskellEmacsTest.doNBody 5000000)
                  (HaskellEmacsTest.doNBody 5000000))
                 (HaskellEmacsTest.multiply
                  (HaskellEmacsTest.doNBody 5000000)
                  (HaskellEmacsTest.doNBody 5000000)))
                (HaskellEmacsTest.multiply
                 (HaskellEmacsTest.multiply
                  (HaskellEmacsTest.doNBody 5000000)
                  (HaskellEmacsTest.doNBody 5000000))
                 (HaskellEmacsTest.multiply
                  (HaskellEmacsTest.doNBody 5000000)
                  (HaskellEmacsTest.doNBody 5000000)))))))
  (message (concat "Nesting  speed : "
                   (format "%.2f" fuse)
                   (format " (x%.2f)" (/ serial fuse))))
  (message (concat "Total duration : "
                   (format "%s" (round (float-time (time-subtract (current-time) now)))))))

;;; haskell-emacs-test.el ends here
