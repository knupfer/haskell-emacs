;;; haskell-emacs-test.el --- test for haskell-emacs

;;; Commentary:

;; To execute this test-suite and benchmark-suite:
;; emacs -Q --batch -l haskell-emacs-test.el
;; emacs -Q --batch -l haskell-emacs-test.el --eval="(haskell-emacs-run-tests)"

;;; Code:

(let ((load-dir (file-name-directory load-file-name)))
  (add-to-list 'load-path load-dir)
  (add-to-list 'load-path (progn (string-match ".*haskell-emacs/" load-dir)
                                 (match-string 0 load-dir))))

(require 'haskell-emacs)

(haskell-emacs-init)

(defun haskell-emacs-run-tests ()
  (let ((err)
        (nothing)
        (long)
        (nothingMulti)
        (fuse)
        (serial)
        (parallel)
        (num 5)
        (txt "test")
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
       ((HaskellEmacsTest.multiply 4 5) 20)
       ((HaskellEmacsTest.takeSome 4 "abcde") "abcd")
       ((HaskellEmacsTest.switch '("abc" 4)) (4 "abc"))
       ((HaskellEmacsTest.concatFst '(("a" 1) ("b" 2) ("c" 3))) "abc")
       ((HaskellEmacsTest.longAnswer 2) "aaaa")
       ((HaskellEmacsTest.nthFib 10) 55)
       ((mapcar 'eval (list (HaskellEmacsTest.multiply-async 2 4)
                            (HaskellEmacsTest.multiply-async 1 9)
                            (HaskellEmacsTest.multiply-async 10 15)))
        (8 9 150))))
    (if err (error err)
        (message "No errors were found.\n"))
    (setq nothing (/ (car (benchmark-run 5000 (HaskellEmacsTest.nothing "")))
                     5000))
    (message "Sync  fun call : " (format "%.1e" nothing))
    (setq nothingMulti
          (/ (car (benchmark-run 1000
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
             10000))
    (message "Async fun call : " (format "%.1e" nothingMulti))
    (setq long (/ (car (benchmark-run 500 (HaskellEmacsTest.longAnswer 13)))
                  (expt 2 13) 500))
    (message "Costs per char : " (format "%.1e" long))
    (setq serial (car (benchmark-run 4
                        (HaskellEmacsTest.doWork 15000000 0 0))))
    (setq parallel
          (car (benchmark-run 1
                 (mapcar 'eval (list
                                (HaskellEmacsTest.doWork-async 15000000 0 0)
                                (HaskellEmacsTest.doWork-async 15000000 0 0)
                                (HaskellEmacsTest.doWork-async 15000000 0 0)
                                (HaskellEmacsTest.doWork-async 15000000 0 0))))))
    (message "Parallel speed : " (format "x%.2f" (/ serial parallel)))
    (setq fuse
          (car (benchmark-run 1
                 (HaskellEmacsTest.multiply
                  (HaskellEmacsTest.multiply
                   (HaskellEmacsTest.doWork 15000000 0 0)
                   (HaskellEmacsTest.doWork 15000000 0 0))
                  (HaskellEmacsTest.multiply
                   (HaskellEmacsTest.doWork 15000000 0 0)
                   (HaskellEmacsTest.doWork 15000000 0 0))))))
    (message "Nesting  speed : " (format "x%.2f" (/ serial fuse)))
    (message "Total duration : " (format "%s" (round (float-time (time-subtract (current-time) now)))))))

(provide 'haskell-emacs-test)
;;; haskell-emacs-test.el ends here
