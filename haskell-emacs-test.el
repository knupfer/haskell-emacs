;;; haskell-emacs-test.el --- test for haskell-emacs

;;; Commentary:

;; Ensure that you've got a symlinc from HaskellEmacsTest.hs to your
;; `haskell-emacs-dir', reinitialize haskell-emacs and evaluate this
;; buffer.

;;; Code:
(with-no-warnings
  (let ((err)
        (nothing)
        (long)
        (nothingMulti)
        (fuse))
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
    (setq nothing (/ (car (benchmark-run 10000 (HaskellEmacsTest.nothing "")))
                     10000))
    (setq long (/ (car (benchmark-run 1000 (HaskellEmacsTest.longAnswer 13)))
                  (expt 2 13) 1000))
    (setq nothingMulti
          (/ (car (benchmark-run 2000
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
             20000))
    (setq fuse (/ (car (benchmark-run 10000
                         (HaskellEmacsTest.nextNum
                          (+ (HaskellEmacsTest.nextNum
                              (+ (HaskellEmacsTest.multiply
                                  (+ (HaskellEmacsTest.multiply 1 2))
                                  (+ (HaskellEmacsTest.multiply 3 4)))))))))
                  (car (benchmark-run 10000
                         (+ (+ (+ (+ (HaskellEmacsTest.nextNum
                                      (HaskellEmacsTest.nextNum
                                       (HaskellEmacsTest.multiply
                                        (HaskellEmacsTest.multiply 1 2)
                                        (HaskellEmacsTest.multiply 3 4)))))))))))))
  (unless err
    (setq err "No errors were found."))
  (let ((result (concat err "\n\n"
                        "Sync  fun call : " (format "%.1e" nothing) "\n"
                        "Async fun call : " (format "%.1e" nothingMulti) "\n"
                        "Costs per char : " (format "%.1e" long) "\n"
                        "Fusion speed   : " (format "%s" fuse))))
    (display-message-or-buffer result)))

(provide 'haskell-emacs-test)
;;; haskell-emacs-test.el ends here
