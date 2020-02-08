; -*- lexical-binding: t -*-
(require 'test-simple)

(assert-t (load-file "./rena.el") "rena.el not found")

(defun ok (exp string match lastindex)
  (let ((result (funcall exp string 0 0)))
    (cond (result
            (assert-equal match (car result))
            (assert-equal lastindex (cadr result)))
          (t (assert-equal 1 2)))))

(defun ok-attr (exp string match lastindex attr)
  (let ((result (funcall exp string 0 0)))
    (cond (result
            (assert-equal match (car result))
            (assert-equal lastindex (cadr result))
            (assert-equal attr (caddr result)))
          (t (assert-equal 1 2)))))

(defun ng (exp string)
  (let ((result (funcall exp string 0 0)))
    (cond (result (assert-equal 1 2)))))

(let ((r (rena))
      (r1 (rena '((keys "+" "++" "-"))))
      (r2 (rena '((ignore . " "))))
      (r3 (rena '((ignore . " ") (keys "+" "++" "-")))))
  (test-simple-start)

  ;; simple string
  (ok (funcall r "765") "765pro" "765" 3)
  (ng (funcall r "765") "961pro")

  ;; simple regex
  (ok (funcall r 're "[0-9]+") "765pro" "765" 3)
  (ok (funcall r 're "[0-9]+") "346pro" "346" 3)
  (ng (funcall r 're "[0-9]+") "aaa")

  ;; concat
  (ok (funcall r 'concat "765" "pro") "765pro" "765pro" 6)
  (ng (funcall r 'concat "765" "pro") "961pro")
  (ng (funcall r 'concat "765" "pro") "765aaa")
  (ng (funcall r 'concat "765" "pro") "")

  ;; choice
  (ok (funcall r 'choice "765" "346") "765" "765" 3)
  (ok (funcall r 'choice "765" "346") "346" "346" 3)
  (ng (funcall r 'choice "765" "346") "961")

  ;; one-or-more
  (ok (funcall r 'one-or-more "a") "aaab" "aaa" 3)
  (ok (funcall r 'one-or-more "a") "a" "a" 1)
  (ng (funcall r 'one-or-more "a") "")

  ;; zero-or-more
  (ok (funcall r 'zero-or-more "a") "aaab" "aaa" 3)
  (ok (funcall r 'zero-or-more "a") "a" "a" 1)
  (ok (funcall r 'zero-or-more "a") "" "" 0)

  ;; lookahead
  (ok (funcall r 'lookahead "765") "765" "" 0)
  (ng (funcall r 'lookahead "765") "961")

  ;; lookahead-not
  (ok (funcall r 'lookahead-not "961") "765" "" 0)
  (ng (funcall r 'lookahead-not "961") "961")

  ;; attr
  (ok-attr (funcall r 'attr 27) "" "" 0 27)

  ;; cond
  (ok (funcall r 'cond (lambda (x) (= x 0))) "" "" 0)
  (ng (funcall r 'cond (lambda (x) (> x 0))) "")

  ;; key
  (ok (funcall r1 'key "+") "+" "+" 1)
  (ng (funcall r1 'key "+") "++")

  ;; not-key
  (ok (funcall r1 'not-key) "/" "" 0)
  (ng (funcall r1 'not-key) "+")
  (ng (funcall r1 'not-key) "++")
  (ng (funcall r1 'not-key) "-")

  ;; equals-id
  (ok (funcall r 'equals-id "key") "key" "key" 3)
  (ok (funcall r 'equals-id "key") "key " "key" 3)
  (ok (funcall r 'equals-id "key") "key++" "key" 3)
  (ok (funcall r 'equals-id "key") "keys" "key" 3)
  (ok (funcall r1 'equals-id "key") "key" "key" 3)
  (ng (funcall r1 'equals-id "key") "key ")
  (ok (funcall r1 'equals-id "key") "key++" "key" 3)
  (ng (funcall r1 'equals-id "key") "keys")
  (ok (funcall r2 'equals-id "key") "key" "key" 3)
  (ok (funcall r2 'equals-id "key") "key " "key" 3)
  (ng (funcall r2 'equals-id "key") "key++")
  (ng (funcall r2 'equals-id "key") "keys")
  (ok (funcall r3 'equals-id "key") "key" "key" 3)
  (ok (funcall r3 'equals-id "key") "key " "key" 3)
  (ok (funcall r3 'equals-id "key") "key++" "key" 3)
  (ng (funcall r3 'equals-id "key") "keys")

  ;; is-end
  (ok (funcall r 'is-end) "" "" 0)
  (ng (funcall r 'is-end) "961")

  ;; real
  (ok-attr (funcall r 'real) "0" "0" 1 0)
  (ok-attr (funcall r 'real) "765" "765" 3 765)
  (ok-attr (funcall r 'real) "76.5" "76.5" 4 76.5)
  (ok-attr (funcall r 'real) "0.765" "0.765" 5 0.765)
  (ok-attr (funcall r 'real) ".765" ".765" 4 0.765)
  (ok-attr (funcall r 'real) "765e2" "765e2" 5 765e2)
  (ok-attr (funcall r 'real) "765E2" "765E2" 5 765E2)
  (ok-attr (funcall r 'real) "765e+2" "765e+2" 6 765e+2)
  (ok-attr (funcall r 'real) "765e-2" "765e-2" 6 765e-2)
  (ok-attr (funcall r 'real) "+765" "+765" 4 765)
  (ok-attr (funcall r 'real) "+76.5" "+76.5" 5 76.5)
  (ok-attr (funcall r 'real) "+0.765" "+0.765" 6 0.765)
  (ok-attr (funcall r 'real) "+.765" "+.765" 5 0.765)
  (ok-attr (funcall r 'real) "+765e2" "+765e2" 6 765e2)
  (ok-attr (funcall r 'real) "+765E2" "+765E2" 6 765E2)
  (ok-attr (funcall r 'real) "+765e+2" "+765e+2" 7 765e+2)
  (ok-attr (funcall r 'real) "+765e-2" "+765e-2" 7 765e-2)
  (ok-attr (funcall r 'real) "-765" "-765" 4 -765)
  (ok-attr (funcall r 'real) "-76.5" "-76.5" 5 -76.5)
  (ok-attr (funcall r 'real) "-0.765" "-0.765" 6 -0.765)
  (ok-attr (funcall r 'real) "-.765" "-.765" 5 -0.765)
  (ok-attr (funcall r 'real) "-765e2" "-765e2" 6 -765e2)
  (ok-attr (funcall r 'real) "-765E2" "-765E2" 6 -765E2)
  (ok-attr (funcall r 'real) "-765e+2" "-765e+2" 7 -765e+2)
  (ok-attr (funcall r 'real) "-765e-2" "-765e-2" 7 -765e-2)
  (ng (funcall r 'real) "id")
  (ng (funcall r 'real) "")

  ;; letrec
  (setq a (funcall r 'y
                   (lambda (x)
                     (funcall r
                              'concat
                              "("
                              (funcall r 'opt x)
                              ")"))))
  (setq b (funcall r 'y
                   (lambda (x y)
                     (funcall r
                              'concat
                              "("
                              (funcall r 'opt y)
                              ")"))
                   (lambda (x y)
                     (funcall r
                              'concat
                              "["
                              (funcall r 'opt x)
                              "]"))))
  (ok a "((()))" "((()))" 6)
  (ok a "((())))" "((()))" 6)
  (ng a "((())")
  (ok b "([([])])" "([([])])" 8)
  (ok b "([([])])]" "([([])])" 8)
  (ng b "(())")

  (end-tests))
