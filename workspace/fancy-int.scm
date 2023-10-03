
;; scoped module testing
(module test-module (test-function)
(import scheme)
(define (test-function) 'hello-world))

(define (test-function2)
  (import test-module)
  (test-function))

(test-function2)
(test-function)




(define-object fancy-int
  (lambda args
    (if (andmap string? args)
	(cons '<fancy-int> args)
	(error "Args are not all strings")))
  (lambda (x)
    (and (pair? x) (eq? '<fancy-int> (car x)))))

(define-message (data (fancy-int? x))
  (cdr x))

(define-message (->int (fancy-int? x))
  (let ((int-representation
	 (lambda (str)
	   (cond ((string-ci=? "one"   str) 1)
		 ((string-ci=? "two"   str) 2)
		 ((string-ci=? "three" str) 3)
		 ((string-ci=? "four"  str) 4)
		 ((string-ci=? "five"  str) 5)
		 ((string-ci=? "six"   str) 6)
		 ((string-ci=? "seven" str) 7)
		 ((string-ci=? "eight" str) 8)
		 ((string-ci=? "nine"  str) 9)
		 ((string-ci=? "zero"  str) 0)
		 (else (error "Not a vaild name of int"))))))
    (chain (map int-representation (data x))
	(foldl (lambda (x y) (+ (* 10 x) y)) 0 _))))

(define-message (->fancy-int (integer? x))
  (let ((string-name
	 (lambda (x)
	   (cond ((= x 1) "one")
		 ((= x 2) "two")
		 ((= x 3) "three")
		 ((= x 4) "four")
		 ((= x 5) "five")
		 ((= x 6) "six")
		 ((= x 7) "seven")
		 ((= x 8) "eight")
		 ((= x 9) "nine")
		 ((= x 0) "zero")
		 (else (error "Not a vaild string from int"))))))
    (chain (unfold-right (lambda (x) (= x 0))
		      (lambda (x) (modulo x 10))
		      (lambda (x) (floor (/ x 10)))
		      x)
	(map string-name _)
	(cons <fancy-int> _)
	(apply make _))))

(define-message (+ (fancy-int? n1) . (fancy-int? ns))
  (chain (cons n1 ns)
      (map ->int _)
      (foldr + 0 _)
      (->fancy-int _)))

(make-parent-predicate integer? fancy-int?)




(integer? 123)
(integer? (make <fancy-int> "one" "two" "three"))
(fancy-int? 123)
(fancy-int? (make <fancy-int> "one" "two" "three"))
(+ 123 456 789)
(+ (make <fancy-int> "one" "two" "three")
   (make <fancy-int> "four" "five" "six")
   (make <fancy-int> "seven" "eight" "nine"))


;; Show that anything that works with integer? works when an int
;; is expected if the correct messages are implemented

;; Double works because + is implemented over fancy-int
(define-message (double (integer? x))
  (+ x x))
(double 123)
(double (make <fancy-int> "one" "two" "three"))

;; Mult does not because =, even?, -, and / do not have messages with fancy-int in their signatures
(define-message (mult (integer? a) (integer? b))
  (cond ((= b 0) 0)
	((even? b) (double (mult a (/ b 2))))
	(else (+ a (mult a (- b 1))))))
(mult 123 456)
(mult (make <fancy-int> "one" "two" "three") (make <fancy-int> "four" "five" "six"))
