

(import (chicken condition))

(condition->list (handle-exceptions exn
		   exn
		   (= 1 "hello world")))

(condition->list (handle-exceptions exn
		   exn
		   (error "bad argument type - not a symbol" 'foo)))

(handle-exceptions exn
  exn
  (abort (make-property-condition 'exn 'message "foobar")))




(define-syntax my-cond
  (syntax-rules (else)
    ((_) (if #f 'not-reached))
    ((_ (else . body))
     (begin . body))
    ((_ (bool-expression . body) . rest)
     (if bool-expression
	 (begin . body)
	 (cond . rest)))))


(define-syntax try
  (syntax-rules (catch)
    ((_ thunk (catch var . handle-body))
     (handle-exceptions var
       (begin handle-body)
       (begin thunk)))))

(try
 (begin (= 1 "hello world"))
 catch exn
 exn)





(handle-exceptions exn
  (begin
    (display "Went wrong")
    (newline))
  (car '()))
					; displays "Went wrong"

(handle-exceptions exn
  (cond
   ((eq? exn 'one) 1)
   (else (abort exn)))
  (case (random 3)
    [(0) 'zero]
    [(1) (abort 'one)]
    [else (abort "Something else")]))
					;=> 'zero, 1, or (abort "Something else")



(define (try-car v)
  (let ((orig (current-exception-handler)))
    (with-exception-handler
	(lambda (exn)
	  (orig (make-composite-condition
		 (make-property-condition
		  'not-a-pair
		  'value
		  v)
		 exn)))
      (lambda () (car v)))))

(try-car '(1))
					;=> 1

(handle-exceptions exn
  (if ((condition-predicate 'not-a-pair) exn)
      (begin
	(display "Not a pair: ")
	(display
	 ((condition-property-accessor 'not-a-pair 'value) exn))
	(newline))
      (abort exn))
  (try-car 0))
					; displays "Not a pair: 0"

(let* ((cs-key (list 'color-scheme))
       (bg-key (list 'background))
       (color-scheme? (condition-predicate cs-key))
       (color-scheme-background
	(condition-property-accessor cs-key bg-key))
       (condition1 (make-property-condition cs-key bg-key 'green))
       (condition2 (make-property-condition cs-key bg-key 'blue))
       (condition3 (make-composite-condition condition1 condition2)))
  (and (color-scheme? condition1)
     (color-scheme? condition2)
     (color-scheme? condition3)
     (color-scheme-background condition3)))
					; => 'green or 'blue
