
(import scheme)
(import (chicken time))
(import srfi-69)

;; (define (fib n)
;;   (cond ((= n 0) 0)
;; 	((= n 1) 1)
;; 	(else (+ (fib (- n 1)) (fib (- n 2))))))

;; (time (fib 40))

(define fib
  (let ((ht (alist->hash-table '((0 . 0) (1 . 1)))))
    (lambda (n)
      (if (hash-table-exists? ht n)
	  (hash-table-ref ht n)
	  (let ((m (+ (fib (- n 1)) (fib (- n 2)))))
	    (hash-table-set! ht n m)
	    m)))))

(define answer)
(time (set! answer (fib 100000)))
(display answer)
(newline)

;; TODO big numbers, aka bignum
