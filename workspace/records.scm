

(import scheme)
(import (chicken base))
(import (chicken format))


(define)



(define-record point x y)
(define p1 (make-point 123 456))
(point? p1)
(point-x p1)
(point-y-set! p1 99)
(point-y p1)

p1

(record-printer 'point)

(fprintf (current-output-port) "#<object `~a'>" 'point)



(##sys#register-record-printer
 'hash-table
 (lambda (ht port)
   (##sys#print "#<hash-table (" #f port)
   (##sys#print (##sys#slot ht 2) #f port)
   (##sys#print ")>" #f port) ) )
