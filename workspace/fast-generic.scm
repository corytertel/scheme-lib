
(import srfi-1)
(import srfi-133)
(import fast-generic)

(define-type <list> list?)
(define-type <vector> vector?)
(define-type <procedure> procedure?)

(define-generic (ref (<list> lst) i) (list-ref lst i))
(define-generic (ref (<vector> vec) i) (vector-ref vec i))

(ref '(a b c) 1)
(ref '#(99 100 101) 2)

;; DOESN'T WORK WITH ALREADY DEFINED TYPES
;; (define-generic (map (<procedure> proc) v)
;;   (vector-map proc v))

(define-generic (vmap (<procedure> proc) v)
  (vector-map proc v))

(vmap add1 #(1 2 3))

(define (odd-sized-vector? x)
  (and (vector? x) (odd? (vector-length x))))

(define-type <odd-sized-vector> odd-sized-vector? <vector>)

(define-generic (pairwise (<vector> v))
  (chop (vector->list v) 2))

(define-generic (pairwise (<odd-sized-vector> v))
  (error "I'm sorry, Cory, but I can't do that."))

(pairwise #(1 2 3 4))
(pairwise #(1 2 3))
