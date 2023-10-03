;; Here are all the "stdlib" functions that i create and use

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Imports
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (import r7rs)
;; (import (chicken keyword)) ;; maybe unneeded. remove?
(import (chicken condition))
(import srfi-1)
(import srfi-13) ;; string utils
(import records)
(import srfi-63) ;; homogeneous and heterogeneous arrays
(import srfi-69) ;; hash tables
(import srfi-95) ;; sorting and merging
;; (import srfi-113)
(import srfi-133) ;; vector library
;; (import srfi-196) ;; ranges
(import srfi-197) ;; pipeline operator
(import matchable)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Global vars
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; TODO should be constant
(define unspecified)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Make all existing binds OO compatible
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (import coops)
;; (import coops-primitive-objects)
;; (load "/home/cory/Code/Scheme/lib/objects/coops.scm")
;; (load "/home/cory/Code/Scheme/lib/objects/coops-primitive-objects.scm")
;; (load "/home/cory/Code/Scheme/lib/objects/hash-table.scm")

;; TODO create a range primitive object for srfi-196 ranges

;; TODO create a class object for srfi-69 hash tables

;; TODO re-implement objects for builtin types yourself
;; (replacing the need for coops-primitive-objects)

;; TODO create an error class

;; TODO rename existing functions

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Syntax
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Macro for creating aliases
(define-syntax define-alias
  (syntax-rules ()
    ((_ new-name old-name)
     (define-syntax new-name
       (syntax-rules ()
         ((_ . args) (old-name . args)))))))

;;; Imperative C-like control flow syntax for quick translation from a C-like lang into scheme

;; C-style while loop
(define-syntax while
  (syntax-rules ()
    ((_ pred? body ...)
     (do () ((not pred?)) body ...))))

;; C-style for loop
;; (for (i 0) (< i 10) (+ i 1)
;;      (display "Hello World"))
(define-syntax for
  (er-macro-transformer
   (lambda (exp rename compare)
     (let ((init (cadr exp))
	   (pred (caddr exp))
	   (step (cadddr exp))
	   (body (cddddr exp)))
       `(,(rename 'do)
	 ((,@init ,step))
	 ((,(rename 'not) ,pred))
	 ,@body)))))

;; Infinite loop
;; (define-syntax loop
;;   (syntax-rules ()
;;     ((_ body ...)
;;      (do () (#f) body ...))))
(define-syntax loop
  (syntax-rules ()
    ((_ body ...)
     (letrec ((loop (lambda () body ... (loop))))
       (loop)))))

;;; Exception macros

;; Simple C++-like try-block
(define-syntax try
  (syntax-rules ()
    ((_ thunk ...)
     (call/cc
      (lambda (k)
	(with-exception-handler
	    (lambda (x) (k x))
	  (lambda () thunk ...)))))))

;; TODO get a better bound command
;; This may sent false positives do to it only checking if it's a condition
(define-syntax bound?
  (syntax-rules ()
    ((_ var)
     (not (condition? (try var))))))

;; TODO catch block

;; TODO function/macro for something like this
;; (handle-exceptions exn
;;     (begin
;;       (display "Went wrong: ")
;;       (display
;;        ((condition-property-accessor 'exn 'message) exn))
;;       (newline))
;;   (car '()))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; General functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; ;; Merge two sorted lists, order is based on comp
;; (define (merge comp l1 l2)
;;   (cond ((null? l1) l2)
;; 	((null? l2) l1)
;; 	((comp (car l1) (car l2)) (cons (car l1) (merge comp (cdr l1) l2)))
;; 	(else (cons (car l2) (merge comp l1 (cdr l2))))))

;; ;; Returns the given list sorted by comp (uses merge sort)
;; ;; pred is a function that takes 2 args and returns which arg it wants to use
;; (define (sort comp lst)
;;   (cond ((<= (length lst) 1) lst)
;; 	(else (chain (split lst 2)
;; 		  (map (lambda (x) (sort comp x)) _)
;; 		  (cons comp _)
;; 		  (apply merge _)))))

(define (range-iter num step count acc)
  (if (<= count 0)
      acc
      (range-iter (+ num step) step (- count 1) (cons num acc))))

(define range
  (case-lambda
    ((end) (range-iter (- end 1) -1 end '()))
    ((beg end) (range-iter (- end 1) -1 (- end beg) '()))
    ((beg end step) (range-iter (- end step) (- step) (/ (- end beg) step) '()))
    (args (error "bad argument count" args))))

(define range*
  (case-lambda
    ((end) (range-iter end -1 (+ 1 end) '()))
    ((beg end) (range-iter end -1 (+ 1 (- end beg)) '()))
    ((beg end step) (range-iter end (- step) (+ 1 (/ (- end beg) step)) '()))
    (args (error "bad argument count" args))))

;; Takes a num and a list of nums, then returns a list of list of numbers
;; i.e. (sliding 2 '(1 3 3 1)) returns '((1 3) (3 3) (3 1))
;; Useful for getting lists of each element and their "next" values
;; (define (sliding num lst)
;;   (define (sliding-iter num lst count)
;;     (if (<= count 0)
;; 	'()
;; 	(cons (take num lst) (sliding-iter num (cdr lst) (- count 1)))))
;;   (sliding-iter num lst (- (length lst) (sub1 num))))
(define (sliding num lst)
  (define (sliding-iter num lst count acc)
    (if (<= count 0)
	(reverse acc)
	(sliding-iter num
		      (cdr lst)
		      (sub1 count)
		      (cons (take lst num) acc))))
  (sliding-iter num lst (- (length lst) (sub1 num)) '()))

;; Returns a random number between 0 (inclusive) and 1 (exclusive).
(define random
  (let ((a 69069) (c 1) (m (expt 2 32)) (seed 19380110))
    (lambda new-seed
      (if (pair? new-seed)
          (set! seed (car new-seed))
          (set! seed (modulo (+ (* seed a) c) m)))
      (/ seed m))))

;; Shuffles the elements in a list
(define (shuffle lst)
  (define (remove-index lst x)
    ;; This function returns the list with the index x removed, and then reversed.
    ;; This is an optimization for speed and memory.
    ;; This weird behavior does not matter as the end goal is a random shuffle.
    (define (iter lst x acc)
      (cond ((null? lst) acc)
	    ((= x 0) (iter (cdr lst) (- x 1) acc))
	    (else (iter (cdr lst) (- x 1) (cons (car lst) acc)))))
    (iter lst x '()))
  (define (iter lst acc)
    (if (null? lst)
	acc
	(let* ((index (floor (* (random) (length lst))))
	       (item (list-ref lst index)))
	  (iter (remove-index lst index) (cons item acc)))))
  (iter lst '()))

;; To shuffle a list, convert it to a vector, shuffle the vector by Knuth’s algorithm, and convert the result back to a list:
;; (define (shuffle x)
;;   (do ((v (list->vector x)) (n (length x) (- n 1)))
;;       ((zero? n) (vector->list v))
;;     (let* ((r (randint n)) (t (vector-ref v r)))
;;       (vector-set! v r (vector-ref v (- n 1)))
;;       (vector-set! v (- n 1) t))))

;; Combines the results of a map with and
(define (andmap pred lst)
  (letrec ((and-l
	    (lambda x
	      (cond ((null? x) #t)
		    ((car x) (apply and-l (cdr x)))
		    (else #f)))))
    (apply and-l (map pred lst))))

;; Combines the results of a map with or
(define (ormap pred lst)
  (letrec ((or-l
	    (lambda x
	      (cond ((null? x) #f)
		    ((car x) #t)
		    (else (apply or-l (cdr x)))))))
    (apply or-l (map pred lst))))

;; Combines the results of a map with one application of flatten
(define (flatmap proc lst)
  (chain (map proc lst)
	 (join _)))

;; NOTE chop does this in chicken base
;; Split list into evenly sized chunks
;; (define (chunks-of lst size)
;;   (cond ((<= size (length lst)) (cons (take lst size) (chunks-of (drop lst size) size)))
;; 	((null? lst) '())
;; 	(else (list lst))))

;; Sum
(define (sum lst)
  (foldl + 0 lst))

;; Product
(define (product lst)
  (foldl * 1 lst))

;; Returns a new list, combining the matching pairs of each list with fun.
(define (map* proc lsts)
  (chain (apply zip lsts)
	 (map (lambda (l) (apply proc l)) _)))

;; Partitions the list into n parts
(define (split lst n)
  (chop lst (ceiling (/ (length lst) n))))

;; Return the heads of all lists
(define (heads . lsts)
  (filter-map (lambda (x) (if (null? x)
			 #f
			 (car x)))
	      lsts))

;; Swaps the a'th and b'th elements of the list
(define (swap a b lst)
  (cond ((> a b) (swap b a lst))
	((= a b) lst)
	(else (append (take lst a)
		      (list (list-ref lst b))
		      (take (list-tail lst (+ a 1)) (- b a 1))
		      (list (list-ref lst a))
		      (list-tail lst (+ b 1))))))

;; Returns the element given  that the comp function determines most fit
(define (choose comp . elems)
  (foldl (lambda (a b) (if (comp a b) a b))
	 (car elems)
	 (cdr elems)))

;; Returns the last element of a list
;; (define (last lst)
;;   (list-ref lst (- (length lst) 1)))

;; Squares x
(define (square x)
  (* x x))

;; Cubes x
(define (cube x)
  (* x x x))

;; Average
(define (avg . vals)
  (/ (sum vals) (length vals)))

;; Iterates over every value in a data structure applying proc and keeping the structure.
(define (tree-map proc tree)
  (map (lambda (x) (if (list? x)
		  (tree-map proc x)
		  (proc x)))
       tree))

;; Returns a list with every combination of the lists given
(define (cartesian-product . lsts)
  (if (null? (cdr lsts))
      (map list (car lsts))
      (flatmap (lambda (x) (map (lambda (y) (cons y x)) (car lsts)))
	       (apply cartesian-product (cdr lsts)))))

;; Returns #t if the list is in increasing order, #f if not
(define (increasing? lst)
  (apply < lst))

;; Returns #t if the list is in decreasing order, #f if not
(define (decreasing? lst)
  (apply > lst))

;; Returns the given lst without any duplicates
(define (remove-duplicates lst)
  (if (null? lst)
      '()
      ((if (member (car lst) (cdr lst))
	   identity
	   (lambda (x) (cons (car lst) x)))
       (remove-duplicates (cdr lst)))))

;; Return the list with n elements shifted left and wrapped around
(define (rotate-l lst n)
  (append (drop lst n) (take lst n)))

;; Return the list with n elements shifted right and wrapped around
(define (rotate-r lst n)
  (append (take-right lst n) (drop-right lst n)))

;; Returns all the subsequences in a list
(define (subsequences lst)
  (if (null? lst)
      '()
      (let ((res (subsequences (cdr lst))))
	(append (list (list (car lst)))
		(map (lambda (x) (cons (car lst) x)) res)
		res))))

;; Removes consecutive equal elements
(define (unique lst)
  (cond ((null? (cdr lst)) lst)
	((equal? (car lst) (cadr lst))
	 (unique (cdr lst)))
	(else
	 (cons (car lst) (unique (cdr lst))))))

;; Returns a copy of the given data
(define (clone data)
  (let ((copy data)) copy))

;; Removes the element at the given index
(define (list-remove-index lst x)
  (if (= x 0)
      (cdr lst)
      (cons (car lst) (list-remove-index (cdr lst) (- x 1)))))

;; Cross produces the cross product of one or more lists.
;; The implementation is a functional pearl due to Christoper Strachey, collected by Mike Spivey:
(define (cross . xss)
  (define (f xs yss)
    (define (g x zss)
      (define (h ys uss)
        (cons (cons x ys) uss))
      (fold-right h zss yss))
    (fold-right g '() xs))
  (fold-right f (list '()) xss))

;; Tests if a variable is initialized
(define (initialized? var)
  (not (eq? unspecified var)))

;; Tests if a variable is an alist
(define (alist? lst)
  (and (list? lst) (andmap list? lst)))
