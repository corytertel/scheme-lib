
(load "/home/cory/Code/Scheme/lib/lib.scm")

#|
The goal is to reduce mental burden.
|#

#|
Object-oriented programming to me means only messaging, local retention and protection
and hiding of state-process, and extreme late-binding of all things.
|#

#|
NO INHERITANCE.
It is possible to achieve everything without any inheritance, whether that be in the form of class
inheritance, abstract classes, interfaces, or traits.
You can achieve polymorphism through message dispatching. No inheritance required.
Inheritance over-emphasizes classes. Classes are not more important than messaging.
Inheritance trees can often over-complicate the project instead of accomplishing the task at hand.
Inheritance has no way of semantically forcing a child to resemble a parent in structure, actions,
or usage.
|#

#|
Dispatching (messaging) and error handling are the main goals.
|#

#|
TODO
Figure out encapsulation with multiple dispatching.
Is it even possible with multiple dispatching? Or is it only possible with single dispatch?

TODO
Figure out how to put the code where the data is,
or at the very least something similar enough.
You shouldn't be taking data in and out.
|#

#|
TODO
Bake in proven design patterns/principles that reoccur, like Gang of Four patterns, into the
language itself to make life simpler.
You abstract functions from hand-rolled for-loops to functions that execute common algorithmic
patterns (like map, filter, reduce) because they exist so much and they can be more powerful and
easier to write when abstracted, so why not do the same with program organization/architecture?

Patterns can introduce so much extra work and confusion that they become hard to use.
So why not bake them in as a keyword?

Emphasize the biological model rather than the command and control calculator style model.
|#

;;;; Objects

;; the type is a pair containing predicate to test if the type is true and information for the make message
;; non-primitive object type check will check for a symbol in car of the type
;; constructor must take in a variable number of args and it returns a new object of the type

(define (make type . args)
  (apply type args))

#|
object defintion takes a function that is a constructor
it returns a new object of the type

Constructor should return the data of the new object.
If there is no explicit predicate passed in, the data returned from the constructor
will be wrapped in a pair of the form (<type> . data) to distinguish it from
other types. If you don't want the resulting object to be of form (<type> . data),
then explicitly pass in a predicate that will be able to check if something is of that type.

When adding a dispatch to a predicate, you have to know that all of the functions that can
be used with the existing definition may be used with the new definition and an object passing
the new definition cannot work with an existing function, then an error will be thrown.

Errors should be expected at every step.
|#
;; TODO custom printer
;; (define-syntax define-object
;;   (er-macro-transformer
;;    (lambda (x r c)
;;      (let ((type (cadr x))
;; 	   (constructor (caddr x)))
;;        `(,(r 'begin)
;; 	 (,(r 'define) ,(symbol-append '< type '>)
;; 	  (,(r 'lambda) ,(r 'args)
;; 	   (,(r 'cons)
;; 	    (,(r 'quote) ,(symbol-append '< type '>))
;; 	    (,(r 'apply) ,constructor ,(r 'args)))))
;; 	 (,(r 'define) ,(symbol-append type '?)
;; 	  (,(r 'lambda) (,(r 'x))
;; 	   (,(r 'and) (,(r 'pair?) ,(r 'x))
;; 	    (,(r 'eq?) (,(r 'quote) ,type) (,(r 'car) ,(r 'x)))))))))))
;; For now, the class structure will be manually created
;; This is until the predicate typing is fully worked out.
;; The goal is to keep this as primitive as possible until all the bugs are worked out.
;; So for now, you'll have to do manual tagging if you want it.
;; You might not even need manual tagging for many objects.
;; It is up to the user as to how they want to structure the object.
;; Maybe it's a record. Maybe it's a list.
(define-syntax define-object
  (er-macro-transformer
   (lambda (x r c)
     (let ((type (cadr x))
	   (constructor (caddr x))
	   (pred (cadddr x))
	   (%begin (r 'begin))
	   (%define (r 'define)))
       `(,%begin
	 (,%define ,(symbol-append '< type '>)
		   ,constructor)
	 (,%define ,(symbol-append type '?)
		   ,pred))))))

;; A primitive object is an object that already has a predicate made for it.
;; (define-syntax define-primitive-object
;;   (er-macro-transformer
;;    (lambda (x r c)
;;      (let ((type (cadr x)))
;;        `(,(r 'define) ,(symbol-append '< type '>)
;; 	 (,(r 'lambda) (,(r 'x))
;; 	  (,(r 'if) (,(symbol-append type '?) ,(r 'x))
;; 	   ,(r 'x)
;; 	   (,(r 'error) (,(r 'string-append) "Not a " (,(r 'symbol->string) ,type)) ,(r 'x)))))))))
(define-syntax define-primitive-object
  (er-macro-transformer
   (lambda (x r c)
     (let ((type (cadr x))
	   (constructor (caddr x)))
       `(,(r 'define) ,(symbol-append '< type '>)
	 ,constructor)))))

;; Add current type definition to another type
;; aka, define a parent predicate
(define-syntax make-parent-predicate
  (syntax-rules ()
    ((_ parent child)
     (define parent
       (let ((old? parent))
	 (lambda (x)
	   (or (child x) (old? x))))))))

#|
the goal is to make everything seem like it is naturally an object
and those "objects" that you can create simply (without the make constructor),
aka the types that are built into scheme, are just easier ways to make those objects
and that you can use make, but not using it seems like syntax sugar
|#

;;;; Generic Messages

;; https://stackoverflow.com/questions/38820187/how-to-call-other-macros-from-a-chicken-scheme-macro
;; TODO have the macro check for boundness at macro expansion time, not at function-call time

(begin-for-syntax
 (import srfi-1)

 (define (dotted-alist? lst)
   (cond ((null? lst) #f)
	 ((not (list? (car lst))) #t)
	 (else (dotted-alist? (cdr lst)))))

 (define (dotted-alist-car lst)
   (cond ((null? lst) '())
	 ((not (list? (car lst))) '())
	 (else (cons (car lst) (dotted-alist-car (cdr lst))))))

 (define (dotted-alist-cdr lst)
   (cond ((null? lst) '())
	 ((not (list? (car lst))) lst)
	 (else (dotted-alist-cdr (cdr lst))))))

#|
This is used to define a function for a specific type signature.
It handles different signatures through dispatching.

Each arg is (type param-name) or (param-name) if there is no type restriction.

Of the form:
(define-message (map (<procedure> fn) (<vector> v))
(vector-map fn v))

Can accept variable number of args:
(define-message (map (<procedure> fn) (<vector> vector1) . (<vector> vectors))
(apply vector-map (cons fn (cons vector1 vectors))))


*Guiding advice:*
You should only allow yourself to look into and/or modify the structure of one object at a time.
Because encapsulation does not exist (for now), we have to enforce the rules of encapsulation
ourselves.

Never modify or access something directly. Always access and modify data through the appropriate
functions. Only modify or access data directly at it's lowest level.
|#
;; TODO clean up
;; NOTE there may be an issue with dispatching over functions with different number of args
;; TODO error handling if dispatch doesn't exist
(define-syntax define-message
  (ir-macro-transformer
   (lambda (exp inject compare)
     (let ((name (caadr exp))
	   (params (cdadr exp))
	   (body (cddr exp)))
       `(define ,name
	  (let ((old-proc
		 (or (call/cc
		     (lambda (k)
		       (with-exception-handler
			   (lambda (x) (k #f))
			 (lambda () ,name))))
		    (lambda args
		      (error "No message signatures match the given arguments")))))
	    (lambda ,(let ((f (lambda (x) (if (= (length x) 1) (car x) (cadr x)))))
		  (if (dotted-alist? params)
		      (append (map f (dotted-alist-car params))
			      (f (dotted-alist-cdr params)))
		      (map f params)))
	      (if (and ,@(if (dotted-alist? params)
			   (append (filter (lambda (x) (not (= (length x) 1)))
					   (dotted-alist-car params))
				   (if (= (length (dotted-alist-cdr params)) 1)
				       '()
				       (list (cons 'andmap (dotted-alist-cdr params)))))
			   (filter (lambda (x) (not (= (length x) 1))) params)))
		  (begin
		    ,@body)
		  ,(let ((f (lambda (x) (if (= (length x) 1) (car x) (cadr x)))))
		     (if (dotted-alist? params)
			 `(apply old-proc (append (list ,@(map f (dotted-alist-car params)))
						  ,(f (dotted-alist-cdr params))))
			 `(old-proc ,@(map f params))))))))))))
