
;;;; Object definitions for primitives

#|
Makes built-in types usable as objects in constructors.
This just makes everything more consistent, but it's not necessary for the system to run.
|#

;; Scheme Base
;; TODO covert these errors to type "type"
(define-primitive-object symbol    (lambda (x) (if (symbol? x) x (error "bad argument type - not a symbol" x))))
(define-primitive-object procedure (lambda (x) (if (procedure? x) x (error "bad argument type - not a procedure" x))))
(define-primitive-object number    (lambda (x) (if (number? x) x (error "bad argument type - not a number" x))))
(define-primitive-object complex   (lambda (x) (if (complex? x) x (error "bad argument type - not a complex" x))))
(define-primitive-object real      (lambda (x) (if (real? x) x (error "bad argument type - not a real" x))))
(define-primitive-object rational  (lambda (x) (if (rational? x) x (error "bad argument type - not a rational" x))))
(define-primitive-object integer   (lambda (x) (if (integer? x) x (error "bad argument type - not a integer" x))))
(define-primitive-object exact     (lambda (x) (if (exact? x) x (error "bad argument type - not an exact" x))))
(define-primitive-object inexact   (lambda (x) (if (inexact? x) x (error "bad argument type - not an inexact" x))))
(define-primitive-object string    (lambda (x) (if (string? x) x (error "bad argument type - not a string" x))))
(define-primitive-object boolean   (lambda (x) (if (boolean? x) x (error "bad argument type - not a boolean" x))))
(define-primitive-object pair      (lambda (x) (if (pair? x) x (error "bad argument type - not a pair" x))))
(define-primitive-object list      (lambda (x) (if (list? x) x (error "bad argument type - not a list" x))))
(define-primitive-object char      (lambda (x) (if (char? x) x (error "bad argument type - not a char" x))))
(define-primitive-object vector    (lambda (x) (if (vector? x) x (error "bad argument type - not a vector" x))))
;; (define-primitive-object port)
;; (define-primitive-object eof-object)
;; (define-primitive-object promise make-promise)
;; Chicken Base
(define-primitive-object alist     (lambda (x) (if (alist? x) x (error "bad argument type - not an alist" x))))
(define-primitive-object atom      (lambda (x) (if (atom? x) x (error "bad argument type - not an atom" x))))
;; (define-primitive-object record)
;; Chickes Condition
(define-primitive-object condition make-property-condition)

;;;; Redefine built-in functions that should be dispatched

(define-message (map (procedure? fn) (vector? v))
  (vector-map fn v))
