#|
This file exists to port the hash table library in SRFI-69 to an object system for a consistent
object-oriented scheme environment.
|#

;; TODO make this file import srfi-69 into a module, so it is not globally scoped.
;; then buff out the object definition of <hash-table> more to make it completely
;; representative of the usage of srfi-69

;; TODO SRFI-90 constructor compliance
(define-object hash-table
  (case-lambda
    (() (make-hash-table))
    (args (alist->hash-table args)))
  hash-table?)

(define-message (size (<hash-table> ht))
  (hash-table-size ht))

(define-message (equivalence-function (<hash-table> ht))
  (hash-table-equivalence-function ht))

(define-message (hash-function (<hash-table> ht))
  (hash-table-hash-function ht))

(define-message (min-load (<hash-table> ht))
  (hash-table-min-load ht))

(define-message (max-load (<hash-table> ht))
  (hash-table-max-load ht))

(define-message (weak-keys (<hash-table> ht))
  (hash-table-weak-keys ht))

(define-message (weak-values (<hash-table> ht))
  (hash-table-weak-values ht))

(define-message (has-initial? (<hash-table> ht))
  (hash-table-has-initial? ht))

(define-message (initial (<hash-table> ht))
  (hash-table-initial ht))

(define-message (keys (<hash-table> ht))
  (hash-table-keys ht))

;; No naming conflict because `compose' is the equivalent of `values'
(define-message (values (<hash-table> ht))
  (hash-table-values ht))

(define-message (ref (<hash-table> ht) (key))
  (hash-table-ref ht key))

(define-message (ref/default (<hash-table> ht) (key) (default))
  (hash-table-ref/default ht key default))

(define-message (exists? (<hash-table> ht) (key))
  (hash-table-exists? ht key))

;; Sets instead of using the hash-table-update! function. This is because the behavior of update
;; can be achieved by checking to see if the key exists before doing so.
;; As we cannot use set! as a name for a message, this was the best name for a setting message.
(define-message (update (<hash-table> ht) (key) (value))
  (chain (hash-table->alist ht)
      (alist-update key value _)
      (alist->hash-table _)))

(define-message (update! (<hash-table> ht) (key) (value))
  (hash-table-set! ht key value))

(define-message (copy (<hash-table> ht))
  (hash-table-copy ht))

(define-message (clear! (<hash-table> ht))
  (hash-table-clear! ht))

(define-message (map (<procedure> fn) (<hash-table> ht))
  (chain (lambda (x y) (cons x (fn y)))
      (hash-table-map ht _)
      (alist->hash-table _)))

(define-message (fold (<procedure> func) (init) (<hash-table> ht))
  (hash-table-fold ht func init))

(define-message (for-each (<procedure> proc) (<hash-table> ht))
  (hash-table-for-each ht proc))

(define-message (walk (<procedure> proc) (<hash-table> ht))
  (hash-table-walk ht proc))

(define-message (->hash-table (<alist> alist))
  (alist->hash-table alist))

(define-message (->alist (<hash-table> ht))
  (hash-table->alist ht))

(define-message (remove (<procedure> pred) (<hash-table> ht))
  (let ((ht (hash-table-copy ht)))
    (hash-table-remove! ht pred)
    ht))

(define-message (remove! (<procedure> pred) (<hash-table> ht))
  (hash-table-remove! ht pred))

(define-message (merge (<hash-table> h1) (<hash-table> h2))
  (hash-table-merge h1 h2))

(define-message (merge! (<hash-table> h1) (<hash-table> h2))
  (hash-table-merge! h1 h2))

(define-message (delete! (key) (<hash-table> ht))
  (hash-table-delete! ht key))
