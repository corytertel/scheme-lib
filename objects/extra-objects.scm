
;;; Stack

(define-object stack
  (lambda args
    (cons '<stack> args))
  (lambda (x)
    (and (pair? x) (eq? '<stack> (car x)))))

(define-message (push (stack? s) (x))
  (apply make (cons <stack> (cons x (cdr s)))))

(define-message (push! (stack? s) (x))
  (set-cdr! s (cons x (cdr s))))

(define-message (pop (stack? s))
  (apply make (cons <stack> (cddr s))))

(define-message (pop! (stack? s))
  (set-cdr! s (cddr s)))

(define-message (peek (stack? s))
  (cadr s))

(define-message (empty? (stack? s))
  (null? (cdr s)))

;;; Queue

(define-object queue
  (lambda ()
    (cons '<queue> (cons '() '())))
  (lambda (x)
    (and (pair? x) (eq? '<queue> (car x)))))

;; TODO find some way to hide these accessors from the public
;; have the object be a function?

(define-message (data (queue? queue))
  (cadr queue))

(define-message (set-data! (queue? queue) (x))
  (set-car! (cdr queue) x))

(define-message (end (queue? queue))
  (cddr queue))

(define-message (set-end! (queue? queue) (x))
  (set-cdr! (cdr queue) x))

(define-message (empty? (queue? queue))
  (null? (data queue)))

(define-message (enqueue! (queue? queue) (x))
  (let ((new (list x)))
    (cond ((empty? queue)
	   (set-data! queue new)
	   (set-end! queue new))
	  (else
	   (set-cdr! (end queue) new)
	   (set-end! queue new)))))

(define-message (dequeue! (queue? queue))
  (if (empty? queue)
      (error) ;; TODO fix to return a condition rather than throw an error
      (let ((front (car (data queue))))
	(set-data! queue (cdr (data queue)))
	(when (empty? queue)
	  (set-end! queue '()))
	front)))

(define-message (->list (queue? queue))
  (copy (data queue)))
