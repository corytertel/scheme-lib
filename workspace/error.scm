

;; (import bind)

;; (bind-rename "getTime" "current-microseconds")

;; (bind* #<<EOF
;;        uint64_t getTime();

;;        #ifndef CHICKEN
;;        #include <sys/time.h>
;;        uint64_t getTime() {
;; 			   struct timeval tim;
;; 				  gettimeofday(&tim, NULL);
;; 				  return 1000000 * tim.tv_sec + tim.tv_usec;
;; 				  }
;;        #endif

;;        EOF
;;        )


(import (chicken condition))
(import (chicken time))

(define ())

;;; Tests

(time (for (i 0) (< i 100000000) (+ i 1)
	   (+ 1 1)))

#|
26.125s CPU time, 0.128s GC time (major), 6841672/1140276 mutations (total/tracked),
127/624873 GCs (major/minor), maximum live heap: 824.14 KiB

26.266s CPU time, 0.131s GC time (major), 6841690/1140279 mutations (total/tracked),
127/624873 GCs (major/minor), maximum live heap: 821.67 KiB

25.756s CPU time, 0.124s GC time (major), 6841672/1140276 mutations (total/tracked),
127/624873 GCs (major/minor), maximum live heap: 821.67 KiB

26.759s CPU time, 0.127s GC time (major), 6841672/1140276 mutations (total/tracked),
127/624873 GCs (major/minor), maximum live heap: 821.66 KiB

26.933s CPU time, 0.121s GC time (major), 6841672/1140276 mutations (total/tracked),
127/624873 GCs (major/minor), maximum live heap: 821.62 KiB

26.838s CPU time, 0.135s GC time (major), 6841672/1140276 mutations (total/tracked),
127/624873 GCs (major/minor), maximum live heap: 821.64 KiB

26.232s CPU time, 0.135s GC time (major), 6841672/1140276 mutations (total/tracked),
127/624873 GCs (major/minor), maximum live heap: 821.63 KiB

26.013s CPU time, 0.13s GC time (major), 6841672/1140276 mutations (total/tracked),
127/624873 GCs (major/minor), maximum live heap: 821.64 KiB

26.278s CPU time, 0.13s GC time (major), 6841672/1140276 mutations (total/tracked),
127/624873 GCs (major/minor), maximum live heap: 821.66 KiB

26.532s CPU time, 0.131s GC time (major), 6841690/1140279 mutations (total/tracked),
127/624873 GCs (major/minor), maximum live heap: 821.62 KiB
|#

(time (for (i 0) (< i 1000000) (+ i 1)
	   (+ 1 1)))

#|
0.253s CPU time, 0.001s GC time (major), 68434/11403 mutations (total/tracked), 1/6249 GCs (major/minor), maximum live heap: 719.02 KiB

0.254s CPU time, 0.001s GC time (major), 68434/11403 mutations (total/tracked), 1/6249 GCs (major/minor), maximum live heap: 719.02 KiB

0.254s CPU time, 0.001s GC time (major), 68434/11403 mutations (total/tracked), 1/6249 GCs (major/minor), maximum live heap: 719.0 KiB

0.253s CPU time, 68434/11403 mutations (total/tracked), 1/6249 GCs (major/minor), maximum live heap: 718.91 KiB

0.253s CPU time, 0.001s GC time (major), 68434/11403 mutations (total/tracked), 1/6249 GCs (major/minor), maximum live heap: 719.02 KiB

0.256s CPU time, 0.001s GC time (major), 68434/11403 mutations (total/tracked), 1/6249 GCs (major/minor), maximum live heap: 718.91 KiB

0.252s CPU time, 0.001s GC time (major), 68434/11403 mutations (total/tracked), 1/6249 GCs (major/minor), maximum live heap: 718.87 KiB

0.253s CPU time, 68434/11403 mutations (total/tracked), 1/6249 GCs (major/minor), maximum live heap: 719.23 KiB

0.255s CPU time, 0.001s GC time (major), 68434/11403 mutations (total/tracked), 1/6249 GCs (major/minor), maximum live heap: 719.23 KiB

0.253s CPU time, 0.001s GC time (major), 68434/11403 mutations (total/tracked), 1/6249 GCs (major/minor), maximum live heap: 719.18 KiB
|#

(time (for (i 0) (< i 1000000) (+ i 1)
	   (handle-exceptions exn
	     exn
	     (+ 1 1))))

#|
3.718s CPU time, 0.029s GC time (major), 34212470/2083236 mutations (total/tracked), 34/61217 GCs (major/minor), maximum live heap: 717.51 KiB

3.648s CPU time, 0.022s GC time (major), 34212470/2083242 mutations (total/tracked), 34/61217 GCs (major/minor), maximum live heap: 717.45 KiB

3.667s CPU time, 0.027s GC time (major), 34212452/2083251 mutations (total/tracked), 34/61217 GCs (major/minor), maximum live heap: 717.51 KiB

3.801s CPU time, 0.026s GC time (major), 34212452/2083201 mutations (total/tracked), 34/61217 GCs (major/minor), maximum live heap: 717.5 KiB

3.621s CPU time, 0.025s GC time (major), 34212452/2083228 mutations (total/tracked), 34/61217 GCs (major/minor), maximum live heap: 717.5 KiB

3.625s CPU time, 0.028s GC time (major), 34212470/2083238 mutations (total/tracked), 34/61217 GCs (major/minor), maximum live heap: 717.5 KiB

3.787s CPU time, 0.027s GC time (major), 34212470/2083216 mutations (total/tracked), 34/61217 GCs (major/minor), maximum live heap: 717.5 KiB

3.671s CPU time, 0.029s GC time (major), 34212452/2083204 mutations (total/tracked), 34/61217 GCs (major/minor), maximum live heap: 717.5 KiB

3.693s CPU time, 0.026s GC time (major), 34212452/2083183 mutations (total/tracked), 34/61217 GCs (major/minor), maximum live heap: 717.5 KiB

3.648s CPU time, 0.025s GC time (major), 34212452/2083198 mutations (total/tracked), 34/61217 GCs (major/minor), maximum live heap: 717.5 KiB
|#

(time (for (i 0) (< i 1000000) (+ i 1)
	   (try (+ 1 1))))

#|
1.97s CPU time, 0.011s GC time (major), 19131452/2032634 mutations (total/tracked), 18/34563 GCs (major/minor), maximum live heap: 731.79 KiB

2.028s CPU time, 0.016s GC time (major), 19131452/2032631 mutations (total/tracked), 18/34563 GCs (major/minor), maximum live heap: 731.38 KiB

1.983s CPU time, 0.013s GC time (major), 19131452/2032637 mutations (total/tracked), 18/34563 GCs (major/minor), maximum live heap: 731.41 KiB

1.993s CPU time, 0.013s GC time (major), 19131452/2032632 mutations (total/tracked), 18/34563 GCs (major/minor), maximum live heap: 731.79 KiB

1.997s CPU time, 0.014s GC time (major), 19131434/2032679 mutations (total/tracked), 18/34563 GCs (major/minor), maximum live heap: 731.79 KiB

1.98s CPU time, 0.013s GC time (major), 19131452/2032629 mutations (total/tracked), 18/34563 GCs (major/minor), maximum live heap: 731.38 KiB

1.985s CPU time, 0.012s GC time (major), 19131452/2032631 mutations (total/tracked), 18/34563 GCs (major/minor), maximum live heap: 731.79 KiB

1.992s CPU time, 0.016s GC time (major), 19131452/2032642 mutations (total/tracked), 18/34563 GCs (major/minor), maximum live heap: 731.79 KiB

2.056s CPU time, 0.013s GC time (major), 19131452/2032615 mutations (total/tracked), 18/34563 GCs (major/minor), maximum live heap: 731.79 KiB

1.977s CPU time, 0.013s GC time (major), 19131452/2032639 mutations (total/tracked), 18/34563 GCs (major/minor), maximum live heap: 731.48 KiB
|#

(time (for (i 0) (< i 1000000) (+ i 1)
	   (call/cc
	    (lambda (k)
	      (+ 1 1)))))

#|
0.843s CPU time, 0.007s GC time (major), 5091834/24185 mutations (total/tracked), 7/15905 GCs (major/minor), maximum live heap: 729.4 KiB
|#



You are only error handling for the case when the message doesn't exist.
Not for when there actually is an error within the message itself.
If there is an error or possible error within the message, then the message
itself should take care of it.

Other than that, dispatching will determine if the types are correct and return a condition if not, not throw an error. No need for an exception handler over every iteration of dispatching.

The predicate typing should be setup in such a way that type errors aren't possible.
If there still is a type error, then the typing isn't specific enough.

(make-property-condition)


(define (abort obj)
  ((current-exception-handler) obj)
  (abort (make-property-condition
	  'exn
	  'message
	  "Exception handler returned")))

((current-exception-handler) "hello world")

(abort "hello world")


(abort (make-property-condition
	'exn
	'message
	"hello world"
	'location
	1
	'arguments
	2
	'call-chain
	'(#(3 4))))


(condition->list (handle-exceptions e e
				    (hth)))


;; Signaled on type-mismatch errors, for example when an argument
;; of the wrong type is passed to a built-in procedure.
(exn type)


;; Signaled when there is no message matching the signature of arguments passed.
;; aka a signature mis-match error.
(exn message)

(make-composite-condition
 (make-property-condition
  'exn
  'message "bad argument types - no message signatures match the given arguments"
  'arguments '(args)
  'location #f)
 (make-property-condition 'message))


Check arg types, as normal, and then if there is no match, then check to see if one of them is a condition and return the first condition, and if not, then return a new condition saying no match
