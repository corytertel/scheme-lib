
#|
Types:
<number>
<complex>
<real>
<rational>
<integer>
<exact>
<inexact>
<zero>
<positive>
<negative>
<odd>
<even>
<boolean>
<pair>
<null>
<list>
<symbol>
<char>
<char-alphabetic>
<char-numeric>
<char-whitespace>
<char-upper-case>
<char-lower-case>
<string>
<vector>
<procedure>
<eof-object>

|#

;;; Numbers

(define-base-message (= . (number? nums)))
(define-base-message (< . (real? nums)))
(define-base-message (> . (real? nums)))
(define-base-message (<= . (real? nums)))
(define-base-message (>= . (real? nums)))
(define-base-message (max . (real? nums)))
(define-base-message (min . (real? nums)))
(define-base-message (+ . (number? nums)))
(define-base-message (* . (number? nums)))
(define-base-message (- . (number? nums)))
(define-base-message (/ . (number? nums)))
(define-base-message (abs (real? x)))
(define-base-message (quotient (integer? n1) (integer? n2)))
(define-base-message (remainder (integer? n1) (integer? n2)))
(define-base-message (modulo (integer? n1) (integer? n2)))
(define-base-message (gcd . (integer? nums)))
(define-base-message (lcm . (integer? nums)))
(define-base-message (numerator (rational? q)))
(define-base-message (denominator (rational? q)))
(define-base-message (floor (real? x)))
(define-base-message (ceiling (real? x)))
(define-base-message (truncate (real? x)))
(define-base-message (round (real? x)))
(define-base-message (rationalize (real? x) (real? y)))
(define-base-message (exp (number? z)))
(define-base-message (log (number? z)))
(define-base-message (sin (number? z)))
(define-base-message (cos (number? z)))
(define-base-message (tan (number? z)))
(define-base-message (asin (number? z)))
(define-base-message (acos (number? z)))
(define-base-message (atan (number? z)))
(define-base-message (sqrt (number? z)))
(define-base-message (expt (number? z1) (number? z2)))
(define-base-message (real-part (number? z)))
(define-base-message (imag-part (number? z)))
(define-base-message (magnitude (number? z)))
(define-base-message (angle (number? z)))

(define-message (->inexact (exact? z))
  (exact->inexact z))

(define-message (->exact (inexact? z))
  (inexact->exact z))

(define-message (->string (number? z))
  (number->string z))

(define-message (->number (string? z))
  (number->string z))

(define-primitive-object complex
  (case-lambda
    ((x) (if (complex? x) x (error "bad argument type - not a complex" x)))
    ((type x y)
     (cond ((not (symbol? type)) (error "bad argument type - not a type" type))
	   ((eq? type 'rectangular) (make-rectangular x y))
	   ((eq? type 'polar) (make-polar x y))
	   (else (error "type must be 'rectangular or 'polar" type))))))

;;; Lists/Pairs

car
cdr
list
length
append
reverse
list-tail
list-ref
memq
memv
member
assq
assv
assoc

symbol->string
string->symbol

char=?
char<?
char>?
char<=?
char>=?
char-ci=?
char-ci<?
char-ci>?
char-ci<=?
char-ci>=?
char->integer
integer->char
char-upcase
char-downcase

make-string
string
string-length
string-ref
string-set!
string=?
string<?
string>?
string<=?
string>=?
string-ci=?
string-ci<?
string-ci>?
string-ci<=?
string-ci>=?
substring
string-append
string->list
list->string
string-copy
string-fill!

make-vector
vector
vector-length
vector-ref
vector-set!
vector->list
list->vector
vector-fill!

apply
map
for-each
force
call-with-current-continuation
values
call-with-values
dynamic-wind

eval

call-with-input-file
call-with-output-file
input-port?
output-port?
current-input-port
current-output-port
with-input-from-file
with-output-to-file
open-input-file
open-output-file
close-input-port
close-output-port

read
read-char
peek-char
char-ready?

write
display
newline
write-char

load


(define-primitive-object string
  (case-lambda
    ((k) (cond ((integer? k) (make-string k))
	       ((string? k) k)
	       (else (error "bad argument type - not an integer or string" k))))
    ((k char) (make-string k char))))

(define-primitive-object vector
  (case-lambda
    ((k) (cond ((integer? k) (make-vector k))
	       ((vector? k) k)
	       (else (error "bad argument type - not an integer or vector" k))))
    ((k fill) (make-vector k fill))))
