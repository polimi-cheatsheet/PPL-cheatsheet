#lang racket

; lambda definition
(lambda (x y) (+ x y))

; code blocks
(begin
  (displayln 42)
  (displayln 24))

; function definition
(define (minimum L)
  (let ((x (car L)) (xs (cdr L)))
    (if (null? xs)
        x
        (min x (minimum xs)))))
(minimum '(4 2 1 5)) ; => 1
; variable number of arguments
(define (minimum2 x . rest)
  (if (null? rest)
      x
      (min x (minimum rest))))
(minimum2 4 2 1 5) ; => 1

; let binding
(let ((x 10) (y 15))
  (+ x y))

; static scoping
(let ((a 1))
  (let ((f (lambda () (displayln a))))
    (let ((a 2))
      (f)))) ; => 1

; global variables
(define x 12)
(set! x 42)
(define double (lambda (x) (* 2 x)))

; lists
(define l1 '(1 2 3))
(define l2 (cons 1 (cons 2 (cons 3 '())))) ; => '(1 2 3)
(car l1) ; => 1
(cdr l1) ; => '(2 3)
(apply + '(1 2 3 4)) ; => 10
(null? l1) ; => #f
(null? '()) ; => #t

; if-else
(if (= (+ 1 2) 3) 42 24) ; => 42
(when (< 10 12)
  (displayln "ciao")
  (displayln "mondo")
  42) ; => ciao mondo 42

; equivalence
(eq? 'ciao 'ciao) ; => #t
(eqv? 42 42) ; => #t
(equal? '(1 2 3) '(1 2 3)) ; => #t

(case (car '(c d))
  ((a e i o u) 'vowel)
  (else 'consonant)) ; => 'consonant
(cond ((> 3 3) 'greater)
      ((< 3 3) 'less)
      (else 'equal)) ; => 'equal

; loops
(let label ((x 0))
  (when (< x 10)
    (displayln x)
    (label (+ x 1)))) ; => 1 2 ... 9
(for-each (lambda (x) (displayln x))
          '(1 2 3 4))

; vectors
(define vec (vector 1 2 3))
(vector-ref vec 1) ; => 2
(vector-set! vec 1 42) ; => #(1 42 3)
(vector-length vec) ; => 3

; structs
(struct being
  (name
   (age #:mutable)))
(define (say-hello x)
  (if (being? x)
      (printf "Hi ~a (~a)~n"
              (being-name x)
              (being-age x))
      (displayln "Not a being")))
(define edo (being "Edo" 22))
(set-being-age! edo 23)
(say-hello edo) ; => Hi Edo (23)
(struct may-being being
  ((alive? #:mutable))) ; inheritance

; closures
(define (make-adder n)
  (lambda (x) (+ x n)))
(define add5 (make-adder 5))
(add5 10) ; => 15

; useful functions
(map (lambda (x) (+ x 5)) '(1 2 3)) ; => '(6 7 8)
(filter (lambda (x) (>= x 10)) '(1 10 100)) ; => '(10 100)
(foldl cons '() '(1 2 3)) ; => '(3 2 1)
(foldr cons '() '(1 2 3)) ; => '(1 2 3)
(foldl * 1 '(1 2 3 4)) ; => 24
; apply f first to the first, going from the left
(define (fold-left f i L)
  (if (null? L)
      i
      (fold-left f (f (car L) i) (cdr L))))
; apply f first to the last, going from the right
(define (fold-right f i L)
  (if (null? L)
      i
      (f (car L) (fold-right f i (cdr L)))))

; macro
(define-syntax while
  (syntax-rules ()
    ((_ condition body ...) ; (while cond (a) (b)...)
     (let loop ()
       (when condition
         (begin
           body ...
           (loop)))))))
(define-syntax For
  (syntax-rules (from to do) ; extra keyword
    ((_ var from min to max do body ...)
     (let loop ((var min))
       body ...
       (when (< var max)
         (loop (+ var 1)))))))
(For i from 1 to 5 do (displayln i)) ; => 1 2 3 4 5

; continuations
(define saved-cont #f)
(define (test-cont)
  (let ((x 0))
    (call/cc (lambda (k) (set! saved-cont k)))
    (set! x (+ x 1))
    (displayln x)))
(test-cont) ; => 1
(saved-cont) ; => 2
(define other-cont saved-cont)
(test-cont) ; => 1
(other-cont) ; => 3
(saved-cont) ; => 2

; closure as objects
(define (make-simple-object)
  (let ((my-var 0))  ; attributes
    (define (my-add x)
      (set! my-var (+ my-var x))
      my-var)
    (define (get-my-var)
      my-var)
    (define (my-display)
      (printf "my-var=~a~n" my-var))

    (lambda (message . args)
      (apply (case message
               ((my-add) my-add)
               ((get-my-var) get-my-var)
               ((my-display) my-display)
               (else (error "Unknown method")))
             args))))
(define obj (make-simple-object))
(obj 'my-add 3)
(obj 'get-my-var) ; => 3
(obj 'my-display)
; inheritance
(define (make-son)
  (let ((parent (make-simple-object))
        (name "test"))
    (define (hello)
      "hi")
    (define (my-display)
      (printf "My name is ~a and " name)
      (parent 'my-display))
    (lambda (message . args)
      (case message
        ((hello) (apply hello args))
        ((my-display) (apply my-display args))
        (else (apply parent (cons message args)))))))
(define obj2 (make-son))
(obj2 'hello) ; => hi
(obj2 'my-display) ; => My name is test and my-var=0

; Proto-oo
(define new-object make-hash)
(define clone hash-copy)
(define-syntax !! ; setter
  (syntax-rules ()
    ((_ object msg new-val)
     (hash-set! object 'msg new-val))))
(define-syntax ?? ; getter
  (syntax-rules ()
    ((_ object msg)
     (hash-ref object 'msg))))
(define-syntax -> ; send message
  (syntax-rules ()
    ((_ object msg arg ...)
     ((hash-ref object 'msg) object arg ...))))

(define Pino (new-object))
(!! Pino name "Pino")
(!! Pino hello (lambda (self) (printf "Name is ~v~n" (?? self name))))
(!! Pino set-name (lambda (self x) (!! self name x)))
(define Pina (clone Pino))
(-> Pina set-name "Pina")
(-> Pina hello) ; => Name is Pina
; inheritance
(define (son-of parent)
  (let ((o (new-object)))
    (!! o <<parent>> parent)
    o))
(define (dispatch object msg)
  (if (eq? object 'unknown)
      (error "Unknown message" msg)
      (let ((slot (hash-ref object msg 'unknown)))
        (if (eq? slot 'unknown)
            (dispatch (hash-ref object '<<parent>> 'unknown) msg)
            slot))))
(define-syntax ??? ; reader ** should be ?? **
  (syntax-rules ()
    ((_ object msg)
     (dispatch object 'msg))))
(define-syntax --> ; send message ** should be -> **
  (syntax-rules ()
    ((_ object msg arg ...)
     ((dispatch object 'msg) object arg ...))))

(define Glenn (son-of Pino))
(!! Glenn name "Glenn")
(!! Glenn age 50)
(--> Glenn hello)
; (--> Glenn boh) ; => error: Unknown message boh






































