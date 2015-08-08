#lang racket

; streams - an alternative model (to assignment);
; for capturing time

; recap

; must use racket/stream:
; 
; http://docs.racket-lang.org/reference/streams.html

(require racket/stream)

; we modeled real-world objects with local state using
; computational objects with local variables

; time variation in the real world is treated as time
; variation in the computer

; time variation is implemented by assignment to 
; local variables

; analogizing to a mathematical function x(t),
; our model views that function as a series of states
; --the values of the function at different times

; an alternative view of x(t)
; the collection of all of the states
; --under this view, we are not focused
; on individual changes in state w.r.t. time but rather
; with the total evolution--the total sequence of states,
; the states perhaps changing with time

; new computational structure: stream--a sequence
; plus delayed evaluation (to capture infinite sequences)

; using streams we can capture state without using assignment


; 3.5.1 streams made of pairs with
; the second element not evaluated

; streams allow us to use sequence manipulations without incurring
; the costs of manipulating sequences as lists

; streams --> partial construction of the sequence

; constructor: stream-cons

; selectors
;   stream-first
;   stream-rest

;> (stream-first (stream-cons 1 2))
;1
;> (define s (stream-cons 1 2))
;> (stream-first s)
;1
;> (stream-rest s)
;#<stream>
;> 

; stream-cons constructs a stream s
; stream-first returns the first value of s
; stream-rest returns a stream equivalent to
; s less the first element of s

; empty-stream (no elements)

(define (stream-index s n)
  (if (= n 0)
      (stream-first s)
      (stream-index (stream-rest s) (- n 1))))

; stream-ref is already part of racket

; the key to a stream is delaying the execution of
; the rest of the stream with a function: delay
;
; later, when the rest of the stream is called
; it is executed by the function force

; stream-cons is a special form delaying 
; the evaluation of the expression that
; will be the stream-rest part of the 
; stream:
;
; (stream-cons <a> <b>) is equivalent to
;
;          (cons <a> (delay <b>))
;
; (define (stream-first s)
;   (car s))
;
; (define (stream-rest s)
;   (force (cdr s)))

; key pedagogical concept--the evaluation model
;     
;     understand the evaluation model
;     of the language you are using
;
; see SICP pp. 321-324

; this is going to let us use the concept of the whole
; string without having to evaluate each element
; of that string

; this idea itself is useful in working with very large 
; lists--even infinitely long lists--by using them in their
; entirety but without the cost of computing each element

(define (stream-enumerate-interval low high)
  (if (> low high)
      empty-stream
      (stream-cons
       low
       (stream-enumerate-interval (+ low 1) high))))

(define (stream-filter-bis pred stream)
  (cond ((null? stream) empty-stream)
        ((pred (stream-first stream))
         (stream-cons (stream-first stream)
                      (stream-filter-bis pred
                                     (stream-rest stream))))
        (else (stream-filter-bis pred (stream-rest stream)))))


; implementing delay and force

; delay must package an expression so that it can be evaluated
; later on demand
; we can accomplish this by treating the expression
; as the body of a procedure

; 

; delay must be a special form (because its arg
; must not be evaluated):
;
;         (delay <exp>)
;
;
; means
;
;         (lambda () <exp>)
;

; force simply calls the procedure produced by delay:
;
;

;(define (force delayed-object)
;  (delayed-object))


; for delayed evaluation in racket:
; (require racket/promise)
;
; in racket:
;
;     http://docs.racket-lang.org/reference/Delayed_Evaluation.html
;

(require racket/promise)

(define (integers-starting-from n)
  (stream-cons n (integers-starting-from (+ n 1))))

(define integers (integers-starting-from 1))


;> integers
;#<stream>
;> (stream-first integers)
;1
;> (stream-first (stream-rest integers))
;2
;> (stream-first (stream-rest (stream-rest integers)))
;3
;> (stream-first (stream-rest (stream-rest (stream-rest integers))))
;4
;> 

; with the infinite stream integers we can define other
; infinite streams -- for example the sequence
; of numbers not divisible by 7

(define (divisible? x y) (= (remainder x y) 0))

(define no-sevens
  (stream-filter (lambda (x) (not (divisible? x 7)))
                 integers))

;'stream-ref-100
;(stream-ref no-sevens 100)

;'stream-ref-100
;117
;> 

; infinite stream of Fibonacci

(define (fibgen a b)
  (stream-cons a (fibgen b (+ a b))))

(define fibs (fibgen 0 1))

;'fib-100
;(stream-ref fibs 100)

;'fib-100
;354224848179261915075
;> 

; stream of prime numbers using
; sieve of Eratosthenes

; we start with integers beginning with 2, the first prime
; filter all multiples of 2 from the rest of the integers
; this yields a stream beginning with 3, the second prime
; filter all multiples of 3 from that stream
; this yields a stream beginning with 5, the third prime
; continue filtering in this fashion

(define (sieve s)
  (stream-cons
   (stream-first s)
   (sieve (stream-filter
           (lambda (x)
             (not (divisible? x  (stream-first s))))
           (stream-rest s)))))

(define primes (sieve (integers-starting-from 2)))

;'the-100th-prime:

;(stream-ref primes 100)

;'the-100th-prime:
;547
;> 

;(stream-ref primes 1000)

;7927
;> 


; defining streams implicitly

; integers and fibs were defined by a 
; generating procedure that explicitly
; computed stream elements one by one

; consider an infinite stream of ones defined
; implicitly

(define ones (stream-cons 1 ones))

;'first-element-ones
;(stream-first ones)

;'second-ones
;(stream-first (stream-rest ones))

;'first-element-ones
;1
;'second-ones
;1
;> 

(define (add-streams s1 s2)
  (stream-map-e + s1 s2))

(define (stream-map-e proc . argstreams)
  (if (null? (car argstreams))
      empty-stream
      (stream-cons
       (apply proc (map stream-first argstreams))
       (apply stream-map-e
              (cons proc (map stream-rest argstreams))))))

(define twos (add-streams ones ones))

;'first-of-twos
;(stream-first twos)

;'second-of-twos
;(stream-first (stream-rest twos))

; now define integers implicitly:

(define integers-i (stream-cons 1 (add-streams ones integers-i)))

;> (stream-ref integers-i 10)
;11
;> (stream-ref integers-i 0)
;1
;> 

; a similar implicit definition of Fibonaccis:

(define fib-imp
  (stream-cons 0
               (stream-cons 1
                            (add-streams (stream-rest fib-imp)
                                         fib-imp))))


; a useful stream operator: stream-scale

(define (stream-scale s k)
  (stream-map (lambda (x) (* x k)) s))

; powers of 2

(define 2-powers (stream-cons 1 (stream-scale 2-powers 2)))

;> (stream-ref 2-powers 10)
;1024
;> 

(define primes-def
  (stream-cons
   2
   (stream-filter prime? (integers-starting-from 3))))

(define (prime? n)
  (define (iter ps)
    (cond ((> (square (stream-first ps)) n) true)
          ((divisible? n (stream-first ps)) false)
          (else (iter (stream-rest ps)))))
  (iter primes))

(define (square x) (* x x))

;> (stream-ref primes-def 10)
;31
;> 

;> (stream-ref primes-def 100)
;547
;> 

;> (stream-ref primes-def 1000)
;7927
;> 

;(stream-ref primes-def 10000)
;104743
;> 

;(stream-ref primes-def 100000)
;1299721
;> 

; Ex. 3.53

; explain what s does

(define s (stream-cons 1 (add-streams s s)))

; Ex. 3.54 
;
; define mul-streams analogous to add-streams and 
; producing elementwise product of 2 input streams
;
; use with integers stream integers-i to complete definition
; of following stream: nth element counting from 0 is 
;     (n+1)!
;

(define (mul-streams s1 s2)
  (stream-map-e * s1 s2))

(define factorials (stream-cons 1 (mul-streams factorials integers-i)))

; factorial of n+1 = factorial of n * (n+1) 
; (n+1 is the next integer)

;> (stream-ref factorials 0)
;1
;> (stream-ref factorials 1)
;1
;> (stream-ref factorials 2)
;2
;> (stream-ref factorials 3)
;6
;> (stream-ref factorials 4)
;24
;> (stream-ref factorials 5)
;120
;> 

; Ex. 3.55 define partial-sums taking 
; a stream S and returning a stream with 
; elements S0, S0+S1, S0+S1+S2 . . .
;
; for example partial-sums integers should be
;
; 1, 3, 6, 10 . . . .
;

(define (partial-sums s)
  (stream-cons (stream-first s)
               (add-streams (partial-sums s) (stream-rest s))))

;> (define t (partial-sums integers))
;> (stream-ref t 1)
;3
;> (stream-ref t 0)
;1
;> (stream-ref t 2)
;6
;> (stream-ref t 3)
;10
;> (stream-ref t 4)
;15
;> 


; Ex. 3.56 - enumerate in ascending order with no repetitions
; all positive integers with no prime factors other than 2, 3, or 5.
; Let S be this stream.
;
; S begins with 1
; (stream-scale S 2) are elements of S
; (stream-scale S 3) are elements of S
; (stream-scale S 5) are elements of S
; there are no other elements of S

; we will combine these streams to form S using
; merge
;
; merge combines 2 ordered streams into an ordered
; stream with no repetitions:

(define (merge s1 s2)
  (cond ((null? s1) s2)
        ((null? s2) s1)
        (else
         (let ((s1-first (stream-first s1))
               (s2-first (stream-first s2)))
           (cond ((< s1-first s2-first)
                  (stream-cons s1-first (merge (stream-rest s1) s2)))
                 ((< s2-first s1-first)
                  (stream-cons s2-first (merge (stream-rest s2) s1)))
                 (else
                  (stream-cons s1-first
                               (merge (stream-rest s1) (stream-rest s2)))))))))

(define s235 (stream-cons 1 (merge (stream-scale s235 5) 
                                   (merge (stream-scale s235 2)
                                          (stream-scale s235 3)))))

;> (stream-ref s235 0)
;1
;> (stream-ref s235 1)
;2
;> (stream-ref s235 2)
;3
;> (stream-ref s235 3)
;4
;> (stream-ref s235 4)
;5
;> (stream-ref s235 5)
;6
;> (stream-ref s235 6)
;8
;> (stream-ref s235 7)
;9
;> (stream-ref s235 8)
;10
;> (stream-ref s235 9)
;12
;> (stream-ref s235 10)
;15
;> (stream-ref s235 11)
;16
;> (stream-ref s235 12)
;18
;> 


; Ex. 3.59 
;
; in section 2.5.3 we saw how to implement a polynomial arithmetic
; system representing polynomials as lists of terms.
;
; we can also work with power series:
;
;        e^x  =  1 + x + x^2/2 + x^3/3*2 + x^4/4*3*2 . . .
;
;      cos x  =  1 - x^2/2 + x^4/4*3*2 + - . . .
;
;      sin x  =  x - x^3/3*2 + x^5/5*4*3*2 - . . .
;
;
; representing them as infinite streams.
;
;
; We will represent the series 
; 
; S = a0 + a1x + a2x^2 + a3x^3 . . .
;
; as the stream with elelments a0, a1, a2, a3 . . . 
;
;
; the integral of the series S is
;
;      S-I  =  c + a0x + (a1/2)*x^2 + (a2/3)*x^3 + . . .
;
;
; define integrate-series that returns the stream integral
; (non-constant terms only) of a given power series stream
;
; (because the result does not include the constant term,
; it is not a power series. we will simply cons on the
; constant term when we use the integrate-series)
;

; s has the form (a0, a1, a2, a3 . . .)
;
; s-i has the form (a0, a1/2, a2/3, a3/4 . . .)

; just divide elementwise by series of integers:

(define (integrate-series s)
  (stream-map-e / s integers-i))

;> (define t (integrate-series integers-i))
;> (stream-ref t 0)
;1
;> (stream-ref t 1)
;1
;> (stream-ref t 2)
;1
;> 

;> (define q (integrate-series ones))
;> (stream-ref q 0)
;1
;> (stream-ref q 1)
;1/2
;> (stream-ref q 2)
;1/3
;> (stream-ref q 3)
;1/4
;> (stream-ref q 4)
;1/5
;> 

;
; the function x --> e^x is its own derivative
;
; so e^x and the integral of e^x are the same series
; except for the constant term e^0 = 1
;
; we can generate the series for e^x with:

(define exp-series
  (stream-cons 1 (integrate-series exp-series)))

;> (stream-ref exp-series 5)
;1/120
;> 

; awesome

; show how to generate the series for sine and cosine given
; that the derivative of sine = cosine and
; the derivative of cosine = - sine

; --> integral cosine  =  sine + c
; --> integral sine    =  - cosine + c

(define cosine-series
  (stream-cons 1 (stream-scale
                  (integrate-series sine-series)
                  (- 1))))

(define sine-series
  (stream-cons 0 (integrate-series cosine-series)))

;> (stream-ref cosine-series 0)
;1
;> (stream-ref cosine-series 1)
;0
;> (stream-ref cosine-series 2)
;-1/2
;> (stream-ref cosine-series 3)
;0
;> (stream-ref cosine-series 4)
;1/24
;> (stream-ref cosine-series 5)
;0
;> (stream-ref cosine-series 6)
;-1/720
;> 


;> (stream-ref sine-series 0)
;0
;> (stream-ref sine-series 1)
;1
;> (stream-ref sine-series 2)
;0
;> (stream-ref sine-series 3)
;-1/6
;> (stream-ref sine-series 4)
;0
;> (stream-ref sine-series 5)
;1/120
;> (stream-ref sine-series 6)
;0
;> (stream-ref sine-series 7)
;-1/5040
;> 


; Ex. 3.60
;
; with power series represented as streams of coefficients
; as in exercise 3.59, adding series is implemented by
; add-streams
;
; complete mul-series:

; s1, s2:  two series of the form
;
;     s1  =  a0 + a1x + a2x^2 + a3x^3 + a4x^4 . . .
;
;     s2  =  b0 + b1x . . .
;
; the product s1*s2: ( s1 ) * ( s2 ) is equivalent to
;
;      a0*( s2 ) + (rest of s1)*s2 :    <-- note: this is a useful form for streams/recursion 
;                                           series = first-element + rest-of-series
;                                           with streams we can operate on the first element
;                                           and hold on to the rest of the elements without
;                                           having to evaluate them immediately


; preliminarily for notation:

(define (add-series s1 s2)
  (add-streams s1 s2))

(define (scale-series s k)
  (stream-scale s k))

  (define (mul-series-a s1 s2)
  (add-series (scale-series s2 (stream-first s1))
              (cons 0 (mul-series-a (stream-rest s1) s2))))
  
; this definition of mul-series can be re-arranged
; to the form proposed in the exercise.
; the first term must be the product a0*b0.
;
; -->
;
;  a0*b0 + a0*(rest of s2) + (rest of s1)*(b0 + (rest of s2))
;
;  a0*b0 + (rest of s2)*[a0 + (rest of s1)] + b0*(rest of s1)
;
;  a0*b0 + (rest of s2)*s1 + b0*(rest of s1)

; also try to reach this form by thinking through what is happening
; in the definition for mul-series-a.

(define (mul-series s1 s2)
  (stream-cons (* (stream-first s1) (stream-first s2))
               (add-streams (mul-series s1 (stream-rest s2))
                            (scale-series (stream-rest s1) (stream-first s2)))))

;'test-cos^2+sin^2=1

(define t (add-streams
           (mul-series cosine-series cosine-series)
           (mul-series sine-series sine-series)))

;'test-cos^2+sin^2=1
;> t
;#<stream>
;> (stream-first t)
;1
;> (stream-first (stream-rest t))
;0
;> (stream-first (stream-rest (stream-rest t)))
;0
;> 

(define (display-line x)
  (newline)
  (display x))

(define (display-stream s n)
  (if (= n 0)
      (newline)
      (begin
        (display-line (stream-first s))
        (display-stream (stream-rest s) (- n 1)))))
        

; 3.5.3 Exploiting the Stream paradigm

(define (sqrt-improve guess x)
  (average guess (/ x guess)))

(define (average x y)
  (/ (+ x y) 2))


(define (sqrt-stream x)
  (define guesses
    (stream-cons 1.0
                 (stream-map (lambda (guess)
                               (sqrt-improve guess x))
                             guesses)))
  guesses)

;(display-stream (sqrt-stream 2.0) 10)

;1.0
;1.5
;1.4166666666666665
;1.4142156862745097
;1.4142135623746899
;1.414213562373095
;1.414213562373095
;1.414213562373095
;1.414213562373095
;1.414213562373095
;> 

; A stream to approximate pi:
;
;     pi/4  =  1 - 1/3 + 1/5 - 1/7 + ...
;
;

(define (pi-summands n)
  (stream-cons (/ 1.0 n)
               (stream-map - (pi-summands (+ n 2)))))

(define pi-stream
  (stream-scale (partial-sums (pi-summands 1)) 4))

;(display-stream pi-stream 10)

;4.0
;2.666666666666667
;3.466666666666667
;2.8952380952380956
;3.3396825396825403
;2.9760461760461765
;3.2837384837384844
;3.017071817071818
;3.2523659347188767
;3.0418396189294032
;> 


; Sequence acceleration with streams:

; Euler
;
; for sequences that are partial sums of alternating series
;
; let Sn be the nth term of the sum sequence
;
; here is the accelerated sequence:
;
;
;   S_n+1 - [(S_n+1 - S_n)^2]/(S_n-1 - 2S_n + S_n+1)

; let's call this the Euler transform of a sequence of partial sums

(define (euler-transform s)
  (let ((s0 (stream-ref s 0))    ; S_n-1
        (s1 (stream-ref s 1))    ; S_n
        (s2 (stream-ref s 2)))   ; S_n+1
    (stream-cons (- s2 (/ (square (- s2 s1))
                          (+ s0 (* -2 s1) s2)))
                 (euler-transform (stream-rest s)))))

;(display-stream (euler-transform pi-stream) 10)
;
;3.166666666666667
;3.1333333333333337
;3.1452380952380956
;3.13968253968254
;3.1427128427128435
;3.1408813408813416
;3.142071817071818
;3.1412548236077655
;3.1418396189294033
;3.141406718496503
;> 

; Accelerate the accelerated sequence

; a stream of stream (a tableau) where each stream is the transform of
; the prior stream:

(define (make-tableau transform s)
  (stream-cons s
               (make-tableau transform
                             (transform s))))

(define (accelerated-sequence transform s)
  (stream-map stream-first
              (make-tableau transform s)))

; see this super-acceleration of the pi sequence:

;(display-stream (accelerated-sequence euler-transform pi-stream) 10)
;
;4.0
;3.166666666666667
;3.142105263157895
;3.141599357319005
;3.1415927140337785
;3.1415926539752927
;3.1415926535911765
;3.141592653589778
;3.1415926535897953
;3.141592653589795
;> 

; Ex. 3.64 - write stream-limit taking a stream and a number
; for tolerance
;
; stream-limit examines stream until it finds two successive
; elements of difference (abs) less than tolerance
; 
; stream-limit returns the second of the 2 elements

; we can use stream-limit to compute square roots to
; specified tolerance with

(define (sqrt-s x tolerance)
  (stream-limit (sqrt-stream x) tolerance))

(define (stream-limit s tolerance)
  (define (good-enough? diff)
    (< diff tolerance))
  (define (iter s)
    (if (null? s) (error "expected a non-empty stream -- ITER in stream-limit")
        (let ((e1 (stream-first s))
              (e2 (stream-first (stream-rest s))))
          (if (good-enough? (abs (- e2 e1)))
              e2
              (iter (stream-rest s))))))
  (iter s))

;(sqrt-s 2.0 0.0001)

;1.4142135623746899
;> 

; Ex. 3.65
;
;     ln 2  =  1 - 1/2 + 1/3 - 1/4 + ...
;
; define three sequences to approximate ln 2 as we did for pi
;
; discuss the change in rate of convergence
;
; plot the three rates of convergence

(define (ln2-summands n)
  (stream-cons (/ 1.0 n)
               (stream-map - (ln2-summands (+ n 1)))))

(define ln2-stream
  (partial-sums (ln2-summands 1)))
         
;(display-stream ln2-stream 10)

;1.0
;0.5
;0.8333333333333333
;0.5833333333333333
;0.7833333333333332
;0.6166666666666666
;0.7595238095238095
;0.6345238095238095
;0.7456349206349207
;0.6456349206349207
;> 

; by the way, ln 2:
;> (log 2)
;0.6931471805599453
;> 

; method 2:

;(display-stream (euler-transform ln2-stream) 10)

;0.7
;0.6904761904761905
;0.6944444444444444
;0.6924242424242424
;0.6935897435897436
;0.6928571428571428
;0.6933473389355742
;0.6930033416875522
;0.6932539682539683
;0.6930657506744464
;> 

; method 3, with accelerated acceleration:

;(display-stream (accelerated-sequence euler-transform ln2-stream) 10)

;1.0
;0.7
;0.6932773109243697
;0.6931488693329254
;0.6931471960735491
;0.6931471806635636
;0.6931471805604039
;0.6931471805599445
;0.6931471805599427
;0.6931471805599454
;> 


; Infinite streams of pairs

; to continue


