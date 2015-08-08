#lang racket

; discussion notes:
; closure
; sequences of pairs
; a list is equivalent to nested conses ending with nil
; (list 1 2 3 4)

; mapping over lists

; scale-list applies a scaling factor
; to the elements of a list

(define (scale seq factor)
	(if (null? seq)
            '()
            (cons (* (car seq) factor)
                  (scale (cdr seq) factor))))

;'(1 2 3 4 5)
;> (scale (list 1 2 3 4 5 6 7 8) 4)
;'(4 8 12 16 20 24 28 32)
;> 

; the map function:

;> (map (lambda (x) (* x x)) (list 1 2 3 4))
;'(1 4 9 16)
;> (map (lambda (x) (* x x)) '(1 2 3 4))
;'(1 4 9 16)
;> 

; 2.2.2 Hierarchical Structures

; recursion is natural for working
; with tree structures
; can recurse down branches

; pair? tests if its argument is a pair.

; mapping over trees:

(define (scale-tree tree factor)
  (cond ((null? tree) '())
        ((not (pair? tree)) (* tree factor))
        (else (cons (scale-tree (car tree) factor)
                    (scale-tree (cdr tree) factor)))))

;> (scale-tree (list 1 (list 2 (list 3 4) 5) (list 6 7))
;              10)
;'(10 (20 (30 40) 50) (60 70))
;>

; another scale-tree, using map:

(define (scale-tree2 tree factor)
  (map (lambda (sub-tree)
         (if (pair? sub-tree)
             (scale-tree2 sub-tree factor)
             (* sub-tree factor)))
       tree))


; ********

(define (square x) (* x x))

;;; sequences as conventional interfaces:

; Computation Structures: --> building computations

; note different ways to structure a computation
; to focus on different conceptual steps in the
; computation


(define (sum-odd-squares1 tree)
  (cond ((null? tree) 0)
  ((not (pair? tree))
   (if (odd? tree) (square tree) 0))
  (else (+ (sum-odd-squares1 (car tree))
     (sum-odd-squares1 (cdr tree))))))

(sum-odd-squares1 (list 1 2 3 4 (list 5 6 7) (list 8 (list 9 10)))) ; Value: 165.

(define (even-fibs1 n)
  (define (next k)
    (if (> k n)
  '()
  (let ((f (fib k)))
    (if (even? f)
        (cons f (next (+ k 1)))
        (next (+ k 1))))))
  (next 0))

(define (fib n)
  (cond ((= n 0) 0)
  ((= n 1) 1)
  (else (+ (fib (- n 1)) (fib (- n 2))))))

(fib 7)

(fib 34)

(even-fibs1 15)


(map square (list 1 2 3 4 5 6 7 8))

; Developing a signal processing computation structure:

;;; a filter procedure p. 115

(define (filter predicate sequence)
  (cond ((null? sequence) '())
        ((predicate (car sequence))
         (cons (car sequence)
         (filter predicate (cdr sequence))))
  (else (filter predicate (cdr sequence)))))

(filter odd? (list 1 2 3 4 5))

;;; an accumulator:

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence) (accumulate op initial (cdr sequence)))))

(accumulate + 0 (list 1 2 3 4 5))

(accumulate cons '() (list 1 2 3 4 5))


;;;  an interval enumerator

(define (enumerate-interval low high)
  (if (> low high)
      '()
      (cons low (enumerate-interval (+ low 1) high))))

(enumerate-interval 1 20)


(define (enumerate-tree tree)
  (cond ((null? tree) '())
  ((not (pair? tree)) (list tree))
  (else (append (enumerate-tree (car tree))
          (enumerate-tree (cdr tree))))))

(enumerate-tree (list 1 (list 2 3 (list 4 5 6) (list 7)) 8 9))

(define (sum-odd-squares2 tree)
  (accumulate +
        0
        (map square
       (filter odd?
         (enumerate-tree tree)))))

(sum-odd-squares2 (list 1 2 (list 3 4 (list 5 6)) 7 8 9))

(define (even-fibs n)
  (accumulate cons
        '()
        (filter even?
          (map fib
         (enumerate-interval 0 n)))))

(even-fibs 20)

; nested mappings

;;;  flatmap creates a list by applying proc to each item in list seq:
;;;  seq is a list of lists
;;;  map works on a list of primitives

(flatmap (lambda (i) (+ 1 i)) (list (list 1) (list 2) (list 3) (list 4) (list 5)))

 
(define (flatmap proc seq)
  (accumulate append '() (map proc seq)))

;;;  a procedure to generate list of integers i, j with j < i and 
;;;  i + j prime; i, j both < n

(define (prime-sum? pair)
  (prime? (+ (car pair) (cadr pair))))

(define (make-pair-sum pair)
  (list (car pair) (cadr pair) (+ (car pair) (cadr pair))))

(define (prime-sum-pairs2 n)
  (map make-pair-sum
       (filter prime-sum?
         (flatmap
    (lambda (i)
      (map (lambda (j) (list i j))
           (enumerate-interval 1 (- i 1))))
    (enumerate-interval 1 n)))))

(prime-sum-pairs2 20)


(define (remove item sequence)
  (filter (lambda (x) (not (= x item)))
    sequence))

(define (permutations s)
  (if (null? s)
      (list '())
      (flatmap (lambda (x)
     (map (lambda (p) (cons x p))
          (permutations (remove x s))))
         s)))

(permutations (list 1 2 3 4))


;;;  Ex. 2.34 p. 119  ********
;;;  Evaluating a polynomial in x at a given value of x can be formulated as an accumulation.
;;;  a_n * x^n  +  a_n-1 * x^(n-1)  + ... +  a_1 * x  +  a_0  =
;;;  (... (a_n * x  +  a_(n-1) * x  + ...  + a_1) * x  +  a_0
;;;  start with the nth coefficient; multiply by x then add next coefficient; multiply by x
;;;  all the way to the last coefficient a_0.


(define (horner-eval x coefficient-sequence)
  (accumulate (lambda (this-coeff higher-terms) (+ (* higher-terms  x) this-coeff)) 
        0
        coefficient-sequence))

(horner-eval 2 (list 1 3 0 5 0 1))


;;;  Ex. 2.35 p. 120
;;;  Redefine count-leaves as an accumulation

(define (count-leaves2 t)
  (accumulate (lambda (x y) (+ 1 y))
        0
        (enumerate-tree t)))

(define (count-leaves3 t)
  (accumulate (lambda (x y) (+ 1 y))
        0
        (map (lambda (x) 1) (enumerate-tree t)))) ; not really doing anything different from count-leaves2

;;;  Ex. 2.36 p. 120
;;;  The procedure accumulate-n similar to accumulate
;;;  but 3rd argument is a sequence of sequences with same
;;;  number of elements
;;;  then accumulate-n behaves like map element-wise

(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      '()
      (cons (accumulate op init (map car seqs))
      (accumulate-n op init (map cdr seqs)))))

;;;  NB.  seqs is a list of lists
;;;  mapping car onto seqs creates a list of the first
;;;  element in each list in seqs
;;;  mapping cdr onto seqs creates a list of lists compristing
;;;  the cdrs of each list in seq.
;;;  this is the correct list to look at for consing
;;;  Key is to describe precisley what is the list formed by 
;;;  map
;;;  (cdr seqs) is wrong because it contains all the lists except
;;;  the first list in the list seqs whereas what you want
;;;  is the list of each list in seqs minus the first element
;;;  You need to map cdr onto seqs for that.

(define s (list (list 1 2 3) (list 4 5 6) (list 7 8 9)))

(accumulate-n + 0 s)

(map car s)

(accumulate + 0 (map car s))
