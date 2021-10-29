#lang racket

(require (only-in racket/generator in-generator yield))
(provide in-rearrangements equivalence?)

;===================================================================================================

(define (in-rearrangements lst (eq equal?))
 
 (define (in-rearrangements lst)
  (in-generator
   (cond
    ((null? lst) (yield '()))
    (else
     (for ((rotation (in-rotations lst)))
      (define element (car rotation))
      (for ((rearrangement-of-cdr (in-rearrangements (cdr rotation))))
       (yield (cons element rearrangement-of-cdr))))))))
 
 (define (in-rotations lst)
  (in-generator
   (let loop ((head lst) (tail '()) (skip-set empty-skip-set))
    (unless (null? head)
     (define element (car head))
     (define new-tail (cons element tail))
     (define new-head (cdr head))
     (cond
      ((set-member? skip-set element)
       ; Same as (member element tail eq), but possibly faster for long lists.
       ; However, possibly slower for short lists.
       ; Using member we dont need skip-set and can remove procedure skip-set
       ; and loop argument skip-set. This simplifies the code significantly.
       (loop new-head new-tail skip-set))
      (else
       (yield (append head (reverse tail)))
       ; The reversal is not necessary, but makes procedure in-rearrangements
       ; produce the rearrangements in a more logical order.
       ; The following is faster, though:
       #;(yield (append head tail))
       (loop new-head new-tail (set-add skip-set element))))))))
 
 (define empty-skip-set
  ((call-with-values (λ () (make-custom-set-types (λ (x y) (eq x y)) #:name 'skip-set))
    (λ (a b c d e f g) g))))
 
 (in-rearrangements lst))

;===================================================================================================

(define (equivalence? eq lst)
 (define (eql x y) (and (eq x y) #t))
 (define in-lst (in-list lst))
 (for/and ((x in-lst))
  (and (eq x x) ; check reflexivity
   (for/and ((y in-lst))
    (define xy (eql x y))
    (and (eq? xy (eql y x)) ; check symmetry
     (or (not xy) ; check transitivity
      (for/and ((z in-lst))
       (or (not (eq y z)) (eql x z)))))))))

;===================================================================================================
; The end
;(for/list ((r (in-rearrangements (range 10) =)) (k (in-range 100))) r)
(equivalence? >= (range 100))