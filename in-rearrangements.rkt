#lang racket

(require (only-in racket/generator in-generator yield))
(provide in-rearrangements equivalence?)

;===================================================================================================

(define (make-skip-set eq)
 (call-with-values (λ () (make-custom-set-types eq #:name 'skip-set))
  (λ (a b c d e f g) g)))

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
   (let loop ((head lst) (tail '()) (skip (skip-set)))
    (unless (null? head)
     (define element (car head))
     (define new-tail (cons element tail))
     (define new-head (cdr head))
     (cond
      ((set-member? skip element)
       (loop new-head new-tail skip))
      (else
       (yield (append head (reverse tail)))
       (loop new-head new-tail (set-add skip element))))))))
 
 (define skip-set (make-skip-set eq))
 
 (in-rearrangements lst))

;===================================================================================================

(define (equivalence? eq lst)
 (define (eql x y) (and (eq x y) #t))
 (define in-lst (in-list lst))
 (for/and ((x in-lst))
  (and (eq x x)
   (for/and ((y in-lst))
    (define xy (eql x y))
    (and (eq? xy (eql y x))
     (or (not xy)
      (for/and ((z in-lst))
       (or (not (eq y z)) (eq x z)))))))))

;===================================================================================================
; The end

