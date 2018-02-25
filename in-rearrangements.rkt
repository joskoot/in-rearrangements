#lang racket

(provide in-rearrangements)

(require (only-in racket/generator in-generator yield))

(define (in-rearrangements lst (eq equal?))
 (in-generator
  (cond
   ((null? lst) (yield '()))
   (else
    (for ((rotation (in-rotations lst eq)))
     (define element (car rotation))
     (for ((rearrangement-of-cdr (in-rearrangements (cdr rotation) eq)))
      (yield (cons element rearrangement-of-cdr))))))))

(define (in-rotations lst eq)
 (in-generator
  (let loop ((head lst) (tail '()))
   (unless (null? head)
    (define element (car head))
    (define new-tail (cons element tail))
    (define new-head (cdr head))
    (cond
     ((member element tail eq)
      (loop new-head new-tail))
     (else
      (yield (append head (reverse tail)))
      (loop new-head new-tail)))))))

