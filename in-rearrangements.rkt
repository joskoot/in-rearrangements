#lang racket

(provide in-rearrangements)

(require (only-in racket/generator in-generator yield))

(define (in-rearrangements lst (eq equal?))
 (define (in-rearrangements lst)
  (in-generator
   (cond
    ((null? lst) (yield '()))
    (else
     (for ((rotation (in-rotations lst eq)))
      (define element (car rotation))
      (for ((rearrangement (in-rearrangements (cdr rotation))))
       (yield (cons element rearrangement))))))))
 (in-rearrangements lst))

(define (in-rotations lst eq)
 (in-generator
  (let loop ((head lst) (tail '()))
   (unless (null? head)
    (define element (car head))
    (cond
     ((member element tail eq)
      (loop (cdr head) (cons element tail)))
     (else
      (yield (append head (reverse tail)))
      (loop (cdr head) (cons element tail))))))))

