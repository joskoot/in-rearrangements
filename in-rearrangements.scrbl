#lang scribble/manual

@(require
  scribble/core
  scribble/eval
  racket
  scribble/html-properties
  "in-rearrangements.rkt"
  (for-label
   "in-rearrangements.rkt"
   racket
   (only-in typed/racket Setof Exact-Nonnegative-Integer Sequenceof))
  (for-syntax racket))

@(define (underline . es)
  (define ul-attrs '((style . "text-decoration: underline")))
  (apply elem es #:style (style #f (list (attributes ul-attrs)))))

@(define-syntax-rule (rack x ...) (nonbreaking(racket x ...)))

@(define (make-color-style color)
  (define prop:color (color-property color))
  (define color-style (style #f (list prop:color)))
  (λ (elem) (element color-style elem)))

@(define red (make-color-style "red"))
@(define green (make-color-style "green"))

@(define example-ns (make-base-namespace))

@(parameterize ((current-namespace example-ns))
  (namespace-require 'racket)
  (namespace-require '"in-rearrangements.rkt"))

@(define-syntax-rule (eval-example expr)
  (nonbreaking (element 'tt (begin (random-seed 0) (~s (eval 'expr example-ns))))))

@(define-syntax (example stx)
  (syntax-case stx ()
   ((_ a)
  #'(begin
     (rack a)
     (hspace 1)
     "→"
     (hspace 1)
     (eval-example a)
     (linebreak)))))

@(define-syntax (color-example stx)
  (syntax-case stx ()
   ((_ color a)
  #'(begin
     (rack a)
     (hspace 1)
     "→"
     (hspace 1)
     (elem #:style 'tt (color (eval-example a)))
     (linebreak)))))

@(define-syntax (example/n stx)
  (syntax-case stx ()
   ((_ a)
  #'(begin
     (rack a)
     (hspace 1)
     "→"
     (linebreak)
     (eval-example a)))))

@(define-syntax-rule (Tabular ((e ...) ...) . rest)
  (tabular (list (list e ...) ...) . rest))

@(define-syntax-rule (example-table x ...)
  (Tabular ((@rack[x] "→" (eval-example x)) ...) #:sep (hspace 1)))

@(define(minus) (element 'tt "-"))
@(define(-?) (element "roman" ?-))
@(define (note . x) (inset (apply smaller x)))
@(define (inset . x) (apply nested #:style 'inset x))
@(define-syntax-rule (linespacebreak) @↑{@(hspace 1)@(linebreak)})
@(define (expt-1) @↑{@(minus)1})
@(define ↑ superscript)
@(define ↓ subscript)
        
@title[#:version ""]{in-rearrangements}
@author{Jacob J. A. Koot}
@(defmodule "in-rearrangements.rkt" #:packages ())

@defproc[(in-rearrangements (lst list?) (eq (any/c any/c . -> . any/c) equal?)) (Sequenceof list?)]{
Returns a sequence of all distinct rearrangements of @rack[lst].
A rearrangement of a list is a list of the same elements but in arbitrary order,
the initial order in @rack[lst] included.
@rack[eq] must be an equivalence relation for the elements of @rack[lst].
What @rack[eq] does with other arguments than elements of @rack[lst] is irrelevant.
Two rearrangements are distinct if and only if @rack[eq] returns @rack[#f]
for at least one pair of corresponding elements.}

Examples:

@interaction[
(require racket "in-rearrangements.rkt")
(define (example lst (eq equal?))
 (for ((x (in-rearrangements lst eq)))
  (printf "~s~n" x)))
(example '())
(example '(a))
(example '(a b c))
(example '(a a b))
(define aab (list (list 'a) (list 'a) (list 'b)))
(example aab equal?)
(example aab eq?)
(example (list '(a 1) '(a 2) '(b 3)) (λ (x y) (eq? (car x) (car y))))
(example '(0 1 2 3) (λ (x y) (eq? (even? x) (even? y))))
(example '(0 1 2 3) (λ (x y) (zero? (modulo (- x y) 3))))]

@defproc[ #:kind "predicate" (equivalence? (eq (-> any/c any/c)) (lst list?)) boolean?]{
Procedure @rack[in-rearrangements]
does not check @rack[eq] to be an equivalence relation for the elements of @rack[lst].
Predicate @rack[equivalence?] can be used to check whether or not a given procedure @rack[eq]
can be used as an equivalence relation for the elements of a given @rack[lst].

Examples:}

@example[(equivalence? = (range 100))]
@example[(equivalence? >= (range 100))]
@example[(equivalence? (λ (x y) (eq? (even? x) (even? y))) (range 100))]
@example[(equivalence? (λ (x y) (zero? (modulo (- x y) 5))) (range 100))]

@bold{@larger{@larger{The end}}}
