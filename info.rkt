#lang info
(define collection 'multi)
(define deps '("base"
               "rackunit-lib"))
(define build-deps '("scribble-lib" "racket-doc"))
(define scribblings '(("scribblings/pbrt-racket.scrbl" ())))
(define pkg-desc "Attempt to write pbrt using racket")
(define version "0.0")
(define pkg-authors '(msuen))
