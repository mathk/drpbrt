#lang racket/base

(module+ test
  (require rackunit
           math/flonum
           "vector.rkt"
           (submod "vector.rkt" private-test)
   )

  (define-simple-check (check-epsilon-eq? a b)
    (and (< (- a (flulp a)) b) (> (+ a (flulp a)) b)))

  (define a (vector 1 2 3))
  (define b (vector 4 5 6))
  (define c (vector 2 2 2))
  (define i (vector 1 0 0))
  (define j (vector 0 1 0))
  (define k (vector 0 0 1))
  (test-case "Vector add"
             (define sum (vector-add a b))
             (check-eq? (vector-x sum) 5)
             (check-eq? (vector-y sum) 7)
             (check-eq? (vector-z sum) 9))
  (test-case "Vector sub"
             (define sub (vector-sub a b))
             (check-eq? (vector-x sub) -3)
             (check-eq? (vector-y sub) -3)
             (check-eq? (vector-z sub) -3))
  (test-case "Vector dot"
             (define dot (vector-dot a b))
             (check-eq? dot 32))
  (test-case "Vector magnitude"
             (define mag (vector-magnitude c))
             (check-epsilon-eq? mag (sqrt 12)))
  (test-case "Vector cross"
             (check-equal? (vector-cross i j) k)
             (check-equal? (vector-cross j k) i))
  )