#lang racket/base

(module+ test
  (require rackunit
           math/flonum
           "vector.rkt"
           "point.rkt"
   )

  (define-simple-check (check-epsilon-eq? a b)
    (and (< (- a (flulp a)) b) (> (+ a (flulp a)) b)))

  (define a (vector 1 2 3))
  (define b (vector 4 5 6))
  (define c (vector 2 2 2))
  (define i (vector 1 0 0))
  (define j (vector 0 1 0))
  (define k (vector 0 0 1))
  (define o (point 0 0 0))
  (define p1 (point 1 2 3))
  (define p2 (point 2 2 2))
  (test-case "Vector add"
             (check-equal? (vector-add a b) (vector 5 7 9)))
  (test-case "Vector sub"
             (check-equal? (vector-sub a b) (vector -3 -3 -3)))
  (test-case "Vector time"
             (check-equal? (vector-time a 2) (vector 2 4 6)))
  (test-case "Vector divide"
             (check-equal? (vector-divide b 2) (vector 2 (/ 5 2) 3)))
  (test-case "Vector dot"
             (check-eq? (vector-dot a b) 32))
  (test-case "Vector magnitude"
             (check-epsilon-eq? (vector-magnitude c) (sqrt 12)))
  (test-case "Vector cross"
             (check-equal? (vector-cross i j) k)
             (check-equal? (vector-cross j k) i)
             (check-equal? (vector-cross k i) j)
             (check-not-equal? (vector-cross i k) j))
  (test-case "Point move forward"
             (check-equal? (point-move-forward o a) (point 1 2 3)))
  (test-case "Point move backward"
             (check-equal? (point-move-forward o a) (point 1 2 3)))
  (test-case "Point direction"
             (check-equal? (point-direction p1) (vector 1 2 3))
             (check-equal? (point-direction p2 p1) (vector 1 0 -1)))
  )