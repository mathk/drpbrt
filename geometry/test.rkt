#lang racket/base

(module+ test
  (require rackunit
           math/flonum
           "vector.rkt"
           "point.rkt"
           "ray.rkt"
           "bounding-box.rkt"
           (submod "bounding-box.rkt" internal)
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
  (define ray (ray-simple o j))
  (define bUnit (bbox-from-two-point (point 0 0 0) (point 1 1 1)))
  (define bUnitOverlaps (bbox-from-two-point (point 0.5 0 0) (point -1 -1 -1)))
  (define bUnitOut (bbox-from-two-point (point 1.5 1.6 1.1) (point 2 2.3 2.5)))
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
             (check-eq? (vector-square-magnitude c) 12)
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
  (test-case "Point distance"
             (check-epsilon-eq? (point-distance p1 o) (sqrt 14))
             (check-eq? (point-square-distance p1 o) 14))
  (test-case "Ray at"
             (check-equal? (ray-at ray 1) (point 0 1 0))
             (check-equal? (ray-at ray 1.5) (point 0 1.5 0)))
  (test-case "Bonding box creation"
             (define bbox (bbox-from-two-point (point -1 4 -0.5) (point 5 -3 9)))
             (check-equal? (bbox-min-p bbox) (point -1 -3 -0.5))
             (check-equal? (bbox-max-p bbox) (point 5 4 9.0)))
  (test-case "Bonding box union"
             (define b1 (bbox-from-two-point (point 0 1 0) (point 4 5 7)))
             (define b2 (bbox-from-two-point (point 1 0 1) (point 8 2 9)))
             (define bUnionB (bbox-union-bbox  b1 b2))
             (define bUnionP (bbox-union-point b1 (point -1 7 -4)))
             (check-equal? (bbox-min-p bUnionP) (point -1 1 -4))
             (check-equal? (bbox-max-p bUnionP) (point 4 7 7))
             (check-equal? (bbox-min-p bUnionB) (point 0 0 0))
             (check-equal? (bbox-max-p bUnionB) (point 8 5 9)))
  (test-case "Bonding box overlaps"
             (check-true (bbox-overlaps? bUnit bUnitOverlaps))
             (check-false (bbox-overlaps? bUnit bUnitOut)))
  (test-case "Bonding box inside"
             (check-true (bbox-inside? bUnit (point 0 0.5 0.9)))
             (check-true (bbox-inside? bUnit (point 0.4 0.5 0.9)))
             (check-false (bbox-inside? bUnit (point 0 0.5 1.9))))
  (test-case "Bonding box expand"
             (define bExpand (bbox-expand bUnit 0.5))
             (check-equal? (bbox-min-p bExpand) (point -0.5 -0.5 -0.5))
             (check-equal? (bbox-max-p bExpand) (point 1.5 1.5 1.5)))
  (test-case "Bounding box max extent"
             (check-eq? (bbox-max-extent bUnitOverlaps) 'x))
)
