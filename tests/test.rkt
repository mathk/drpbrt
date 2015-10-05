#lang racket

(module+ test
  (require rackunit
           math
           math/flonum
           "../drpbrt/geometry/vector.rkt"
           "../drpbrt/geometry/point.rkt"
           "../drpbrt/geometry/ray.rkt"
           "../drpbrt/geometry/bounding-box.rkt"
           "../drpbrt/geometry/transform.rkt"
           "../drpbrt/geometry/sphere.rkt"
           (submod "../drpbrt/geometry/bounding-box.rkt" internal)
           (submod "../drpbrt/geometry/point.rkt" internal)
   )

  (define (almost-equal? epsilon a b )
    (<= (absolute-error a b) epsilon))

  (define-simple-check (check-almost-equal?? epsilon a b)
                       (almost-equal? epsilon a b))

  (define-simple-check (check-point-almost-equal? epsilon a b)
                       (equal?/recur a b (curry almost-equal? epsilon)))


  (define a (vector 1 2 3))
  (define b (vector 4 5 6))
  (define c (vector 2 2 2))
  (define i (vector 1 0 0))
  (define j (vector 0 1 0))
  (define k (vector 0 0 1))
  (define o (point 0 0 0))
  (define p1 (point 1 2 3))
  (define p2 (point 2 2 2))
  (define ray-s (ray-simple o j))
  (define ray-small (ray o j 0 10 0))
  (define bUnit (bbox-from-two-point (point 0 0 0) (point 1 1 1)))
  (define bBig (bbox-from-two-point (point 0 0 0) (point 4 4 4)))
  (define bUnitOverlaps (bbox-from-two-point (point 0.5 0 0) (point -1 -1 -1)))
  (define bUnitOut (bbox-from-two-point (point 1.5 1.6 1.1) (point 2 2.3 2.5)))
  (define scale (transform-scale 1 2 1))
  (define rotate-z (transform-rotate-z (/ pi 2)))
  (define reverse-scale (transform-scale 1 -1 1))
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
             (check-almost-equal?? 1.0e-10 (vector-magnitude c) (sqrt 12)))
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
             (check-almost-equal?? 1.0e-10 (point-distance p1 o) (sqrt 14))
             (check-eq? (point-square-distance p1 o) 14))
  (test-case "Ray in range"
             (check-true (ray-in-range ray-small 9.2))
             (check-false (ray-in-range ray-small -2))
             (check-false (ray-in-range ray-small 11.2)))
  (test-case "Ray at"
             (check-equal? (ray-at ray-s 1) (point 0 1 0))
             (check-equal? (ray-at ray-s 1.5) (point 0 1.5 0)))
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
  (test-case "Bouding box offset"
             (check-equal? (bbox-offset bBig (point 2 2 2)) (vector 1/2 1/2 1/2)))
  (test-case "Bouding box location"
             (check-equal? (bbox-location bBig (vector 1/2 1/2 1/2)) (point 2 2 2)))
  (test-case "Point middle"
             (check-equal? (point-middle o p2) (point 1 1 1)))
  (test-case "Bounding box sphere"
             (define-values (center radius) (bbox-enclosing-sphere bUnit))
             (check-equal?  center (point 1/2 1/2 1/2))
             (check-almost-equal?? 1.0e-10  radius (sqrt 0.75)))
  (test-case "Transform scale"
             (define scale-bbox (transform-bbox-apply scale bUnit))
             (check-equal? (transform-point-apply scale (point 1 2 3)) (point 1 4 3))
             (check-equal? (transform-normal-apply scale (vector 1 1 1)) (vector 1 1/2 1))
             (check-equal? (bbox-min-p scale-bbox) (point 0 0 0))
             (check-equal? (bbox-max-p scale-bbox) (point 1 2 1)))
  (test-case "Transform compose"
             (define t-compose (transform-compose rotate-z scale))
             (define p (transform-point-apply t-compose (point 0 1 0)))
             (check-point-almost-equal? 1.0e-10 (transform-point-apply t-compose (point 0 1 0)) (point -2.0 0.0 0)))
  (test-case "Transform handedness"
             (check-true (transform-swap-handedness? reverse-scale))
             (check-false (transform-swap-handedness? scale)))
)
