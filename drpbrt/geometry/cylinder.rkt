#lang racket

(provide
  cylinder-init)

(require
  "ray.rkt"
  "point.rkt"
  "vector.rkt"
  "shape-utils.rkt")

;; Define a cylinder shpae
;; Cynlinder are always define centered arround
;; the z axis.
(struct cylinder (radius z-min z-max phi-max))

(define (cylinder-init radius z-min z-max phi-max)
  (cylinder radius z-min z-max phi-max))

(define (cylinder-quadratic-hit c ray)
  (let*-values
    ([(dx dy) (vector-values (ray-direction ray))]
     [(ox oy) (point-values (ray-origin ray))]
     [(a) (+ (sqr dx) (sqr dy))]
     [(b) (* 2 (+ (* dx ox) (* dy oy)))]
     [(c) (- (+ (sqr ox) (sqr oy) (sqr (cylinder-radius c))))])
    (quadratic-solve-in-range a b c ray)))

(define (cylinder-z-range-values s)
  (values (cylinder-z-min s) (cylinder-z-max s)))

;; Test if a hit point is in the range
;; of the clip shape.
(define (cylinder-hit-in? c ray t)
  (let-values ([(px py pz) (point-values (ray-at ray t))]
               [(z-min z-max) (cylinder-z-range-values c)]
               [(phi-max) (cylinder-phi-max c)])
    (if (not (< z-min pz z-max))
      #f
      (< (atan py px) phi-max))))

(define (cylinder-intersect-hit c ray)
  (let-values
    ([(hit0 hit1) (cylinder-quadratic-hit c ray)])
    (if (and hit0 (cylinder-hit-in? c ray hit0))
      hit0
      (if (and hit1 (cylinder-hit-in? c ray hit1))
        hit1
        #f))))
