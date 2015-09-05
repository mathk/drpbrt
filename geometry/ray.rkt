#lang racket/base

(provide
 ;; Create a ray data
 ray
 ;; Create a ray with minimum information
 ray-simple
 ;; Get the point allong the ray at given parametric value
 ray-at
 )

(require math/flonum
         "vector.rkt"
         "point.rkt")

(struct ray (origin direction mint maxt bounce-count))

(define (ray-simple o d) (ray o d 0 +max.0 0))

(define (ray-at r t) (point-move-forward (ray-origin r) (vector-time (ray-direction r) t)))
