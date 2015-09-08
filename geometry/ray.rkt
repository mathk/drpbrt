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

(module* plot #f 
  (provide ray-plot)
  (require plot
           plot/utils
           (submod "point.rkt" internal))

  (define (ray-plot r)
    (let [[plot-ray-at (lambda (t) 
                         (let [[at (ray-at r t)]]
                           (list (point-x at) (point-y at) (point-z at))))]]
    (plot3d (parametric3d plot-ray-at (ray-mint r) (min 1000000 (ray-maxt r))))))
  )
