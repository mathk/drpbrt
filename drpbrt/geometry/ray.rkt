#lang racket/base

(provide
 ;; Create a ray data
 ray
 ;; Create a ray with minimum information
 ray-simple
 ;; Get the point allong the ray at given parametric value
 ray-at
 ;; Create a new ray from one ray changing the origin and direction
 ray-new-origin-direction
 ;; Get the origin of a ray
 ray-origin
 ;; Get the direction of a ray
 ray-direction
 ;; Check if a given parametric value of the ray it is in the range.
 ray-in-range?
 )

(require math/flonum
         "vector.rkt"
         "point.rkt")

(struct ray (origin direction mint maxt bounce-count))

(define (ray-simple o d) (ray o d 0 +max.0 0))

(define (ray-new-origin-direction r o d)
  (ray o d (ray-mint r) (ray-maxt r) (ray-bounce-count r)))

(define (ray-at r t) (point-move-forward (ray-origin r) (vector-time (ray-direction r) t)))

(define (ray-in-range? r . t)
  (let ([in-range (lambda (x) (if (< (ray-mint r) x (ray-maxt r)) x #f))])
    (apply values (map in-range t))))

(module* internal #f
  (provide
    ray-mint
    ray-maxt
    ray-bounce-count))

(module* plot #f 
  (provide ray-plot
           ray-renderer)
  (require plot
           plot/utils
           (submod "point.rkt" internal))


  (define (ray-renderer r)
    (let [[plot-ray-at (lambda (t) 
                         (let [[at (ray-at r t)]]
                           (list (point-x at) (point-y at) (point-z at))))]]
      (parametric3d plot-ray-at (ray-mint r) (min 100 (ray-maxt r)))))

  (define (ray-plot r)
    (plot3d (ray-renderer r)))
  )
