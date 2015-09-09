#lang racket/base

(provide
 ;; Create a new point
 point
 ;; Return a new point forward by a given vector
 point-move-forward
 ;; Return a new point backward by a given vector
 point-move-backward
 ;; Return the vector directed from two point. Second point is optional and default to origin.
 point-direction
 ;; Return the distance between two point
 point-distance
 ;; Return the square distance between two point
 point-square-distance
 )

(require "vector.rkt"
         (submod "vector.rkt" internal))

(struct point (x y z)
  #:methods gen:custom-write
  [(define (write-proc point port mode)
     (let ([print (if mode write display)])
       (write-string "(" port)
       (print (point-x point) port)
       (write-string ", " port)
       (print (point-y point) port)
       (write-string ", " port)
       (print (point-z point) port)
       (write-string ")" port)))]
  #:methods gen:equal+hash
  [(define (equal-proc a b equal?-recur)
     (and (equal?-recur (point-x a) (point-x b))
          (equal?-recur (point-y a) (point-y b))
          (equal?-recur (point-z a) (point-z b))))
   (define (hash-proc a hash-recur)
     (+ (hash-recur (point-x a))
        (* 3 (hash-recur (point-y a)))
        (* 5 (hash-recur (point-z a)))))
   (define (hash2-proc a hash-recur)
     (- (* 3 (hash-recur (point-x a)))
        (hash-recur (point-y a))
        (hash-recur (point-z a))))])

(define (point-move op p v)
  (point
   (op (point-x p) (vector-x v))
   (op (point-y p) (vector-y v))
   (op (point-z p) (vector-z v))))

(define (point-move-forward p v) (point-move + p v))

(define (point-move-backward p v) (point-move - p v))

(define (point-direction to-point [from-point (point 0 0 0)])
  (vector (- (point-x to-point) (point-x from-point))
          (- (point-y to-point) (point-y from-point))
          (- (point-z to-point) (point-z from-point))))

(define (point-distance p1 p2) (vector-magnitude (point-direction p1 p2)))

(define (point-square-distance p1 p2) (vector-square-magnitude (point-direction p1 p2)))

(module* internal #f
  (provide point-x point-y point-z))

(module* plot #f
  (provide point-plot
           point-renderer)
  (require plot
           plot/utils)

  (define (point-renderer p)
    (points3d (list (list (point-x p) (point-y p) (point-z p)))))

  (define (point-plot p)
    (plot3d (point-renderer p))
  )
