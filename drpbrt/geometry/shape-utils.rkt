#lang racket

(provide quadratic-solve-in-range
         quadratic-solve)

(require 
  "ray.rkt")

;; Return the solutions of a quadratic equation.
;; If it has no solution then the
(define (quadratic-solve a b c)
  (let ([delta (- (sqr b) (* 4 a c))])
    (if (< delta 0)
      (values #f #f)
      (let* [[quad (sqrt delta)]
             [q (* -1/2 (+ b quad))]
             [t0 (/ q a)]
             [t1 (/ c q)]]
        (values (min t0 t1) (max t0 t1))))))

(define (quadratic-solve-in-range a b c ray)
  (let-values ([(t0 t1) (quadratic-solve a b c)])
    (ray-in-range? ray t0 t1)))
