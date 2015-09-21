#lang racket


(provide 
  (contract-out
    ;; Create a sphere shape
    [sphere-init 
      (->i (
            [radius number?]
            [z-max (radius) (<=/c radius)]
            [z-min (z-max radius) (between/c (- 0 radius) z-max)]
            [phi-max (between/c 0.0 (* 2 pi))])
           [result sphere?])]))

(require
  math)

;; Define a sphere shape.
;; The sphere always assume the center to be <0, 0, 0>.
;; z-min and z-max cut the sphere along the z axis.
;; phi max is the cut off angle starting from the x axis around z.
(struct sphere (radius z-min z-max theta-min theta-max phi-max)
        #:methods gen:custom-write
        [(define (write-proc s port mode)
           (let ([print (if mode write display)])
             (write-string "(r=" port)
             (print (sphere-radius s) port)
             (write-string ", z-min=" port)
             (print (sphere-z-min s) port)
             (write-string ", z-max=" port)
             (print (sphere-z-max s) port)
             (write-string ", θmin=" port)
             (print (sphere-theta-min s) port)
             (write-string ", θmax=" port)
             (print (sphere-theta-max s) port)
             (write-string ", φmax=" port)
             (print (sphere-phi-max s) port)
             (write-string ")" port)))]

        )


(define (sphere-init radius z-min z-max phi-max)
  (sphere radius z-min z-max (acos (/ z-min radius)) (acos (/ z-max radius)) phi-max))


(module* plot #f
  (provide sphere-plot
           sphere-renderer)

  (require plot
           plot/utils)

  (define (sphere-renderer s)
    (polar3d (lambda (theta phi) 
               (cond [(< (- (sphere-theta-max s) (/ pi 2)) theta (- (sphere-theta-min s) (/ pi 2))) 0]
                     [(< 0 phi (sphere-phi-max s)) 0]
                     [else (sphere-radius s)]))))

  (define (sphere-plot s)
    (plot3d (sphere-renderer s)))
  )
