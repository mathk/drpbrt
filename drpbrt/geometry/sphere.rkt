#lang racket

(provide 
  (contract-out
    ;; Create a sphere shape
    [sphere-init 
      (->i (
            [radius number?]
            [z-min (radius) (>=/c (- radius))]
            [z-max (radius z-min) (between/c z-min radius)]
            [phi-max (between/c 0.0 (* 2 pi))])
           [result sphere?])])
  sphere-intersect-hit
  sphere-quadratic-hit)

(require math
         racket/trace
         "point.rkt"
         "ray.rkt"
         "vector.rkt"
         "bounding-box.rkt"
         "shape-utils.rkt"
         (submod "point.rkt" internal))

;; Define a sphere shape.
;; The sphere always assume the center to be <0, 0, 0>.
;; z-min and z-max cut the   along the z axis.
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
  (sphere radius z-min z-max (acos (/ z-max radius)) (acos (/ z-min radius)) phi-max))

(define (sphere-bbox s)
  (let* ([radius (sphere-radius s)]
         [radius-inv (- 0 radius)]
         [phi (sphere-phi-max s)]
         [min-y (cond 
                  [(<= (* pi 3/2) phi) radius-inv]
                  [(< pi phi (* pi 3/2)) (* radius (sin phi))]
                  [else 0])]
         [max-y (cond 
                  [(< phi (* 1/2 pi)) (* radius (sin phi))]
                  [else radius])]
         [max-x radius]
         [min-x (cond
                  [(and (< (* 1/2 pi) phi) (< phi pi)) (* radius (cos phi))]
                  [(<= phi (* 1/2 pi)) 0]
                  [else radius-inv])])
    (bbox-from-two-point 
      (point min-x min-y (sphere-z-min s))
      (point max-x max-y (sphere-z-max s)))))

(define (sphere-quadratic-hit s ray)
  (let*-values
    ([(d) (ray-direction ray)]
     [(dx dy dz) (vector-values d)]
     [(ox oy oz) (point-values (ray-origin ray))]
     [(a) (vector-square-magnitude d)]
     [(b) (* 2 (+ (* dx ox) (* dy oy) (* dz oz)))]
     [(c) (- (+ (sqr ox) (sqr oy) (sqr oz)) (sqr (sphere-radius s)))])
    (quadratic-solve-in-range a b c ray)))

(define (sphere-intersect-hit s ray)
  (let-values
    ;; Get the first hit point
    ([(hit0 hit1) (sphere-quadratic-hit s ray)])
    (if (and hit0 (sphere-hit-in? s ray hit0))
      hit0
      (if (and hit1 (sphere-hit-in? s ray hit1))
        hit1
        #f))))

(define (sphere-hit-in? s ray t)
  (let* ([p (ray-at ray t)]
         [pz (point-z p)]
         [px (point-x p)]
         [py (point-y p)]
         [z-min (sphere-z-min s)]
         [z-max (sphere-z-max s)]
         [phi-max (sphere-phi-max s)])
    (if (not (< z-min pz z-max))
      #f
      (< (atan py px) phi-max))))

(module* plot #f
  (provide sphere-plot
           sphere-renderer)

  (require plot
           plot/utils)

  (define (sphere-renderer s)
    (let [[bbox (sphere-bbox s)]]
      (polar3d 
        (lambda (theta phi)
          (cond
            [(> theta (sphere-phi-max s)) (- 0 (sphere-radius s))]
            [else (sphere-radius s)]))
        #:z-min (bbox-min-z bbox)
        #:z-max (bbox-max-z bbox)
        #:y-min (bbox-min-y bbox)
        #:y-max (bbox-max-y bbox)
        #:x-min (bbox-min-x bbox)
        #:x-max (bbox-max-x bbox))))

  (define (sphere-plot s)
    (plot3d (sphere-renderer s)))
  )


(module* main #f
  (define mys (sphere-init 9 -8 8 (* 2/3 pi)))
  (require
    racket/trace
    plot
    "ray.rkt"
    (submod ".." plot)
    (submod "bounding-box.rkt" plot))

  (define sphere (sphere-init 3.2 -2 2 (* 2 pi)))
  sphere
  (define ray-straight (ray-simple (point 0 0 0) (vector 1 1 0)))
  (sphere-quadratic-hit sphere ray-straight)
  (sphere-intersect-hit sphere ray-straight)
  )
