#lang racket

(provide
  ;; Identity transformation
  transform-identity
  ;; Return the inverse transformation
  transform-inverse
  )

(require math/matrix
         math/array
         "vector.rkt"
         "point.rkt"
         (submod "point.rkt" internal)
         (submod "vector.rkt" internal))

(struct transform (matrix inverted))

(define transform-identity
  (let [[ident (matrix
                [[1 0 0 0]
                 [0 1 0 0]
                 [0 0 1 0]
                 [0 0 0 1]])]]
    (transform ident ident)))

(define (transform-inverse t)
  (transform (transform-inverted t) (transform-matrix t)))

(define (transform-translate v)
  (transform
    (matrix
      [[1 0 0 (vector-x v)]
       [0 1 0 (vector-y v)]
       [0 0 1 (vector-z v)]
       [0 0 0 1]])
    (matrix
      [[1 0 0 (- 0 (vector-x v))]
       [0 1 0 (- 0 (vector-y v))]
       [0 0 1 (- 0 (vector-z v))]
       [0 0 0 1]])))

(define (transform-scale x y z)
  (transform
    (matrix
      [[x 0 0 0]
       [0 y 0 0]
       [0 0 z 0]
       [0 0 0 1]])
    (matrix
      [[(/ 1 x) 0 0 0]
       [0 (/ 1 y) 0 0]
       [0 0 (/ 1 z) 0]
       [0 0 0 1]])))

(define (transform-rotate-x r)
  (transform
    (matrix
      [[1 0         0 0]
       [0 (cos r)   (- 0 (sin r)) 0]
       [0 (sin r)   (cos r) 0]
       [0 0         0 1]])
    (matrix
      [[(/ 1 x) 0 0 0]
       [0 (cos r) (sin r) 0]
       [0 (- 0 (sin r)) (cos r) 0]
       [0 0 0 1]])))

(define (transform-rotate-y r)
  (transform
    (matrix
      [[(cos r) 0 (sin r) 0]
       [0 1 0 0]
       [(- 0 (sin r)) 0 (cos r) 0]
       [0 0 0 1]])
    (matrix
      [[(cos r) 0 (- 0 (sin r)) 0]
       [0       1 0             0]
       [(sin r) 0 (cos r)       0]
       [0       0 0             1]])))

(define (transform-rotate-z r)
  (transform
    (matrix
      [[(cos r) (- 0 (sin r))   0 0]
       [(sin r) (cos r)         0 0]
       [0       0               1 0]
       [0       0               0 1]])
    (matrix
      [[(cos r)         (sin r) 0 0]
       [(-0 (sin r))    (cos r) 0 0]
       [0               0       1 0]
       [0               0       0 1]])))

(define (transform-rotate angle arround-v)
  (let* [[norm-v (vector-normalize arround-v)]
         [sina (sin angle)]
         [cosa (cos angle)]
         [vxvx (* (vector-x norm-v) (vector-x norm-v))]
         [vxvy (* (vector-x norm-v) (vector-y norm-v))]
         [vxvz (* (vector-x norm-v) (vector-z norm-v))]
         [vyvy (* (vector-y norm-v) (vector-y norm-v))]
         [vyvz (* (vector-y norm-v) (vector-z norm-v))]
         [vzvz (* (vector-z norm-v) (vector-z norm-v))]
         [one-c (- 1 cosa)]
         [one-vxvx*c (* (- 1 vxvx) cosa)]
         [one-vyvy*c (* (- 1 vyvy) cosa)]
         [one-vzvz*c (* (- 1 vzvz) cosa)]
         [vxs (* (vector-x norm-v) sina)]
         [vys (* (vector-y norm-v) sina)]
         [vzs (* (vector-z norm-v) sina)]
         ]


(define (transform-point-apply t p)
  (let* [[p-in (matrix [[(point-x p)] [(point-y p)] [(point-z p)] [1]])]
         [p-out (matrix* (transform-matrix t) p-in)]
         [w-out (array-ref p-out #(3 0))]]
    (point
      (/ (array-ref p-out #(0 0)) w-out)
      (/ (array-ref p-out #(1 0)) w-out)
      (/ (array-ref p-out #(2 0)) w-out))))

