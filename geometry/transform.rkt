#lang racket

(provide
  ;; Identity transformation
  transform-identity
  ;; Return the inverse transformation
  transform-inverse
  ;; Create a rotation transformation around a given vector
  transform-rotate
  ;; Create a rotation transformation around x axis
  transform-rotate-x
  ;; Create a rotation transformation around y axis
  transform-rotate-y
  ;; Create a rotation transformation around z axis
  transform-rotate-z
  ;; Create a scale transformation
  transform-scale
  ;; Create a translate transformation
  transform-translate
  ;; Create a look at transformation
  transform-look-at
  ;; Apply a transformation to a vector
  transform-vector-apply
  ;; Apply transformation to a point
  transform-point-apply
  ;; Apply transformation to a normal vector
  transform-normal-apply
  ;; Apply transformation to a ray
  transform-ray-apply
  ;; Apply a transformation to a bbox
  transform-bbox-apply
  ;; Compose two transformation together
  transform-compose
  )

(require math/matrix
         math/array
         "vector.rkt"
         "point.rkt"
         "ray.rkt"
         "bounding-box.rkt"
         (submod "point.rkt" internal)
         (submod "ray.rkt" internal)
         (submod "bounding-box.rkt" internal)
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
      [[(/ 1 x) 0       0       0]
       [0       (/ 1 y) 0       0]
       [0       0       (/ 1 z) 0]
       [0       0       0       1]])))

(define (transform-rotate-x r)
  (transform
    (matrix
      [[1 0         0               0]
       [0 (cos r)   (- 0 (sin r))   0]
       [0 (sin r)   (cos r)         0]
       [0 0         0               1]])
    (matrix
      [[0 0             0       0]
       [0 (cos r)       (sin r) 0]
       [0 (- 0 (sin r)) (cos r) 0]
       [0 0             0       1]])))

(define (transform-rotate-y r)
  (transform
    (matrix
      [[(cos r)         0 (sin r)   0]
       [0               1 0         0]
       [(- 0 (sin r))   0 (cos r)   0]
       [0               0 0         1]])
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
  (let* [[norm-v (vector-normalized arround-v)]
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
         [vxvy*one-c (* vxvy one-c)]
         [vxvz*one-c (* vxvz one-c)]
         [vyvz*one-c (* vyvz one-c)]
         [mat (matrix
                [[(+ vxvx one-vxvx*c)   (- vxvy*one-c vzs)  (+ vxvz*one-c vys)  0]
                [(+ vxvy*one-c vzs)    (+ vyvy one-vyvy*c) (- vyvz*one-c vxs)  0]
                [(- vxvz*one-c vys)    (+ vyvz*one-c vxs)  (+ vzvz one-vzvz*c) 0]
                [0                     0                   0                   1]])]]
    (transform mat (matrix-transpose mat))))

(define (transform-look-at p-pos p-look v-up)
  (let* [[dir (vector-normalized (point-distance p-look p-pos))]
         [left (vector-normalized (vector-cross v-up dir))] ; Tex book say to normalize v-up..
         [new-up (vector-cross dir left)]
         [mat (matrix 
                [[(vector-x left)   (vector-x new-up)   (vector-x dir)  (point-x p-pos)]
                 [(vector-y left)   (vector-y new-up)   (vector-y dir)  (point-y p-pos)]
                 [(vector-z left)   (vector-z new-up)   (vector-z dir)  (point-z p-pos)]
                 [0                 0                   0               1]])]]
    (transform mat (matrix-transpose mat))))

(define (transform-vector-apply t v)
  (let* [[v-in (matrix [[(vector-x v)] [(vector-y v)] [(vector-z v)] [0]])]
         [v-out (matrix* (transform-matrix t) v-in)]]
    (vector 
      (array-ref v-out #(0 0))
      (array-ref v-out #(1 0))
      (array-ref v-out #(2 0)))))

(define (transform-normal-apply t v)
  (let* [[v-in (matrix [[(vector-x v) (vector-y v) (vector-z v) 0]])]
         [v-out (matrix* v-in (transform-inverted t))]]
    (vector
      (array-ref v-out #(0 0))
      (array-ref v-out #(0 1))
      (array-ref v-out #(0 2)))))

(define (transform-ray-apply t r)
  (ray-new-origin-direction
    r
    (transform-point-apply t (ray-origin r))
    (transform-vector-apply t (ray-direction r))))

(define (transform-bbox-apply t b)
  (let* [[in-min (bbox-min-p b)]
         [in-max (bbox-max-p b)]
         [first-row (matrix-col (transform-matrix t) 0)]
         [second-row (matrix-col (transform-matrix t) 1)]
         [third-row (matrix-col (transform-matrix t) 2)]
         [min-first-row (matrix* first-row (matrix [[(point-x in-min)]]))]
         [max-first-row (matrix* first-row (matrix [[(point-x in-max)]]))]
         [min-second-row (matrix* second-row (matrix [[(point-y in-min)]]))]
         [max-second-row (matrix* second-row (matrix [[(point-y in-max)]]))]
         [min-third-row (matrix* third-row (matrix [[(point-z in-min)]]))]
         [max-third-row (matrix* third-row (matrix [[(point-z in-max)]]))]]
    (bbox-from-two-point
      (point
        (+
          (min (array-ref min-first-row #(0 0)) (array-ref max-first-row #(0 0)))
          (min (array-ref min-second-row #(0 0)) (array-ref max-second-row #(0 0)))
          (min (array-ref min-third-row #(0 0)) (array-ref max-third-row #(0 0))))
        (+
          (min (array-ref min-first-row #(1 0)) (array-ref max-first-row #(1 0)))
          (min (array-ref min-second-row #(1 0)) (array-ref max-second-row #(1 0)))
          (min (array-ref min-third-row #(1 0)) (array-ref max-third-row #(1 0))))
        (+
          (min (array-ref min-first-row #(2 0)) (array-ref max-first-row #(2 0)))
          (min (array-ref min-second-row #(2 0)) (array-ref max-second-row #(2 0)))
          (min (array-ref min-third-row #(2 0)) (array-ref max-third-row #(2 0)))))
      (point
        (+
          (max (array-ref min-first-row #(0 0)) (array-ref max-first-row #(0 0)))
          (max (array-ref min-second-row #(0 0)) (array-ref max-second-row #(0 0)))
          (max (array-ref min-third-row #(0 0)) (array-ref max-third-row #(0 0))))
        (+
          (max (array-ref min-first-row #(1 0)) (array-ref max-first-row #(1 0)))
          (max (array-ref min-second-row #(1 0)) (array-ref max-second-row #(1 0)))
          (max (array-ref min-third-row #(1 0)) (array-ref max-third-row #(1 0))))
        (+
          (max (array-ref min-first-row #(2 0)) (array-ref max-first-row #(2 0)))
          (max (array-ref min-second-row #(2 0)) (array-ref max-second-row #(2 0)))
          (max (array-ref min-third-row #(2 0)) (array-ref max-third-row #(2 0))))))))

(define (transform-compose t1 t2)
  (transform 
    (matrix* (trasnform-matrix t1) (transform-matrix t2))
    (matrix* (trasnform-inverted t2) (transform-inverted t1))))

(define (transform-point-apply t p)
  (let* [[p-in (matrix [[(point-x p)] [(point-y p)] [(point-z p)] [1]])]
         [p-out (matrix* (transform-matrix t) p-in)]
         [w-out (array-ref p-out #(3 0))]]
    (point
      (/ (array-ref p-out #(0 0)) w-out)
      (/ (array-ref p-out #(1 0)) w-out)
      (/ (array-ref p-out #(2 0)) w-out))))

