#lang racket

(provide (contract-out
  ;; Create a bounding box from 2 point
  [bbox-from-two-point (-> point? point? bbox?)]
  ;; Return a new bounding box that wil englobe a given point
  [bbox-union-point (-> bbox? point? bbox?)])
  ;; Return a new bounding box that wil englobe an other bounding box
  bbox-union-bbox
  ;; Check if two bounding box overlaps
  bbox-overlaps?
  ;; Check if a given point is inside a bounding box.
  bbox-inside?
  ;; Expand a bouding box by a constant factor
  bbox-expand
  ;; Surface of thes bounding box
  bbox-surface
  ;; Volume of the bounding box
  bbox-volume
  ;; Bounding box axis maximum extent
  bbox-max-extent
  ;; Return the point given a vector. Vector is interpret as the unit coordinate within the bounding box
  bbox-location
  ;; Return the vector given a point in the bounding box. The vector return is the relative coordinate considering the bounding box as a box of length one.
  bbox-offset
  ;; Return the sphere that enclose the bounding box
  bbox-enclosing-sphere
  bbox-min-x
  bbox-max-x
  bbox-min-y
  bbox-max-y
  bbox-min-z
  bbox-max-z
  )

(require "point.rkt"
         "vector.rkt"
         (submod "vector.rkt" internal)
         (submod "point.rkt" internal))


;; Utility function that calculate linear interpolation
(define (lerp t a b) (+ (* a (- 1 t)) (* t b)))
;; Utility function that calculate the converse of lerp
(define (lerp-1 v a b) (/ (- v a) (- b a)))

(struct bbox (min-p max-p))

(struct sphere (center radius))


(define (bbox-min-x b) (point-x (bbox-min-p b)))
(define (bbox-max-x b) (point-x (bbox-max-p b)))
(define (bbox-min-y b) (point-y (bbox-min-p b)))
(define (bbox-max-y b) (point-y (bbox-max-p b)))
(define (bbox-min-z b) (point-z (bbox-min-p b)))
(define (bbox-max-z b) (point-z (bbox-max-p b)))

(define (zip-reduce-point zip reduce p1 p2)
  (reduce
      (zip (point-x p1) (point-x p2))
      (zip (point-y p1) (point-y p2))
      (zip (point-z p1) (point-z p2))))

;; Given 2 point and a zip function it compute the resulting point
;; when applying the zip function to the respective coordinate point.
(define (zip-point zip p1 p2)
  (zip-reduce-point zip point p1 p2))

;; Given two point compute the point that have the lowest
;; coordinate value
(define (min-point p1 p2) (zip-point min p1 p2))

;; Given two point compute the point that have the largest
;; coordinate value
(define (max-point p1 p2) (zip-point max p1 p2))

(define (bbox-from-two-point p1 p2)
  (bbox (min-point p1 p2) (max-point p1 p2)))

(define (bbox-union-point b p)
  (bbox
    (min-point (bbox-min-p b) p)
    (max-point (bbox-max-p b) p)))

(define (bbox-union-bbox b1 b2)
  (bbox
    (min-point (bbox-min-p b1) (bbox-min-p b2))
    (max-point (bbox-max-p b1) (bbox-max-p b2))))

(define (bbox-overlaps? b1 b2)
  (let [[expanded-and (lambda (cord1 cord2 cord3) (and cord1 cord2 cord3))]]
    (and
      (zip-reduce-point >= expanded-and (bbox-max-p b1) (bbox-min-p b2))
      (zip-reduce-point <= expanded-and (bbox-min-p b1) (bbox-max-p b2)))))

(define (bbox-inside? b p1)
  (let [[expanded-and (lambda (cord1 cord2 cord3) (and cord1 cord2 cord3))]]
    (and
      (zip-reduce-point >= expanded-and p1 (bbox-min-p b))
      (zip-reduce-point <= expanded-and p1 (bbox-max-p b)))))

(define (bbox-expand b delta)
  (bbox
    (point-move-backward (bbox-min-p b) (vector delta delta delta))
    (point-move-forward (bbox-max-p b) (vector delta delta delta))))

(define (bbox-diagonal b) (point-direction (bbox-max-p b) (bbox-min-p b)))

(define (bbox-surface b)
  (let [[diag (bbox-diagonal b)]]
    (* 2 (+
           (* (vector-x diag) (vector-y diag))
           (* (vector-x diag) (vector-z diag))
           (* (vector-y diag) (vector-z diag))))))

(define (bbox-volume b)
  (let [[diag (bbox-diagonal b)]]
    (* (vector-x diag) (vector-y diag) (vector-z diag))))

(define (bbox-max-extent b)
  (let [[diag (bbox-diagonal b)]]
    (if (and (> (vector-x diag) (vector-y diag))
             (> (vector-x diag) (vector-z diag)))
      'x
      (if (> (vector-y diag) (vector-z diag))
        'y
        'z))))

(define (bbox-location b v)
  (point (lerp (vector-x v) (point-x (bbox-min-p b)) (point-x (bbox-max-p b)))
         (lerp (vector-y v) (point-y (bbox-min-p b)) (point-y (bbox-max-p b)))
         (lerp (vector-z v) (point-z (bbox-min-p b)) (point-z (bbox-max-p b)))))

(define (bbox-offset b p)
  (vector (lerp-1 (point-x p) (point-x (bbox-min-p b)) (point-x (bbox-max-p b)))
         (lerp-1 (point-y p) (point-y (bbox-min-p b)) (point-y (bbox-max-p b)))
         (lerp-1 (point-z p) (point-z (bbox-min-p b)) (point-z (bbox-max-p b)))))

(define (bbox-enclosing-sphere b)
  (let* [[center (point-middle (bbox-min-p b) (bbox-max-p b))]
         [radius (point-distance center (bbox-max-p b))]]
    (values center radius)))


(module* plot #f
  (provide
    ;; Plot a bounding box
    bbox-plot
    ;; Return a render3d? bounding box
    bbox-renderer)
  (require plot)
  (require plot/utils)

  (define (bbox-renderer b)
    (let [[pmin (bbox-min-p b)]
          [pmax (bbox-max-p b)]]
      (rectangles3d
        (list
          (list
            (ivl (point-x pmin) (point-x pmax))
            (ivl (point-y pmin) (point-y pmax))
            (ivl (point-z pmin) (point-z pmax))))
        #:alpha 3/4)))

  (define (bbox-plot b)
      (plot3d
        (bbox-renderer b)))
)

(module* internal #f
  (provide
    bbox-min-p
    bbox-max-p))
