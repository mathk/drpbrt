#lang racket/base

(provide 
  ;; Create a bounding box from 2 point
  bbox-from-two-point
  ;; Return a new bounding box that wil englobe a given point
  bbox-union-point
  ;; Return a new bounding box that wil englobe an other bounding box
  bbox-union-bbox
  )

(require "point.rkt"
         (submod "point.rkt" internal))

(struct bbox (min-p max-p))

;; Given 2 point and a zip function it compute the resulting point
;; when applying the zip function to the respective coordinate point.
(define (zip-point zip p1 p2)
  (point
      (zip (point-x p1) (point-x p2))
      (zip (point-y p1) (point-y p2))
      (zip (point-z p1) (point-z p2))))

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

(module* internal #f
  (provide 
    bbox-min-p
    bbox-max-p))
