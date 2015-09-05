#lang rackt/base

(provide 
  ;; Create a bounding box from 2 point
  bbox-from-two-point
  ;; Return a new bounding box that wil englobe a given point
  bbox-union-point
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

(define (bbox-union-point bbox p)
  (bbox 
    (point
      (min (point-x (bbox-min-p bbox)) (point-x p))
      (min (point-y (bbox-min-p bbox)) (point-y p))
      (min (point-z (bbox-min-p bbox)) (point-z p)))
    (point
      (max (point-x (bbox-max-p bbox)) (point-x p))
      (max (point-y (bbox-max-p bbox)) (point-y p))
      (max (point-z (bbox-max-p bbox)) (point-z p)))
    ))
