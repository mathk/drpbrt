#lang racket

(provide
  sdp-at
  make-sdp-from-discret)

(struct sdp (f))

(define (linear-interpolate v a b)
  (+ (* (- 1.0 v) a) (* v b)))

(define (make-sdp-from-discret spec)
  (let ([sorted-spec (sort spec < #:key car)])
    (define (graph wave)
      (let-values ([(low hi) (splitf-at-right sorted-spec (lambda (e) (< wave (car e))))])
        (case (list (empty? low) (empty? hi))
          [((#t #t)) (raise-arguments-error 'sdp "Can not compute the spectrum power at given wavelength" "length" wave)]
          [((#f #t)) (cdr (last low))]
          [((#t #f)) (cdar hi)]
          [((#f #f)) (let* ([left-l (car (last low))]
                          [left-p (cdr (last low))]
                          [right-l (caar hi)]
                          [right-p (cdar hi)]
                          [v (/ (- wave left-l) (- right-l left-l))])
                     (linear-interpolate v left-p right-p))])))
    (sdp graph)))

(define (sdp-at s w)
  ((sdp-f s) w))
