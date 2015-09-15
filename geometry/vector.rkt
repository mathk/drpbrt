#lang racket/base

(provide
 ;; Vector creation
 vector
 ;; Adding 2 vectors
 vector-add
 ;; Substracting 2 vectors
 vector-sub
 ;; Compute the dot product of 2 vectors
 vector-dot
 ;; Compute the magnitude of a vector
 vector-magnitude
 ;; Compute the square magnitude of a vector. This operation is faster than the magnitude.
 vector-square-magnitude
 ;; Compute cross product of two vector
 vector-cross
 ;; Compute product of vector to a number
 vector-time
 ;; Compute division of vector to a number
 vector-divide
 ;; Return a normalize vector
 vector-normalized
 )

(require racket/list)

(struct vector (x y z)
  #:methods gen:custom-write
  [(define (write-proc vector port mode)
     (let ([print (if mode write display)])
       (write-string "<" port)
       (print (vector-x vector) port)
       (write-string ", " port)
       (print (vector-y vector) port)
       (write-string ", " port)
       (print (vector-z vector) port)
       (write-string ">" port)))]
  #:methods gen:equal+hash
  [(define (equal-proc a b equal?-recur)
     (and (equal?-recur (vector-x a) (vector-x b))
          (equal?-recur (vector-y a) (vector-y b))
          (equal?-recur (vector-z a) (vector-z b))))
   (define (hash-proc a hash-recur)
     (+ (hash-recur (vector-x a))
        (* 3 (hash-recur (vector-y a)))
        (* 5 (hash-recur (vector-z a)))))
   (define (hash2-proc a hash-recur)
     (- (* 3 (hash-recur (vector-x a)))
        (hash-recur (vector-y a))
        (hash-recur (vector-z a))))])

(define (vector-map-fold v1 v2 mapfunc foldfunc)
  (foldfunc
   (mapfunc (vector-x v1) (vector-x v2))
   (mapfunc (vector-y v1) (vector-y v2))
   (mapfunc (vector-z v1) (vector-z v2))))

(define (vector-map v1 v2 mapfunc)
  (vector-map-fold v1 v2 mapfunc vector))

(define (vector-add v1 v2) (vector-map v1 v2 +))
(define (vector-sub v1 v2) (vector-map v1 v2 -))
(define (vector-time v1 s) (vector-map v1 (vector s s s) *))
(define (vector-divide v1 s)
  (let [(invs (/ 1 s))]
    (vector-map v1 (vector invs invs invs) *)))

(define (vector-dot v1 v2) (vector-map-fold v1 v2 * +))

(define (vector-square-magnitude v) (vector-dot v v))

(define (vector-magnitude v) (sqrt (vector-square-magnitude v)))

(define (vector-normalized v) (vector-divide v (vector-magnitude v)))

(define (vector-cross v1 v2)
  (vector
   (- (* (vector-y v1) (vector-z v2))
      (* (vector-z v1) (vector-y v2)))
   (- (* (vector-z v1) (vector-x v2))
      (* (vector-x v1) (vector-z v2)))
   (- (* (vector-x v1) (vector-y v2))
      (* (vector-y v1) (vector-x v2)))))

(module* internal #f
  (provide vector-x vector-y vector-z))

(module* plot #f
  (provide vector-plot)
  (require plot
           plot/utils)

  (define (vector-renderer v [orig (list 0 0 0)])
    (let* [[endpoint (list 
                      (+ (first orig) (vector-x v))
                      (+ (second orig) (vector-y v))
                      (+ (third orig) (vector-z v)))]
           [norm-v (vector-normalized v)]
           [cross-p (vector-normalized (vector-cross norm-v
                                                     ;; Cross product should not be null vector
                                                     (if (and (= (vector-x v) 0) (= (vector-y v) 0))
                                                       (vector 0 1 1)
                                                       (vector 0 0 1))))]
           [distance (/ (vector-magnitude v) 10)]
           [right-end (vector-sub (vector-time norm-v (* distance -2)) (vector-time cross-p distance))]
           [left-end (vector-add (vector-time norm-v (* distance -2)) (vector-time cross-p distance))]
           [right-endpoint (list 
                             (+ (first endpoint) (vector-x right-end))
                             (+ (second endpoint) (vector-y right-end))
                             (+ (third endpoint) (vector-z right-end)))]
           [left-endpoint (list 
                             (+ (first endpoint) (vector-x left-end))
                             (+ (second endpoint) (vector-y left-end))
                             (+ (third endpoint) (vector-z left-end)))]]
    (list 
      (lines3d (list endpoint left-endpoint))
      (lines3d (list endpoint right-endpoint))
      (lines3d (list orig endpoint)))))

  (define (vector-plot v [orig (list 0 0 0)])
    (plot3d (vector-renderer v orig)))
)
