#lang racket/base

(provide
 ;; Vector creation
 vector
 ;; Adding 2 vectors
 vector-add
 ;; Substracting 2 vectors
 vector-sub
 ;; Computing the dot product of 2 vectors
 vector-dot)

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
       (write-string ">" port)))])

(define (vector-map-fold v1 v2 mapfunc foldfunc)
  (foldfunc
   (mapfunc (vector-x v1) (vector-x v2))
   (mapfunc (vector-y v1) (vector-y v2))
   (mapfunc (vector-z v1) (vector-z v2))))

(define (vector-add v1 v2) (vector-map-fold v1 v2 + vector))

(define (vector-sub v1 v2) (vector-map-fold v1 v2 - vector))

(define (vector-dot v1 v2) (vector-map-fold v1 v2 * +))

(define (vector-magnitude v) (sqrt (vector-dot v v))) 

(module* private-test #f
  (provide vector-x vector-y vector-z))