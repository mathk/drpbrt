#lang s-exp syntax/module-reader racket

#:read point-read
#:read-syntax point-read-syntax

;(provide point-read
;         point-read-syntax)

(require
  racket/block
  syntax/readerr)

;(provide (rename-out [point-read read]
;                     [point-read-syntax read-syntax]))

(define (skip-whitespace port)
  ; Skips whitespace characters, sensitive to the current
  ; readtable's definition of whitespace
  (let ([ch (peek-char port)])
    (unless (eof-object? ch)
      (when (and (char? ch)
                 (char-whitespace? ch))
        (read-char port)
        (skip-whitespace port)))))

(define (raise-parse-error msg src char port)
  (let-values ([(l c p) (port-next-location port)])
    ((if (eof-object? char)
       raise-read-eof-error
       raise-read-error)
     msg src l c p 1)))

(define (parse-one-coordinate port read-one src)
  (let ([coord (read-one)]
        [next (peek-char port)])
    (if (eq? #\, next)
      (block
        (read-char port)
        (skip-whitespace port)
        coord)
      (raise-parse-error "expected `,'" src next port))))

(define (parse-struct struct-kind closing-char port read-one src)
  (read-char port)
  (skip-whitespace port)
  (let ([x-elem (parse-one-coordinate port read-one src)]
        [y-elem (parse-one-coordinate port read-one src)]
        [z-elem (read-one)])
    (skip-whitespace port)
    (let ([close/1 (read-char port)])
      (if (not (eq? closing-char close/1))
        (raise-parse-error (format "expected closing `~a>'" closing-char) src close/1 port)
        (let ([close/2 (read-char port)])
          (if (not (eq? #\> close/2))
            (raise-parse-error (format "expected closing `~a>'" closing-char) src close/2 port)
            (list struct-kind x-elem y-elem z-elem)))))))

(define (parse port read-one src)
  (case (peek-char port)
    [(#\:) (parse-struct 'point #\: port read-one src)]
    [(#\~) (parse-struct 'vector #\~ port read-one src)]

    [else 
      (read/recursive port #\< #f #f)]))

(define delimiter-table
  (letrec ([misplaced-delimiter
             (case-lambda
               [(ch port) (misplaced-delimiter ch port #f #f #f #f)]
               [(ch port src line col pos)
                (raise-read-error (format "misplaced `~a' in point" ch) src line col pos 1)])])
    (make-readtable (current-readtable)
                    #\: 'terminating-macro misplaced-delimiter
                    #\~ 'terminating-macro misplaced-delimiter
                    #\, 'terminating-macro misplaced-delimiter)))

(define parse-point
  (case-lambda
    [(ch port) (parse port
                      (lambda () (read/recursive port #f delimiter-table))
                      (object-name port))]
    [(ch port src line col pos)
     (datum->syntax
       #f
       (parse port
              (lambda () (read-syntax/recursive src port #f delimiter-table))
              src)
       (let-values ([(l c p) (port-next-location port)])
         (list src line col pos (and pos (- p pos)))))]))

(define point-readtable (make-readtable (current-readtable)
                                        #\< 'non-terminating-macro parse-point))

(define (point-read in)
  (parameterize ([current-readtable point-readtable])
    (read in)))
;  (syntax->datum
;    (point-read-syntax #f in)))

(define (point-read-syntax src in)
  (parameterize ([current-readtable point-readtable])
    (read-syntax src in)))

