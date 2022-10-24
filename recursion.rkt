#lang racket

(require racket/trace)

(define (atom? x)
  (and (not (pair? x))
       (not (null? x))))

; lat?: list -> bool
(define lat?
  (lambda (l)
    (cond
      ((null? l) #t)
      ((atom? (car l)) (lat? (cdr l)))
      (else #f))))


; member?: symbol lat -> bool
(define member?
  (lambda (a lat)
    (cond
      ((null? lat) #f)
      ((eq? a (car lat)) #t)
      (else (member? a (cdr lat))))))


 