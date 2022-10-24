#lang racket

(define (atom? n)
  (and (not (pair? n)) (not (null? n))))

;(define (rember* a l)
;  (cond
;    ((null? l) (quote ()))
;    ((atom? (car l))
;       (if (eq? a (car l)) (rember* a (cdr l))
;           (cons (car l) (rember* a (cdr l)))))
;    (else (cons (rember* a (car l)) (rember* a (cdr l))))))

(define rember*
  (lambda (a l)
    (cond
      ((null? l) (quote ()))
      ((atom? (car l))
       (cond
         ((eq? (car l) a)
            (rember* a (cdr l)))
         (else (cons (car l)
                 (rember* a (cdr l))))))
      (else (cons (rember* a (car l))
                  (rember* a (cdr l)))))))