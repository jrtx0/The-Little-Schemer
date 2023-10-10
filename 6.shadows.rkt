#lang racket

(require racket/trace)
(require "toys.rkt")

(define atom?
  (lambda (m)
    (and (not (pair? m)) (not (null? m)))))

(define numbered?
  (lambda (aexp)
    (cond
      ((atom? aexp) (number? aexp))
      ((eq? (car (cdr aexp)) (quote +))
       (and (numbered? (car aexp)) (numbered? (car (cdr (cdr aexp))))))
      ((eq? (car (cdr aexp)) (quote *))
       (and (numbered? (car aexp)) (numbered? (car (cdr (cdr aexp))))))
      ((eq? (car (cdr aexp)) (quote ^))
       (and (numbered? (car aexp)) (numbered? (car (cdr (cdr aexp)))))))))

;(define value
;       (lambda (nexp)
;         (cond
;           ((atom? nexp) nexp)
;           ((eq? (car (cdr nexp)) (quote +))
;            (+ (value (car nexp))
;               (value (car (cdr (cdr nexp))))))
;           ((eq? (car (cdr nexp)) (quote *))
;            (* (value (car nexp))
;               (value (car (cdr (cdr nexp))))))
;           (else
;            (expt (value (car nexp))
;               (value
;                (car (cdr (cdr nexp)))))))))

;(define value
;  (lambda (nexp)
;    (cond
;      ((atom? nexp) nexp)
;      ((null? nexp) 0)
;      ((eq? (car nexp) (quote +))
;         (+ (value (car (cdr nexp))) (value (car (cdr (cdr nexp))))))
;      ((eq? (car nexp) (quote *))
;         (* (value (car (cdr nexp))) (value (car (cdr (cdr nexp))))))
;      ((eq? (car nexp) (quote ^))
;         (expt (value (car (cdr nexp))) (value (car (cdr (cdr nexp)))))))))

(define 1st-sub-exp
  (lambda (aexp)
    (car (cdr aexp))))

(define 2nd-sub-exp
  (lambda (aexp)
    (car (cdr (cdr aexp)))))

(define operator
  (lambda (aexp)
    (car aexp)))

(define value
  (lambda (nexp)
    (cond
      ((atom? nexp) nexp)
      ((eq? (operator nexp) (quote +))
       (+ (value (1st-sub-exp nexp))
          (value (2nd-sub-exp nexp))))
      ((eq? (operator nexp) (quote *))
       (* (value (1st-sub-exp nexp))
          (value (2nd-sub-exp nexp))))
      (else
       (exp (value (1st-sub-exp nexp))
            (value (2nd-sub-exp nexp)))))))

(define 1st-sub-exp*
  (lambda (aexp)
    (car aexp)))

(define operator*
  (lambda (aexp)
    (car (cdr aexp))))

(define sero?
  (lambda (n)
    (null? n)))

(define edd1
  (lambda (n)
    (cons (quote ()) n)))

(define zub1
  (lambda (n)
    (cdr n)))

(define add
  (lambda (n m)
    (cond
      ((sero? m) n)
      (else (edd1 (add n (zub1 m)))))))

