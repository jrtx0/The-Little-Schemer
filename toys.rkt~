#lang racket

(require racket/trace)

(define atom?
  (lambda (m)
    (and (not (pair? m)) (not (null? m)))))

;; tail recursive
(define (plus1 x y)
  (if (zero? y) x
      (plus1 (add1 x) (sub1 y))))

;; naturally recursive
(define (plus2 x y)
  (if (zero? y) x
      (add1 (plus2 x (sub1 y)))))
