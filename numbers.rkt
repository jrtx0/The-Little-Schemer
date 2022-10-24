#lang racket

(require racket/trace)

; zero?: number -> bool
(define (zero? x)
  (if (= x 0) #t #f))

; +: number number -> number
; nature recursion
(define +
  (lambda (a b)
    (cond
     ((zero? b) a)
     (else (add1 (+ a (sub1 b)))))))

; tail recursion
;(define +
;  (lambda (a b)
;    (cond
;     ((zero? b) a)
;     (else (+ (add1 a) (sub1 b))))))

; -: number number -> number
; nature recursion
(define -
  (lambda (a b)
    (cond
     ((zero? b) a)
     (else (sub1 (- a (sub1 b)))))))

; tail recursion
;(define -
;  (lambda (a b)
;    (cond
;     ((zero? b) a)
;     (else (- (sub1 a) (sub1 b))))))

; addtup: tup -> number
(define addtup
  (lambda (tup)
    (cond
      ((null? tup) 0)
      (else (+ (car tup) (addtup (cdr tup)))))))

; *: number number -> number
(define *
  (lambda (n m)
    (cond
      ((zero? m) 0)
      (else (+ n (* n (sub1 m)))))))

; tup1+: tup tup -> tup
(define tup1+
  (lambda (tup1 tup2)
    (cond
     ((null? tup2) tup1)
     ((null? tup1) tup2)
     (else (cons (+ (car tup1) (car tup2))
                 (tup1+ (cdr tup1) (cdr tup2)))))))

; tup2+: tup tup -> tup
(define tup2+
  (lambda (tup1 tup2)
    (cond
     ((and (null? tup1) (null? tup2)) '())
     (else (cons (+ (car tup1) (car tup2))
                 (tup2+ (cdr tup1) (cdr tup2)))))))

; <: number number -> bool
(define <
  (lambda (n m)
    (cond
      ((zero? m) #f)
      ((zero? n) #t)
      (else (< (sub1 n) (sub1 m))))))

; >: number number -> bool
(define >
  (lambda (n m)
    (cond
      ((zero? m) #f)
      ((zero? n) #t)
      (else (> (sub1 n) (sub1 m))))))

; =: number number -> bool
(define =
  (lambda (n m)
    (cond
      ((> n m) #f)
      ((< n m) #f)
      (else #t))))

; ^: number number -> number
(define ^
  (lambda (n m)
    (cond
      ((zero? m) 1)
      (else (* n (^ n (sub1 m)))))))

; /: number number -> number
(define /
  (lambda (n m)
    (cond
     ((< n m) 0)
     (else (add1 (/ (- n m) m))))))

; length: lat -> number
(define length
  (lambda (lat)
    (cond
      ((null? lat) 0)
      (else (add1 (length (cdr lat)))))))

; pick: number lat -> symbol
(define pick
  (lambda (n lat)
    (cond
     ((zero? (sub1 n)) (car lat))
     (else (pick (sub1 n) (cdr lat))))))

; rempick: number lat -> lat
(define rempick
  (lambda (n lat)
    (cond
     ((null? lat) '())
     ((zero? (sub1 n)) (cdr lat))
     (else (cons (car lat)
                 (rempick (sub1 n) (cdr lat)))))))

; no-nums: lat -> lat
(define no-nums
  (lambda (lat)
    (cond
     ((null? lat) '())
     ((number? (car lat)) (no-nums (cdr lat)))
     (else (cons (car lat)
                 (no-nums (cdr lat)))))))

; all-nums: lat -> lat
(define all-nums
  (lambda (lat)
    (cond
     ((null? lat) '())
     ((not (number? (car lat))) (all-nums (cdr lat)))
     (else (cons (car lat) (all-nums (cdr lat)))))))

; eqan? symbol symbol -> bool
(define eqan?
  (lambda (a1 a2)
    (cond
     ((and (number? a1) (number? a2))
      (= a1 a2))
     ((or (number? a1) (number? a2))
      #f)
     (else (eq? a1 a2)))))

; occur: symbol lat -> number
(define occur
  (lambda (a lat)
    (cond
     ((null? lat) 0)
     ((eqan? a (car lat))
      (add1 (occur a (cdr lat))))
     (else (occur a (cdr lat))))))

; one?: number -> bool
(define one?
  (lambda (n)
    (cond
     ((zero? n) #f)
     (else (zero? (sub1 n))))))

; rempick: number lat -> lat
(define rempick*
  (lambda (n lat)
    (cond
     ((null? lat) '())
     ((one? n) (cdr lat))
     (else (cons (car lat)
                 (rempick* (sub1 n)
                          (cdr lat)))))))
