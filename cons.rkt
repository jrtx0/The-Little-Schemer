#lang racket

(require racket/trace)

; rember: symbol lat -> lat
(define rember
  (lambda (a lat)
    (cond
      ((null? lat) '())
      ((eq? a (car lat)) (cdr lat))
      (else (cons (car lat) (rember a (cdr lat)))))))

; remove all ocurrences a in lat
; multirember: symbol lat -> lat
(define multirember
  (lambda (a lat)
    (cond
      ((null? lat) (quote ()))
      (else (cond
              ((eq? (car lat) a) (multirember a (cdr lat)))
              (else (cons (car lat) (multirember a
                            (cdr lat)))))))))

; firsts: list -> list
(define firsts
  (lambda (l)
    (cond
      ((null? l) '())
      (else (cons (car (car l)) (firsts (cdr l)))))))

; insertR: symbol symbol lat -> lat
(define insertR
  (lambda (new old lat)
    (cond
      ((null? lat) '())
      ((eq? old (car lat)) (cons old (cons new (cdr lat))))
      (else (cons (car lat) (insertR new old (cdr lat)))))))

; insertL: symbol symbol lat -> lat
(define insertL
  (lambda (new old lat)
    (cond
      ((null? lat) '())
      ((eq? old (car lat)) (cons new lat))
      (else (cons (car lat) (insertL new old (cdr lat)))))))

; subst: symbol symbol lat -> lat
(define subst
  (lambda (new old lat)
    (cond
      ((null? lat) '())
      ((eq? old (car lat)) (cons new (cdr lat)))
      (else (cons (car lat) (subst new old (cdr lat)))))))

; subst2: symbol symbol symbol lat -> lat
(define subst2
  (lambda (new old1 old2 lat)
    (cond
      ((null? lat) '())
      ((eq? old1 (car lat)) (cons new (cdr lat)))
      ((eq? old2 (car lat)) (cons new (cdr lat)))
      (else (cons (car lat) (subst2 new old1 old2 (cdr lat)))))))

; multirember: symbol lat -> lat
(define multirember
  (lambda (a lat)
    (cond
      ((null? lat) '())
      ((eq? a (car lat)) (multirember a (cdr lat)))
      (else (cons (car lat) (multirember a (cdr lat)))))))

; multiinsertR: symbol symbol lat -> lat
(define multiinsertR
  (lambda (new old lat)
    (cond
     ((null? lat) '())
     ((eq? old (car lat))
      (cons old (cons new (multiinsertR new old (cdr lat)))))
     (else (cons (car lat) (multiinsertR new old (cdr lat)))))))

; multiinsertL: symbol symbol lat -> lat
(define multiinsertL
  (lambda (new old lat)
    (cond
     ((null? lat) '())
     ((eq? old (car lat))
      (cons new (cons old (multiinsertL new old (cdr lat)))))
     (else (cons (car lat) (multiinsertL new old (cdr lat)))))))

; multisubst: symbol symbol lat -> lat
(define multisubst
  (lambda (new old lat)
    (cond
     ((null? lat) '())
     ((eq? old (car lat))
      (cons new (multisubst new old (cdr lat))))
     (else (cons (car lat) (multisubst new old (cdr lat)))))))

