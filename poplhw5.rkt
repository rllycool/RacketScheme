#lang racket

(define (poly_add apol bpol)
  (if (null? apol) bpol
      (if (null? bpol) apol
          (cons (addup (car apol) (car bpol))
                (poly_add (cdr apol) (cdr bpol)))))
  )

(define (addup l1 l2) ; From Lecture
  (if (null? l1) l2
      (if (null? l2) l1
          (cons (+ (car l1) (car l2))
                (addup (cdr l1) (cdr l2)))))
  )

(define (poly_sub apol bpol)
   (if (null? apol) bpol
      (if (null? bpol) apol
          (cons(subup (car apol) (car bpol))
              (poly_sub (cdr apol) (cdr bpol)))))
)

(define (subup l1 l2)
  (if (null? l1) l2
      (if (null? l2) l1
          (cons(- (car l2) (car l1)) subup(cdr l1) cdr l2))))