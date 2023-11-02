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
 
          ( cons (+ (car l1) (car l2))
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
          (cons(- (car l1) (car l2))
               (subup(cdr l1) (cdr l2)))))
 )

(define (mulup l1 l2)  ; From Lecture
  (if (null? l1) '()
      (if (null? l2)'()
          (addup(smup (car l1) l2)
                (cons 0 (mulup (cdr l1) l2)))))
)

(define (smup a l1)  ; From Lecture
  (if (null? l1) '()
      (cons (* a (car l1))
            (smup a (cdr l1))))
)

(define (poly-list-of-list l1 L2)
  (if (null? l1) '()
      (if (null? L2) '()
          (cons (mulup l1 (car L2))
            (poly-list-of-list l1 (cdr L2)))))
)

(define (poly_mul2 L1 L2)
  (if(null? L1) '()
     (if(null? L2) '()
        (poly_add(poly-list-of-list (car L1) L2)
                (cons '() (poly_mul2 (cdr L1) L2)))))
  )

         
(define (poly_mul apol bpol)
  (if(null? apol) '()
     (if(null? bpol) '()
        (cons (mulup (car apol) (car bpol))
                (poly_mul (cdr apol) (cdr bpol))))))


(define (checkempty l1 l2)
  (cond (0 car l1) (cons('())
               (cdr l1))))