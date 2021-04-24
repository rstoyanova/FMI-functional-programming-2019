#lang racket

(define (++ n) (+ n 1))
(define (-- n) (- n 1))

(define (acc null op term a next b)
  (define (helper curr res)
    (if (> curr b)
        res
        (helper (next curr) (op res (term curr)))))
  (helper a null))


(define (accfilter null pred op term a next b)
  (define (helper curr res)
    (cond [(> curr b) res]
          [(pred curr) (helper (next curr) (op res (term curr)))]
          [else (helper (next curr) res)]))
  (helper a null))



(define (!! n) (accfilter 1 (lambda (x) (= (remainder x 2) (remainder n 2))) * ))

(define (divisors-sum n) (accfilter 0 (lambda (x) (= (remainder n x) 0)) + (lambda (x) x) 2 (lambda (x) (+ x 1)) n))


(define (count p? a b)
  (cond [(> a b) 0]
        [(p? a) (++ (count p? (++ a) b))]
        [(count p? (++ a) b)]))


(define gabka (delay (+ a 4)))
(define a 4)








