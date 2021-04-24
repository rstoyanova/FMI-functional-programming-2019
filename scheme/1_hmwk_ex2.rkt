#lang racket

(define (prefixes xs)
  (define (helper n)
    (define current (get xs n))
    (print current)
    (if (= n (length xs)) (void)
       (helper (+ n 1))))
  (helper 0))
 
 
(define (get l n)
  (define (helper cnt new-l div-l )
    (if(or (= cnt n) (= cnt (length l)))
       (reverse new-l)
       (helper (+ 1 cnt) (cons (car div-l) new-l) (cdr div-l))))
  (helper 0 '() l))