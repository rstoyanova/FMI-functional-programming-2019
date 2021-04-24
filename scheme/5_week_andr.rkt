#lang racket

(define head car)
(define tail cdr)

(define (-- n) (- n 1))
(define (++ n) (+ n 1))

(define (nth n lst)
  (if (= n 0)
      (head lst)
      (nth (-- n) (tail lst))))

(define (range from to)
  (if (> from to)
      '()
      (cons from (range (++ from) to))))

(define (digit-list n)
  (define (helper n)
    (if (= 0 n)
        '()
        (cons (remainder n 10) (helper (quotient n 10)))))
  (if (= n 0)
      '(0)
      (reverse (helper n))))

(define (take n lst)
  (if (= n 0)
      '()
      (cons (head lst) (take (-- n) (tail lst)))))


(define (drop n lst)
  (cond [(> n (length lst)) "Error"]
        [(= n 0) lst]
        [else (drop (-- n) (tail lst))]))


(define (all? p? lst)
  (if (empty? lst)
      #t
      (and (p? (head lst)) (all? p? (tail lst)))))

      
(define (zip lst1 lst2)
  (if (or (empty? lst1) (empty? lst2))
      '()
      (cons (cons (head lst1) (head lst2)) (zip (tail lst1) (tail lst2)))))


(define (zipWith f lst1 lst2)
  (if (or (empty? lst1) (empty? lst2))
      '()
      (cons (f (head lst1) (head lst2)) (zipWith f (tail lst1) (tail lst2)))))

(define (contains lst el)
  (if (empty? lst)
      #f
      (or (equal? el (head lst)) (contains (tail lst) el))))

(define (uniques lst)
  (define (helper lst res)
    (cond [(empty? lst) res]
          [(contains res (head lst)) (helper (tail lst) res)]
          [else (helper (tail lst) (helper (tail lst) (cons (head lst) res)))]))
  (helper lst '()))
















          
  




