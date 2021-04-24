#lang racket

(define head car)
(define tail cdr)

(define (-- n) (- n 1))
(define (++ n) (+ n 1))

;  ПРИМЕРНО КОНТРОЛНО А 2016/2017г
;
; 1
;
(define (meetTwice? f g a b)
  (define (helper f g a b flag)
    (cond [(> a b) #f]
          [(>= flag 2) #t]
          [(= (f a) (g a)) (helper f g (++ a) b (++ flag))]
          [else (helper f g (++ a) b flag)]))
  (helper f g a b 0))

;
; 2
;
(define (maxDuplicate ll)
  (define (max lhs rhs)
    (if (> lhs rhs)
        lhs
        rhs))
  
  (define (maxlist lst)
    (if (empty? (tail lst))
        (head lst)
        (max (head lst) (maxlist (tail  lst)))))

  (define (acurr lst el)
    (cond [(empty? lst) 0]
          [(equal? (head lst) el) (++ (acurr (tail lst) el))]
          [else (acurr (tail lst) el)]))
  
  (define (filt ll)
    (cond [(empty? ll) '()]
          [(empty? (head ll)) (filt (tail ll))]
          [(> (acurr (head ll) (maxlist (head ll))) 1) (cons (maxlist (head ll)) (filt (tail ll)))]
          [else (filt (tail ll))]))
  (if (empty? (filt ll))
      #f
      (maxlist (filt ll))))

;
; 3
;
(define (idk lst term null)
  (if (empty? lst)
      null
      (or (term (head lst)) (idk (tail lst) term null))))

(define (checkMatrix? m k)
  (if (empty? m)
      #t
      (and (idk (head m) (lambda (x) (= (remainder x k) 0)) #f) (checkMatrix? (tail m) k))))

;
; 4
;
(define (longestDescending­ l)
  
  (define (helper l curr res)
    (cond [(and (> (length curr) (length res)) (empty? l)) curr]
          [(empty? l) res]
          [(> (head curr) (head l)) (helper (tail l) (cons (head l) curr) res)]
          [(> (length curr) (length res)) (helper (tail l) (list (head l)) curr)]
          [else (helper (tail l) (list (head l)) res)]))

  (if (empty? l)
      #f
      (reverse (helper (tail l) (list (head l)) '()))))



;  ПРИМЕРНО КОНТРОЛНО А 2015/2016г
;
; 1
;

(define (min-sum-digit a b k)

  (define (sum-digit n)
    (if (= 0 (quotient n 10))
        n
        (+ (remainder n 10) (sum-digit (quotient n 10)))))

  (cond [(> a b) #f]
        [(= (remainder (sum-digit a) k) 0) a]
        [else (min-sum-digit (++ a) b k)]))


;
; 2 май не бачка
;
(define (average f g) (lambda (x) (/ (+ (f x) (g x)) 2)))

(define (accumulate a b null op term)
  (if (> a b)
      null
      (op (term a) (accumulate (++ a) b null op term))))

(define (pow x i)
  (if (= i 0)
      1
      (* x (pow x (-- i)))))

(define (calcprod f n)
  (define (helper f n i)
    (if (> i n)
        1
        (* ((average f (lambda (x) (pow i x))) i) (helper f n (++ i)))))
  (helper f n 1))


;
(define (sublist? lst1 lst2)
  (define (helper lst1 lst2 buff)
    (cond [(empty? buff) #t]
          [(empty? lst2) #f]
          [(equal? (head lst2) (head buff)) (helper lst1 (tail lst2) (tail buff))]
          [else (helper lst1 lst2 lst1)]))
  (helper lst1 lst2 lst1))


(sublist? '(1 2 3 4) '(2 1 2 3 9 1 2 3 4))

(sublist? '(1 2 3 4) '(" hf" 2 1 2 3 9 1 2 3))




          


