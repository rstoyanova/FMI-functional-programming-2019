#lang racket

(define (++ n) (+ n 1))
(define (-- n) (- n 1))

;
; бързо вдигане на степен
;

(define (fast-exp x n)
  (define (half) (quotient n 2))
  (cond [(= n 0) 1]
        [(= n 1) x]
        [(even? n) (fast-exp (* x x) (half))]
        [else (* x (fast-exp (* x x) (half)))]))

;
; # корени
;
(define (roots a b c)
  (define discr (- (* b b) (* 4 (* a c))))
  (cond [(= discr 0) 1]
        [(> discr 0) 2]
        [else 0]))

;
; факториел с опашкова рекурсия
;
(define (fact n)
  (define (loop i res)
    (if (> i n)
        res
        (loop (+ i 1) (* res i))))
  (loop 1 1))
;
(define (fact2 n)
  (cond [(= n 0) 1]
        [(= n 1) 1]
        [else (* n (fact2 (-- n)))]))

;
; обръща число
;
(define (reverse-int n)
  (define (helper n res)
    (if (= n 0)
        res
        (helper (quotient n 10) (+ (* res 10) (remainder n 10)))))
  (helper n 0))

;
(define (palindrome? n) (= n (reverse-int n)))

;
(define (divisor-sum n)
  (define (helper n i res)
    (cond [(or (= n 0) (= n 1)) 0]
          [(= i n) res]
          [(= (remainder n i) 0) (helper n (++ i) (+ res i))]
          [else (helper n (++ i) res)]))
  (helper n 1 0))
          
;
; перфектно число => сумата на делителите = числото
;
(define (perfect? n) (= n (divisor-sum n)))

;
(define (prime? n)
  (define (helper n i)
    (cond [(> i (/ n 2)) #t]
          [(= n 1) #t]
          [(= (remainder n i) 0) #f]
          [else (helper n (++ i))]))
  (helper n 2))


(define (increasing? n)
  (define (last n) (remainder n 10))
  (define (figure? n) (= (quotient n 10) 0))

  (cond [(figure? n) #t]
        [(> (last n) (remainder (quotient n 10) 10)) (increasing? (quotient n 10))]
        [else #f]))
  

(define (toBinary n)
  (define (cntr num)
    (if (= (quotient num 10) 0)
        1
        (++ (cntr (quotient num 10)))))
  (define (fromZ zeros)
      (if (= zeros 0)
          1
          (* 10 (fromZ (-- zeros)))))          
    
  (define (helper n zeros res)
    (cond [(= n 0) (quotient res 10)]
          [(= (remainder n 2) 0)  (helper (quotient n 2) (++ zeros) res)]
          [else (helper (quotient n 2) 0 (+ res (fromZ (+ zeros (cntr res)))))]))
  (helper n 0 0))










