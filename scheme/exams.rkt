#lang racket

;sameAsCode

(define (++ i) (+ i 1))
(define (-- i) (- i 1))

(define (head lst) (car lst))
(define (tail lst) (cdr lst))

(define (tree? t)
  (or (null? t)
      (and (list? t)
           (= (length t) 3))
           (tree? (cadr t))
           (tree? (caddr t))))
(define empty-tree '())
(define (make-tree root left right) (list root left right))      
(define (make-leaf root) (make-tree root empty-tree empty-tree))
(define root-tree car)
(define left-tree cadr)
(define right-tree caddr)
(define empty-tree? null?)


(define tr
  (make-tree 10
             (make-tree 7
                        (make-leaf 100)
                        (make-leaf 2))
             (make-tree 3
                        (make-tree 6
                                   (make-leaf 1)
                                   (make-leaf 2))
                        empty-tree)))


(define mtr
  (make-tree 1
             (make-leaf 3)
             (make-tree 4
                        (make-leaf 6)
                        empty-tree)))

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


(define (toList n)
  (define (helper n)
    (if (= (quotient n 10) 0)
        (list n)
        (cons (remainder n 10) (helper (quotient n 10)))))
  (reverse (helper n)))
      

(define (helper tree num lst)
  (cond [(empty-tree? tree) #f]
        [(and (empty? lst) (= (root-tree tree) num)) #t]
        [(empty? lst) #f]
        [(= (head lst) 0) (helper (left-tree tree) num (tail lst))]
        [else (helper (right-tree tree) num (tail lst))]))
        

(define (sameAsCode tree n)
  (if (and (= n 1) (= (root-tree tree) 1))
      #t
      (helper tree n (tail (toList (toBinary n))))))

(sameAsCode mtr 7)













                