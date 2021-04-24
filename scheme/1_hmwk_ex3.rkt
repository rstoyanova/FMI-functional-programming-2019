#lang racket

(define (make-date d m y)
  (list d m y))

(define (day date)
  (car date))

(define (month date)
  (cadr date))

(define (year date)
  (caddr date))

(define (leap? date)
  (if (or (and (= (remainder (year date) 4) 0)
          (not (= (remainder (year date) 100) 0)))
       (= (remainder (year date) 400) 0))
      #t
      #f))

(define (from-to start end num)
  (if (and (<= start num) (>= end num))
      #t
      #f))

(define (date? date)
  (cond [(and (not (= (month date) 2)) (= (remainder (month date) 2) 0) (or (and (from-to 1 7 (month date)) (from-to 0 30 (day date))) (and (from-to 8 12 (month date)) (from-to 0 31 (day date))))) #t]
        [(and (= (month date) 2) (or (and (leap? date) (from-to 0 29 (day date))) (and (not (leap? date)) (from-to 0 28 (day date))))) #t]
        [(and (= (remainder (month date) 2) 1) (or (and (from-to 1 7 (month date)) (from-to 0 31 (day date))) (and (from-to 8 12 (month date)) (from-to 0 30 (day date))))) #t]
        [else #f]))
  
(define (date->string date)
  (string-replace (string-replace (string-replace (~a date) " " ".") "(" "") ")" ""))

(define (next-day date)
  (cond [(date? (make-date (+ (day date) 1) (month date) (year date))) (make-date (+ (day date) 1) (month date) (year date))]
        [(and (= (month date) 2) (or (and (leap? date) (= (day date) 29)) (and (not (leap? date)) (= (day date) 28)))) (make-date 1 3 (year date))]
        [(and (= (day date) 31) (= (month date) 12)) (make-date 1 1 (+ (year date) 1))]
        [else (or (= (day date) 30) (= (day date) 31)) (make-date 1 (+ (month date) 1) (year date))]))

(define (date< lhs rhs)
  (cond [(< (year lhs) (year rhs)) #t]
        [(and (= (year lhs) (year rhs)) (< (month lhs) (month rhs))) #t]
        [(and (= (year lhs) (year rhs)) (= (month lhs) (month rhs)) (< (day lhs) (day rhs))) #t]
        [else #f]))

(define (++ i) (+ i 1))

(define (days-from-beg date)
  (define (helper curr date res)
    (if (equal? date curr)
        res
        (helper (next-day curr) date (++ res))))
  (helper (make-date 1 1 1582) date 0))

(define (weekday date)
  (cond [(= (remainder (days-from-beg date) 7) 3) 'Monday]
        [(= (remainder (days-from-beg date) 7) 4) 'Tuesday]
        [(= (remainder (days-from-beg date) 7) 5) 'Wednesday]
        [(= (remainder (days-from-beg date) 7) 6) 'Thursday]
        [(= (remainder (days-from-beg date) 7) 0) 'Friday]
        [(= (remainder (days-from-beg date) 7) 1) 'Saturday]
        [(= (remainder (days-from-beg date) 7) 2) 'Sunday]))

(define (next-weekday day date)
  (if (equal? day (weekday date))
      date
      (next-weekday day (next-day date))))

(define (events-for-day date list)
   (cond [(null? list) '()]
         [(equal? (caar list) date) (cons (car list) (events-for-day date (cdr list)))]
         [else (events-for-day date (cdr list))]))
        
(define (insert date list)
  (cond [(null? list) date]
        [(date< (caar list) (car date)) (cons (car list) (insert date (cdr list)))]
        [else (cons date list)]))


(define (calendar lst)
  (define (helper-sort lst res)
    (if (null? lst)
        res
        (helper-sort (cdr lst) (insert (car lst) res))))
  (helper-sort lst '()))






