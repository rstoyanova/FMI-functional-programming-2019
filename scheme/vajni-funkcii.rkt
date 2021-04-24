#lang racket

(define (reverse-int n)
  (define (helper n res)
    (if (= n 0) res
        (helper (quotient n 10) (+ (* res 10) (modulo n 10)))
    ))
  (helper n 0)
  )

(define (accumulate* op nv from to term iter)
  (if(> from to)
     nv
    (op (term from) (accumulate* op nv (iter from) to term iter))
  ))

(define (foldr* op base lst)
  (if (null? lst)
      base
  (op (car lst) (foldr* op base (cdr lst)))
  ))

(define (foldl* op nv lst)
  (if(null? lst)
     nv
     (foldl* op (op nv (car lst)) (cdr lst)))
  )

(define (length lst)
  (if (null? lst) 0 (+ 1 (length (cdr lst))))
  )

(define (list-tail* lst n)
  (cond [(null? lst) lst]
        [(= n 0) lst]
        [else (list-tail* (cdr lst) (- n 1))])
  )

(define (list-ref* lst n)
  (car (list-tail* lst n))
  )

(define (member* el lst)
  (cond [(null? lst) #f]
        [(equal? el (car lst)) lst]
        [else (member* el (cdr lst))]
  ))

(define (append* l1 l2)
  (foldr cons l2 l1))

(define (append1 l1 l2)
  (if (null? l1) l2
      (cons (car l1) (append1 (cdr l1) l2))))

(define (rcons a b) ; slaga element otzad na spisuk
  (append b (list a))
  )

(define (snoc a b)
  (cons b a))

(define (reverse l)
  (define (iter r l)
    (if (null? l) r
        (iter (cons (car l) r) (cdr l))))
  (iter '() l))

(define (from-to a b)
  (if (> a b)
      '()
      (cons a (from-to (+ a 1) b)))
  )

(define (map* func lst)
  (if (null? lst)
      '()
      (cons (func (car lst)) (map func (cdr lst))))
  )

(define (filter* p? l)
(cond [(null? l) '()]
[(p? (car l)) (cons (car l) (filter* p? (cdr l)))]
[else (filter* p? (cdr l))]))

(define (flatten* lst)
  (if (list? lst)
      (foldr append '() (map flatten* lst))
      (list lst)
      )
  )

(define (pairs l)
  (map (lambda(x) (map (lambda(y) (cons x y)) l)) l)) ; pravi vsichki kombinacii na elementite na daden spisuk samo che razdeleni na spisuci i trqbva da se flattenvat

(define (pairs1 l) ; sushtoto samoche gi vrushta v edin spisuk s pairs
  (define (helper lst)
  (if (null? lst)
      '()
     (append (map (lambda(y) (cons (car lst) y)) l) (helper (cdr lst)))
     )
  )
  (helper l)
  )

;;subsets
(define (subsets s)
  (if (null? s)
      (list null)
      (let ([rest (subsets (cdr s))])
        (append rest (map (lambda (x)
                            (cons (car s) x))
                          rest)))))


;;Permutatios;;
(define (remove lst el)
  (cond [(null? lst) '()]
        [(equal? el (car lst)) (cdr lst)]
        [else (cons (car lst) (remove (cdr lst) el))])
  )

(define (permutations items)
  (if (null? items) '(())
      (apply append
             (map (lambda (element)
                    (map (lambda (permutation)
                           (cons element permutation))
                         (permutations (remove items element))))
                  items))))
;(and (< 3 5) (list 2 5)) tova vrushta list 2 5 , zashtoto ocenqva purvoto uslovie na true i vrushta ocenkata na vtoroto a to e spisuka


(require racket/trace)
(trace foldr*)