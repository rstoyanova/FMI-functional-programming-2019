#lang racket

;-------- DEEP LISTS

(define (atom? el)
  (and (not (null? el)) (not (pair? el))))

(define (flatten* lst)
  (cond [(null? lst) '()]
        [(atom? lst) (list lst)]
        [else (append (flatten* (car lst)) (flatten* (cdr lst)))]
        )
  )

(define (flatten** l) (deep-fold '() list append l))

;(flatten* '((1 (2)) (((3) 4) (5 (6)) () (7)) 8))

(define (deep-reverse lst)
  (cond [(null? lst) '()]
        [(atom? lst) lst]
        [else (append (deep-reverse (cdr lst)) (list (deep-reverse (car lst))))])
  )

;(deep-reverse '((1 (2)) (((3) 4) (5 (6)) () (7)) 8))

(define (deep-fold nv term op lst)
  (cond [(null? lst) nv]
        [(atom? lst) (term lst)]
        [else (op (deep-fold nv term op (car lst)) (deep-fold nv term op (cdr lst)))])
  )

(define (count-atoms l) (deep-fold 0 (lambda (x) 1) + l))

;(count-atoms '((1 (2)) (((3) 4) (5 (6)) () (7)) 8))

;(lambda l  (+ 1 l))
;(lambda (x y . l) ())

;(define (g x y . l) (append x l)) - l e spisuk ot parametrite 
(define (g x y . l) (append (append x l) (append y l)))
;(g '(1 2 3) '(4 5 6) '(7) '(8) '(9))

;(define (func x . l) (cons x l))

;(map + '(1 2 3) '(4 5 6))
;(map list '(1 2 3) '(4 5 6))

;(apply + '(1 2 3 4 5))
;(apply append '((1 2) (3 4) (5 6))) apply maha nai - gorniq spisuk za da stigne do elementa tuk vrushta '(1 2 3 4 5 6)
;(apply append (list '((1 2) (3 4) (5 6)))) -> '((1 2) (3 4) (5 6))
;(apply list '(1 2 3 4))



;-------- TREES,GRAPHS AND matrix
(define (all p? l) (foldr (lambda (x y) (and x y)) #t (map p? l))) ; proverka za korektnost

;****MATRIX****

(define (matrix? m)
  (and (list? m)
       (not (null? (car m)))
       (all list? m)
       (all (lambda (row) (= (length row) (length (car m)))) m))
  )

(define (sum-vectors v1 v2) (map + v1 v2))
(define (sum-matrices m1 m2) (map sum-vectors m1 m2)) ; subirane na matrici

(define (get-row i m) (list-ref m i)) ; namirane na redove i stulbove po daden index
(define (get-column i m)
(map (lambda (row) (list-ref row i)) m))


;-------- racionalni chisla

(define (make-rat n d)
  (if (= d 0) (cons n 1) (cons n d))
  )

(define (rat* p q)
  (make-rat (* (car p) (car q))
            (* (cdr p) (cdr q)))
  )

;------- TREES

(define (tree? t)
  (and (list? t)
       (or (null? t)
           (and 
            (= (length t) 3)
            (tree? (cadr t))
            (tree? (caddr t))))))

;-------Constructors
(define empty-tree '())
(define (make-tree root left right) (list root left right))
(define (make-leaf root) (make-tree root '() '()))

;-------Selectors
(define root-tree car)
(define left-tree cadr)
(define right-tree caddr)
(define empty-tree? null?)

;---- depth tree

(define (depth-tree t) ; namira kolko visoko e t.e kolko niva ima
  (if (empty-tree? t) 0
      (+ 1 (max (depth-tree (left-tree t))
                (depth-tree (right-tree t))))))

;(depth-tree '(1 (2 () ())(3 (4 () ())(5 () ()))))

;----Find subtree

(define (memq-tree x t)
(cond [(empty-tree? t) #f]
[(eq? x (root-tree t)) t]
[else (or
       (memq-tree x (left-tree t))
       (memq-tree x (right-tree t)))]))


;---- Path in tree
;Да се намери в дървото път от корена до даден възел x

(define (path-tree x t)
  (cond [(empty-tree? t) #f]
        [(equal? (root-tree t) x) (list x)]
        [else (cons#f (root-tree t)
                        (or (path-tree x (left-tree t))
                            (path-tree x (right-tree t))))]))

(define (cons#f h t) (and t (cons h t)))

;---------- Associative lists

(define (make-alist f keys)
  (map (lambda (x) (cons x (f x))) keys)
  )

;(make-alist (lambda (x) (* x x)) '(1 2 3))

(define (keys alist) (map car alist))
(define (values alist) (map cdr alist))

;(assoc 3 '((1 . 1) (2 . 4) (3 . 9)))

(define (del-assoc key alist)
  (filter (lambda (x) (not (equal? (car x) key))) alist)
  )

(define (add-assoc key value alist)
  (cons (cons key value) (del-assoc key alist))
  )

;зад дали има елемент от l който удовлетворява предиката p

(define (search p l)
  (and (not (null? l))
       (or (p (car l)) (search p (cdr l))))
  )

;зад дали всички елементи от l удовлетворяват p
(define (all? p l)
  (not (search (lambda (x) (not (p x))) l))
  )

;--------GRAPHS
;((1 2 3) (2 3 6) (3 4 6) (4 1 5) (5 3) (6 5)) key values

(define vertices keys)

(define (children v g)
  (cdr (assoc v g)))


(define (edge? u v g) ; vrushta rebroto
  (memv v (children u g)))

(define (edge1? u v g)
  (if (equal? (memv v (children u g)) #f) #f #t)) ; dali ima rebro 

(define (map-children v f g)
(map f (children v g)))

(define (search-child v f g)
  (search f (children v g)))

;зад да се намерят върховете, които нямат деца

(define (nochildren g)
  (cond [(null? g) '()]
        [(and (null? (cadr (car g))) (null? (caddr (car g)))) (cons (caar g) (nochildren (cdr g)))]
        [else (nochildren (cdr g))]
  )
  )

(define (childless g)
(filter (lambda (v) (null? (children v g))) (vertices g))) ; '( () () ) ne e null takache ne baca mnogo

(define (parents v g)
(filter (lambda (u) (edge? u v g)) (vertices g)))

;symetric graph
(define (symmetric? g)
(all? (lambda (u)
(all? (lambda (v) (edge? v u g))
(children u g)))
(vertices g)))


;DFS

(define (dfs-path u v g)
(define (dfs-search path)
  (let ([current (car path)])
    (cond [(eqv? current v) (reverse path)]
          [(memv current (cdr path)) #f]
          [else (search-child current
                              (lambda (w) (dfs-search (cons w path))) g)])))
(dfs-search (list u)))

