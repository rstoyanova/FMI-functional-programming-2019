#lang racket

(define (++ i) (+ i 1))
(define (-- i) (- i 1))

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
                        (make-tree 4
                                   (make-leaf 1)
                                   (make-leaf 2))
                        empty-tree)))

(define mtr
  (make-tree 1
             (make-leaf 12)
             (make-tree -3
                        (make-leaf 7)
                        empty-tree)))


(define (tree-sum t)
  (if (empty-tree? t)
      0
      (+ (root-tree t) (tree-sum (left-tree t)) (tree-sum (right-tree t)))))

(define (tree-max t)
  (if (empty-tree? t)
      -inf.0
      (max (root-tree t) (tree-max (left-tree t)) (tree-max (right-tree t)))))

(define (tree-level k t)
  (cond [(empty-tree? t) '()]
        [(= k 0) (list (root-tree t))]
        [else (append (tree-level (- k 1) (left-tree t)) (tree-level (- k 1) (right-tree t)))]))

(define (max-depth t)
  (if (empty-tree? t)
      0
      (max (++ (max-depth (left-tree t))) (++ (max-depth (right-tree t))))))

(define (all-levels t)
  (define m (max-depth t))
  (define (helper t i)
      (if (= i m)
          (tree-level i t)
          (cons (tree-level i t) (helper t (++ i)))))
  (helper t 0))
          
(define (tree->list t)
  (if (empty-tree? t)
      '()
      (append (tree->list (left-tree t)) (list (root-tree t)) (tree->list (right-tree t)))))


(define (bst-insert val t)
  (cond [(empty-tree? t) (make-leaf val)]
        [(< val (root-tree t))
         (make-tree (root-tree t)
                    (bst-insert val (left-tree t))
                    (right-tree t))]
        [else (make-tree (root-tree t)
                         (left-tree t)
                         (bst-insert val (right-tree t)))]))

(define t1 (make-leaf 12))
(define t2(bst-insert 10 t1))
(define t3 (bst-insert 1 t2))
(define ord (bst-insert 21 t3))


(tree->list ord)


































  