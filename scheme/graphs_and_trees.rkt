#lang racket

(define (++ i) (+ i 1))

(define (make_bin_tree root lhs rhs)
  (list root lhs rhs))

(define (empty_tree? g) (empty? g))

(define (get_root g) (car g))

(define (get_lhs g) (cadr g))

(define (get_rhs g) (caddr g))

(define (max a b)
  (if (< a b)
      b
      a))

(define (max_depth g)
  (if (empty_tree? g)
      0
      (+ 1 (max (max_depth (get_lhs g)) (max_depth (get_rhs g))))))

(define lhs (make_bin_tree 2 '(3 () ()) '(4 () ())))
(define rhs (make_bin_tree 5 '(6 () ()) '(7 (8 () (9 () ())) ())))
(define mytree (make_bin_tree 1 lhs rhs))














