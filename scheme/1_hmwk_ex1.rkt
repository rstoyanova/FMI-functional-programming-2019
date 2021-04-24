#lang racket

(define (zero . lst)
  (if (null? lst)
      0
      ((car lst) 0)))

(define (one . lst)
  (if (null? lst)
      1
      ((car lst) 1)))

(define (two . lst)
  (if (null? lst)
      2
      ((car lst) 2)))

(define (three . lst)
  (if (null? lst)
      3
      ((car lst) 3)))

(define (four . lst)
  (if (null? lst)
      4
      ((car lst) 4)))

(define (five . lst)
  (if (null? lst)
      5
      ((car lst) 5)))

(define (six . lst)
  (if (null? lst)
      6
      ((car lst) 6)))

(define (seven . lst)
  (if (null? lst)
      7
      ((car lst) 7)))

(define (eight . lst)
  (if (null? lst)
      8
      ((car lst) 8)))

(define (nine . lst)
  (if (null? lst)
      9
      ((car lst) 9)))

(define (times x) (lambda (y) (* x y)))

(define (plus x) (lambda (y) (+ x y)))

(define (minus x) (lambda (y) (- y x)))

(define (div x) (lambda (y) (/ y x)))






