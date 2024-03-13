#lang racket/base

;problem 1.1
(define (discount org)
  (cond
    [(string=? org "AAA") 0.15]
    [(string=? org "ACM") 0.25]
    [(string=? org "IEEE") 0.25]
    [(string=? org "UPE") 0.3]
    [else 0]))

;problem 1.2
(define (flip lst)
  (map (lambda (x) (- x)) lst))

;problem 2.1
(define (list-product list1 list2)
  (if (or (null? list1) (null? list2))
      (if (null? list1) (map (lambda (x) 0) list2) (map (lambda (x) 0) list1))
      (cons (* (car list1) (car list2)) (list-product (cdr list1) (cdr list2)))))

;problem 2.2
(define (merge-increasing list1 list2)
  (let rec ((l1 list1) (l2 list2) (acc '()))
    (cond
      [(and (null? l1) (null? l2)) (reverse acc)]
      [(null? l1) (rec l1 (cdr l2) (cons (car l2) acc))]
      [(null? l2) (rec (cdr l1) l2 (cons (car l1) acc))]
      [else
       (if (> (car l1) (car l2))
           (rec (cdr l1) l2 (cons (car l1) acc))
           (rec l1 (cdr l2) (cons (car l2) acc)))])))

;problem 3.1
(define (count-lists lst)
  (if (null? lst) 1
      (+ (if (list? (car lst)) (count-lists (car lst)) 0)
         (count-lists (cdr lst)))))

;problem 3.2
(define (compare-lists list1 list2)
  (cond
    [(null? list1) #t]
    [(> (car list1) (car list2))
     (compare-lists (cdr list1) (cdr list2))]
    [else #f]))

;problem 4.1
(define (shadow lst)
  (if (null? lst) '()
      (cons (car lst) (cons (car lst) (shadow (cdr lst))))))

;problem 4.2
(define (hunt-target target lst)
  (cond
    [(null? lst) #f]
    [(equal? target (car lst)) #t]
    [else (hunt-target target (cdr lst))]))
        
