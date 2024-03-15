#lang racket/base

;problem 1.1
(define (discount org) ;Here our discounts are based on the name of the Org
  (cond
    [(string=? org "AAA") 0.15]
    [(string=? org "ACM") 0.25]
    [(string=? org "IEEE") 0.25]
    [(string=? org "UPE") 0.3]
    [else 0]))

;problem 1.2
(define (flip lst)
  (map (lambda (x) (- x)) lst)) ;Each element is negated and its sign is flipped

;problem 2.1
(define (list-product list1 list2)
  (if (or (null? list1) (null? list2)) ;If list1 n list2 are empty
      (if (null? list1) (map (lambda (x) 0) list2) (map (lambda (x) 0) list1))
      (cons (* (car list1) (car list2)) (list-product (cdr list1) (cdr list2)))))
;Else calculate the product of the first element of each list and continue

;problem 2.2
(define (merge-increasing list1 list2)
  (let loop ((l1 (reverse list1)) (l2 (reverse list2)) (result '())) ;Loop reverses our lists
    (cond
      ((and (null? l1) (null? l2)) (reverse result)) ;l1 l2 and empty
      ((null? l1) (loop l1 (cdr l2) (cons (car l2) result))) ;l1 empty l2 not
      ((null? l2) (loop (cdr l1) l2 (cons (car l1) result))) ;l2 empty l1 not
      (else ;both arent empty
       (if (<= (car l1) (car l2))
           (loop (cdr l1) l2 (cons (car l1) result))
           (loop l1 (cdr l2) (cons (car l2) result)))))))
;Placing depends on which list has the bigger first element

;problem 3.1
(define (count-lists lst)
  (if (null? lst) 1
      (+ (if (list? (car lst)) (count-lists (car lst)) 0)
         (count-lists (cdr lst)))))
;If the list is empty we return 1
;Else- We add the count of sublists in the first element to the count of sublists in our list.

;problem 3.2
(define (compare-lists list1 list2)
  (cond
    [(null? list1) #t] ;If both our lists are empty we return #t (true)
    [(> (car list1) (car list2))
     (compare-lists (cdr list1) (cdr list2))]
    [else #f])) ;#f if our comparison fails


;problem 4.1
(define (shadow lst)
  (if (null? lst) '() ;If list is empty return an empty list
      (cons (car lst) (cons (car lst) (shadow (cdr lst))))))
;If it is not empty, we duplicate the first element and then continue

;problem 4.2
(define (hunt-target target lst)
  (cond
    [(null? lst) #f] ;If list is empty, target not found (#f)
    [(equal? target (car lst)) #t] ;target matches our first element (#t)
    [(list? (car lst)) (or (hunt-target target (car lst)) (hunt-target target (cdr lst)))]
    [else (hunt-target target (cdr lst))])) ;Else we keep executing the rest of the list to find out target


        
