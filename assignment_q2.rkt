#lang racket

;Davin O'Brien
;06674062
;Assignment 2 Question 2

;Part 1
(provide ins_beg)

(define (ins_beg el lst)
  (cons el lst))
(display "part 1 the ins_beg function \n")
(ins_beg 'a '(b c d))

;Part 2
(provide ins_end)
(define (ins_end el lst)
  (append lst(list el)))
  (display "part 2 the ins_end function \n")
  (ins_end 'a '(b c d))

;Part 3
(provide cout_top_level)
(define (cout_top_level lst)
  (if (null? lst)
      0
      (+ 1 (cout_top_level (cdr lst)))))
(display "part 3 the cout_top_level function \n")
(cout_top_level '(1 2 (3 4 5)))

;part 4
(define (count_instances lst item)
    (cond ((null? lst) 0)
          ((= (car lst) item) (+ 1 (count_instances (cdr lst) item)))
          (else (count_instances (cdr lst) item))))
(display "part 4 the count_instances function:\n")
(count_instances '(1 2 3 4 2)2)

;part 5
(define (count_instances_tr item lst)
  (helper_count_instances_tr item lst 0))
 (define (helper_count_instances_tr item lst count)
  (cond [(empty? lst) count]
        [(equal? item (car lst)) (helper_count_instances_tr item (cdr lst) (+ 1 count))]
        [else (helper_count_instances_tr item (cdr lst) count)]))
(display "part 5 the count_instances_tr function:\n")
(count_instances_tr 2 '(1 3 2 4 2 2))

;part 6
(define (count_instances_deep item lst)
  (cond [(empty? lst) 0]
        [(list? (car lst))
         (+ (count_instances_deep item (car lst)) (count_instances_deep item (cdr lst)))]
        [(equal? item (car lst)) (+ 1 (count_instances_deep item (cdr lst)))]
[else (count_instances_deep item (cdr lst))]))
(display "part 6 the count_instances_deep function: \n")
(count_instances_deep 1 '(1 2 1 (1 3 4)))