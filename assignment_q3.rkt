#lang racket

;Davin O'Brien
;06674062
;Assignment 2 Question 3

(provide tree)
(provide value)
(provide left_tree)
(provide right_tree)

(define tree '(((() 1 ()) 5 (() 8 ())) 16 ((() 27 ()) 33 (() 49 ()))))

(define (left_tree tree)
  (car tree))

(define (right_tree tree)
  (caddr tree))

(define (value tree)
  (cadr tree))

;part 1 sort tree
(display "Part 1:\n")
(provide sort_tree)
(define (sort_tree tree)
    (begin  (cond [ (not (null? (left_tree tree))) (sort_tree (left_tree tree))])
            
            (display (value tree))
            
            (display " ")
            (cond [ (not (null? (right_tree tree))) (sort_tree (right_tree tree))])
    )
)
(display"sorted binary tree \n")
(sort_tree tree)


;part 2 display true or false if item in tree
(display "\n\nPart 2:\n")
(provide is_present)
(define (is_present item tree)
  (cond
    ((null? tree) #f)
    ((= item (cadr tree)) #t)
    ((< item (cadr tree)) (is_present item (left_tree tree)))
    (else (is_present item (right_tree tree)))))
(display "test if displays true if value is present: \n")
(is_present 5 tree)
(display "test if displays false if value is not present: \n")
(is_present 11 tree)

;part 3 inserting an item into a binary tree
(display "\n\nPart 3:\n")
(provide insert)
(define (insert el tree)
  (cond
    [(empty? tree) (list '() el '())]
    [(equal? el (cadr tree)) tree]
    [(< el (cadr tree))
    (list ( insert el (left_tree tree)) (cadr tree) (right_tree tree))]
    [else (list (left_tree tree) (cadr tree) (insert el (right_tree tree)))]))
(display "Tree after 17 is inserted \n")
         (insert 17 tree);inserting 17 into the tree

;part 4 inserting a list into a binary tree
(display "\n\nPart 4 \n")
(provide insert_list)
(define (insert_list items tree)
  (if (null? items) tree
      (insert_list (cdr items) (insert (car items) tree))))
(display "Tree after list of (20 3 6 5) is inserted \n")
(insert_list '( 20 3 6 15)tree)

;part 5 tree sort algorithm
(display "\n\nPart 5 \n")
(provide tree_sort_algorithm)
         (define (tree_sort_algorithm items)
  (sort_tree (insert_list items '())))
(display "tree-sort algorithm\n")
(define tree_to_sort '(4 20 5 13 17 52 21))
(tree_sort_algorithm tree_to_sort)
         
