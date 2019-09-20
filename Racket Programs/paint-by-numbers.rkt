;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname paint-by-numbers) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
;;
;;************************************
;;   Anudeep Das (20782887)
;;   CS 135 Fall 2018
;;   Assignment 06, Problem 1                                    
;;*************************************


;;PART A

;; A Grid is a (listof (listof (anyof 'B '-)))
;; requires: all sublists have the same length

(define fish-grid '((B B - - B B B B - - - B)
                    (- B B B - - - - B - - -)
                    (- B B B - - - - B B - -)
                    (- B - - B B B B B - - -)
                    (B B - - - - B - - - - -)))


(define empty-list-length 0)
(define one-more-element 1)

;; The following is a helper function
;;(length-finder mylist) finds the length of a given list

;;length-finder: (listof Any) -> Nat

(check-expect (length-finder (list 0 1 2 3)) 4)
              
(define (length-finder mylist)
  (cond [(empty? mylist) empty-list-length]
        [else (+ one-more-element (length-finder (rest mylist)))]))




;;The following is another helper function

(define no-more-columns 0)

;;(row-traversal col myrow) finds the col'th element in a given row.
;;  This function can work with any list, but it is being used for grid rows.

;;row-traversal: Nat (listof Any) -> Any

(check-expect (row-traversal 2 empty) empty)

(define (row-traversal col myrow)
  (cond [(empty? myrow) empty]
        [(= col no-more-columns) (first myrow)]
        [else (row-traversal (sub1 col) (rest myrow))]))




;; The following is the main function

;;(column col mygrid) consumes a natural number col and a Grid, and produces
;;    the colth column of the Grid, indexed from 0.
;;    If the colth column does not exist, the function produces empty.

;;column: Nat Grid -> Grid

(check-expect (column 2 fish-grid) '(- B B - - ))
(check-expect (column 32 fish-grid) empty)

(define (column col mygrid)
  (cond [(empty? mygrid) empty]
        [(>= col (length-finder (first mygrid))) empty]
        [else (cons (row-traversal col (first mygrid))
                    (column col (rest mygrid)))]))

(check-expect (column 0 fish-grid) '(B - - - B))
(check-expect (column 1 empty) empty)
(check-expect (column 12 fish-grid) empty)
(check-expect (column 11 fish-grid) '(B - - - - ))
(check-expect (column 0 (rest fish-grid)) '(- - - B))
(check-expect (column 0 (list '(B)
                              '(B)
                              '(-)
                              '(B))) '(B B - B))

              




;;PART B


(define no-b-at-first 0)

;; (b-counter num-of-b myrow) is an accumulator that determines the number of
;;  adjacent 'B, and then returns to the main function, with the traversed list
;;  once a non- 'B element is detected.

;;b-counter: Nat (listof (anyof 'B '-)) -> (listof Nat)

(check-expect (b-counter no-b-at-first empty) (list 0))

(define (b-counter num-of-b myrow)
  ;when 'B doesn't show up, finished accumulating and sending current list back
  (cond [(or (empty? myrow) (symbol=? (first myrow) '-))
         (cons num-of-b (cells->tallies myrow))]
        [(symbol=? (first myrow) 'B) (b-counter (add1 num-of-b) (rest myrow))]))
        

;;(cells->tallies myrow)  consumes a (listof (anyof ’B ’-)) and
;;   produces a list of tallies of adjacent black cells

;;cells->tallies: (listof (anyof 'B '-)) -> (listof Nat)

(check-expect (cells->tallies (first fish-grid)) (list 2 4 1))
(check-expect (cells->tallies empty) empty)


(define (cells->tallies myrow)
  (cond [(empty? myrow) empty]
        [(symbol=? (first myrow) '-)(cells->tallies (rest myrow))]
        [(symbol=? (first myrow) 'B)
         (b-counter no-b-at-first myrow)]))

(check-expect (cells->tallies '(B B B B B)) (list 5))
(check-expect (cells->tallies '(- - - - - -)) empty)
(check-expect (cells->tallies '(B B B B B - B - - B B B -)) (list 5 1 3))
(check-expect (cells->tallies '(- B - B B)) (list 1 2))
(check-expect (cells->tallies '(- - - B - B - -)) (list 1 1))




   




;;PART C


(define initial-column 0)
(define more-than-1-col-left 1)


;; (row-tallies mygrid) consumes a (listof (listof (anyof 'B '-)),
;;    and finds the (cells->tallies) for each (listof (anyof 'B '-))
;;    in the list.

;;row-tallies: (listof (listof (anyof 'B '-)) -> (listof (listof Nat))
;;requires:    Each (listof (anyof 'B '-)) must be the same length
;;             mygrid should be a grid 

(check-expect (row-tallies fish-grid) (list (list 2 4 1)
                                            (list 3 1)
                                            (list 3 2)
                                            (list 1 5)
                                            (list 2 1)))
(define (row-tallies mygrid)
  (cond [(empty? mygrid) empty]
        [else (cons (cells->tallies (first mygrid))
                    (row-tallies (rest mygrid)))]))



;; (column-list-maker starting-column mygrid) creates a list of columns
;;   for each column in a given Grid.

;;column-list-maker: Nat Grid -> (listof (listof (anyof 'B '-)))

(check-expect (column-list-maker 0 empty) empty)

(define (column-list-maker starting-column mygrid)
  (cond [(empty? mygrid) empty]
        [(= starting-column (length-finder (first mygrid))) empty]
        [else (cons (column starting-column mygrid)
                    (column-list-maker (add1 starting-column) mygrid))]))
           
        
(define starting-column 0)  
  
;; (puzzle-labels mygrid) consumes a Grid, and produces labels for the
;;      puzzle. Each label is represented by a list of natural numbers,
;;      corresponding to the adjacent black squares in that row or column.
;;      The function produces a list of length 2, where each element is
;;      itself a list of labels. The first element is a list of row labels
;;      (from top to bottom in the puzzle) and the second is a list of column
;;      labels (from left to right).

;;puzzle-labels: Grid -> (listof (listof (listof Nat))))

(check-expect (puzzle-labels empty) empty)

(define (puzzle-labels mygrid)
  (cond [(empty? mygrid) empty]
        [else (cons (row-tallies mygrid)
              (cons
               ;does row-tallies with columns instead of rows
               (row-tallies (column-list-maker starting-column mygrid))
               empty))]))


(check-expect (puzzle-labels '((B B - - B B B B - - - B)))
              (list (list (list 2 4 1))
                    (list (list 1)
                          (list 1)
                          empty
                          empty
                          (list 1)
                          (list 1)
                          (list 1)
                          (list 1)
                          empty
                          empty
                          empty
                          (list 1))))

(check-expect (puzzle-labels '((B)))
              (list (list (list 1))
                    (list (list 1))))

(check-expect (puzzle-labels (list '(B)
                              '(B)
                              '(-)
                              '(B)))
             (list (list (list 1)
                         (list 1)
                         empty
                         (list 1))
                   (list (list 2 1))))
                         
              
(check-expect (puzzle-labels fish-grid)
             (list (list (list 2 4 1)
                          (list 3 1)
                          (list 3 2)
                          (list 1 5)
                          (list 2 1))
                    (list (list 1 1)
                          (list 5)
                          (list 2)
                          (list 2)
                          (list 1 1)
                          (list 1 1)
                          (list 1 2)
                          (list 1 1)
                          (list 3)
                          (list 1)
                          empty
                          (list 1) )))

