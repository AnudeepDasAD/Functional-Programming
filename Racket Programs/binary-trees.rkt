;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname binary-trees) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
;;
;;************************************
;;   Anudeep Das (20782887)
;;   CS 135 Fall 2018
;;   Assignment 06, Problem 2                                    
;;*************************************

;; A Binary Search Tree (BST) is one of:
;; * empty
;; * a Node

(define-struct node (key left right))
;; A Node is a (make-node Nat BST BST)
;; requires: key > every key in left BST
;; key < every key in right BST



;;PART A

(define my-bst (make-node 5 (make-node 3 empty empty) (make-node 9
(make-node 7 empty empty) empty)))

(define no-more-nodes 0)


;;(bst-count mybst) consumes a BST and produces the
;;     total number of nodes in the BST

;;bst-count BST -> Nat

(define add-a-node 1)

(check-expect (bst-count (make-node 7 empty
                     (make-node 3 (make-node 7 empty (make-node 5 empty empty))
                                 (make-node 9 empty empty)))) 5)

(check-expect (bst-count (make-node 1 empty empty)) 1)

(define (bst-count mybst)
  (cond [(empty? mybst) no-more-nodes]
        ;; 1 counts the starting node
        [else (+ add-a-node (bst-count (node-right mybst))
                 (bst-count (node-left mybst)))]))


(check-expect
 (bst-count (make-node 2 empty (make-node 7 empty (make-node 3 empty empty))))3)

(check-expect (bst-count empty) 0)

(check-expect (bst-count (make-node 7 (make-node 4 empty empty)
                     (make-node 3 (make-node 7 empty (make-node 5 empty empty))
                                 (make-node 9 empty empty)))) 6)

(check-expect (bst-count (make-node 7 (make-node 4 (make-node 3 empty empty)
                                                   (make-node 2 empty empty))
                     (make-node 3 (make-node 7 empty (make-node 5 empty empty))
                                 (make-node 9 empty empty)))) 8)

(check-expect (bst-count (make-node 7 (make-node 4 (make-node 3
                                                      (make-node 1 empty empty)
                                                                     empty)
                                                   (make-node 2 empty empty))
                     (make-node 3 (make-node 7 empty (make-node 5 empty empty))
                                 (make-node 9 empty empty)))) 9)



;;PART B

;;(bst-add newkey mybst) consumes a key (as Nat) and a BST and produces a new
;;    BST with a new node added, where the new node contains the key

;;bst-add: Nat BST -> BST
;; requires: newkey is unique from every other node-key in mybst 

(check-expect (bst-add 3 (make-node 1 empty empty))
              (make-node 1 empty (make-node 3 empty empty)))

(check-expect (bst-add 4 my-bst)
              (make-node 5 (make-node 3 empty (make-node 4 empty empty))
                         (make-node 9 (make-node 7 empty empty) empty)))

(define (bst-add newkey mybst)
  (cond [(empty? mybst) (make-node newkey empty empty)]
        ;creates the current node as well
        [(< newkey (node-key mybst))
         (make-node (node-key mybst)
                    (bst-add newkey (node-left mybst)) (node-right mybst))]
        [(> newkey (node-key mybst))
         (make-node (node-key mybst)
                    (node-left mybst)(bst-add newkey (node-right mybst)))]
        [else "This key already exists"]))
         


(check-expect(bst-add 7 (make-node 2 empty
                                   (make-node 3 empty (make-node 7 empty empty))
                                   ))
             (make-node 2 empty (make-node 3 empty "This key already exists")))

(check-expect(bst-add 9 (make-node 2 empty (make-node 3 empty
                                                      (make-node 7 empty empty))
                                   ))
             (make-node 2 empty (make-node 3 empty (make-node 7 empty
                                                      (make-node 9 empty empty))
                         )))



(check-expect(bst-add 6 (make-node 2 empty (make-node 3 empty
                                           (make-node 7 empty empty))
                                   ))
             (make-node 2 empty (make-node 3 empty
                                           (make-node 7
                                                      (make-node 6 empty empty)
                                                      empty))))
                                   

(check-expect (bst-add 4 (make-node 2 empty
                                (make-node 3 empty (make-node 7 empty empty))))
             (make-node 2 empty (make-node 3 empty
                                (make-node 7 (make-node 4 empty empty) empty))))
                                   
      

(check-expect (bst-add 1 (make-node 2 empty (make-node 3 empty
                                                    (make-node 7 empty empty))
                                   ))
              (make-node 2 (make-node 1 empty empty) (make-node 3 empty
                                                      (make-node 7 empty empty))
                         ))

(check-expect (bst-add 8 (make-node 2 (make-node 3 empty empty)
                                   (make-node 7 empty empty)))
              (make-node 2 (make-node 3 empty empty)
                         (make-node 7 empty (make-node 8 empty empty))))
             





;;PART C

(define add-1-to-height 1)
(define empty-node-has-neg-height -1)

(define another-bst (make-node 5 (make-node 3 (make-node 1
(make-node 0 empty empty) (make-node 2 empty empty))
(make-node 4 empty empty)) (make-node 9 empty empty)))

;;(bst-height mybst)consumes a BST and produces the height (as Nat) of that BST.
;;  The height of a tree is defined as the maximum distance between the
;;  tree’s root and its leaves.

;bst-height: BST -> Nat


(check-expect (bst-height another-bst) 3)
(check-expect (bst-height empty) -1)


(define (bst-height mybst)
  (cond
    [(empty? mybst) empty-node-has-neg-height]
    ;condition for leaves, their additonal height has already been accounted for
    ;from the previous recursive call
    [(and (empty? (node-left mybst)) (empty? (node-right mybst))) no-more-nodes]
    
    ; if only a right child, it's additional height will be accounted for
    [(and (empty? (node-left mybst)) (not (empty? (node-right mybst))))
     (+ add-1-to-height (bst-height (node-right mybst)))]

    ;if only a left child, it's additional height will be accounted for
    [(and (not (empty? (node-left mybst))) (empty? (node-right mybst)))
     (+ add-1-to-height (bst-height (node-left mybst)))]

    ;if two children, just add 1 to the height as they are siblings
    ;(same height from roots)
    [else (+ add-1-to-height
             (cond [(>= (bst-height (node-left mybst))
                        (bst-height (node-right mybst)))
                    (bst-height (node-left mybst))]
                   [else (bst-height (node-right mybst))]))]))
    

(check-expect (bst-height (make-node 5 (make-node 3 (make-node 1
(make-node 0 empty empty) empty) empty) empty))3)

(check-expect (bst-height (make-node 5 empty
                               (make-node 6 empty (make-node 7 empty empty))))2)

(check-expect (bst-height (make-node 0 empty empty)) 0)

(check-expect (bst-height (make-node 5 (make-node 3 (make-node 1
(make-node 0 empty empty) (make-node 2 empty empty))
(make-node 4 empty empty)) empty)) 3)


(check-expect (bst-height (make-node 5 (make-node 3 (make-node 1
(make-node 0 empty empty) (make-node 2 empty empty))
(make-node 4 empty empty)) (make-node 6 (make-node 5.9 empty empty)
                           (make-node 7 (make-node 7.1 empty empty) empty)))) 3)


(check-expect (bst-height (make-node 5 (make-node 3 (make-node 2 (make-node 1
                          (make-node 0 empty empty) empty) empty)
                          (make-node 4 empty
                          (make-node 4.5 empty
                          (make-node 4.6 empty (make-node 4.7 empty empty)))))
                                     empty)) 5)




;;PART D

;;(bst-balanced? mybst) consumes a BST and
;;     produces true if it is balanced and false otherwise.
;;     A tree is considered balanced if the height difference
;;     between the left and right sub-tree of all
;;     nodes is no more than 1.

;;bst-balanced?: BST -> Bool


(define greatest-height-difference-acceptable 1)
(define single-node-is-balanced true)
(define empty-subtree-reached 0)

(check-expect(bst-balanced? my-bst)true)
(check-expect(bst-balanced? another-bst) false)

(define (bst-balanced? mybst)
  (cond
         ;if the either tree is empty, it is balanced
          
         [(and (empty? (node-left mybst)) (empty? (node-right mybst)))
          single-node-is-balanced]

         ;if one one the subtrees become empty, must simply check the difference
         ; between the height of the non-empty subtree against 0
         [(and (empty? (node-left mybst)) (not (empty? (node-right mybst))))
          (<= (abs (- empty-subtree-reached
                     (bst-height mybst)))
     greatest-height-difference-acceptable)]

         [(and (not(empty? (node-left mybst))) (empty? (node-right mybst)))
          (<= (abs (- empty-subtree-reached
                     (bst-height mybst)))
     greatest-height-difference-acceptable)]

         
        ;if the current left and right nodes are balanced, will check if their
        ; left and right are actually balanced, which they must be
        [(<= (abs (- (bst-height (node-left mybst))
                     (bst-height (node-right mybst))))
     greatest-height-difference-acceptable)
         (and (bst-balanced? (node-left mybst))
              (bst-balanced? (node-right mybst)))]
        
        [else false]))
        
(check-expect (bst-balanced? (make-node 5 (make-node 3 (make-node 1
(make-node 0 empty empty) empty) empty) empty))false)

(check-expect (bst-balanced? (make-node 5 empty
                               (make-node 6 empty (make-node 7 empty empty))))
              false)

(check-expect (bst-balanced? (make-node 0 empty empty)) true)

(check-expect (bst-balanced? (make-node 5 (make-node 3 (make-node 1
(make-node 0 empty empty) (make-node 2 empty empty))
(make-node 4 empty empty)) empty)) false)


(check-expect (bst-balanced? (make-node 5 (make-node 3 (make-node 1
(make-node 0 empty empty) (make-node 2 empty empty))
(make-node 4 empty empty)) (make-node 6 (make-node 5.9 empty empty)
                           (make-node 7 (make-node 7.1 empty empty) empty))))
              true)

(check-expect (bst-balanced? (make-node 5 (make-node 3 (make-node 1
(make-node 0 empty empty) (make-node 2 empty empty))
(make-node 4 empty empty)) (make-node 6 (make-node 5.9 empty empty)
                           (make-node 7 empty (make-node 7.1 empty
                                          (make-node 7.2 empty empty))))))
              false)

(check-expect (bst-balanced? (make-node 5 (make-node 3 (make-node 1
(make-node 0 empty empty) (make-node 2 empty empty))
(make-node 4 empty empty)) (make-node 6 (make-node 5.9 empty empty)
                           (make-node 7 (make-node 6.9 empty empty)
                                      (make-node 7.1 empty
                                          (make-node 7.2 empty empty))))))
              false)

(check-expect (bst-balanced? (make-node 5 (make-node 3 empty empty)
                                (make-node 6 empty (make-node 7 empty empty))))
              true)

(check-expect (bst-balanced? (make-node 5
                                        (make-node 3 empty
                                                   (make-node 4 empty empty))
                                        (make-node 6 empty empty)))
              true)


(check-expect (bst-balanced? (make-node 5 (make-node 3 (make-node 2 (make-node 1
                          (make-node 0 empty empty) empty) empty)
                          (make-node 4 empty
                          (make-node 4.5 empty
                          (make-node 4.6 empty (make-node 4.7 empty empty)))))
                                     empty)) false)






        