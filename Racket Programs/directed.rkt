;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname directed) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
;;
;;************************************
;;   Anudeep Das (20782887)
;;   CS 135 Fall 2018
;;   Assignment 09, Problem 1                                    
;;*************************************


;; A Node is a Sym
;; A Graph is a (listof (list Node (listof Node)))




;;(valid-route? route dir-graph) determines whether a given route can be found
;;  in a given graph

;;valid-route?: (listof Node) Graph -> Bool
;;requires: dir-graph can also be empty

(check-expect (valid-route? '(A B C) a-graph) false)
(check-expect (valid-route? '(A B C) empty) false)
(check-expect (valid-route? empty a-graph) true)
(check-expect (valid-route? empty empty) true)
(check-expect (valid-route? '(X Y Z) a-graph) false)

(define (valid-route? route dir-graph)
  (cond [(empty? dir-graph)
         (cond [(empty? route) true]
               [else false])] ;;different values based on input
        
        [(empty? route) true] ;;reached an empty 
        [else
         ;searches for a node in a graph, provides it's neighbours if it is in the graph
         (local [(define (node-search mynode graph)
                   (cond [(empty? graph) false]
                         [(symbol=? (first(first graph)) mynode)
                          (first(rest(first graph)))] ;gives the neighbour-list if there is a match
                         [else (node-search mynode (rest graph))]))
                 
                 ;checks to see if the next node is a neighbour
                 (define (neighbour-search next-node neighbour-list)
                   (cond [(empty? neighbour-list) false]
                         [(false? neighbour-list) false]
                         [(symbol=? (first neighbour-list) next-node)
                          true]
                         [else (neighbour-search next-node (rest neighbour-list))]))]

           ;node-search finds the node in the graph and provides the neighbour list for the
           ;  neighbour-search

           (cond
             ;if the rest of the route is empty, must only check if the current node is in the
             ;  directed list, and if it is,returns true
             [(and(empty? (rest route)) (not (false? (node-search (first route) dir-graph))))
              true]
                 
                 [(neighbour-search (first (rest route))(node-search (first route) dir-graph))
                  (valid-route? (rest route) dir-graph)]
                 [else false]))]))
                  

(define a-graph
  '((A (C D E)) (B (E J)) (C ()) (D (F J))
                (E (K)) (F (K H)) (H ()) (J (H)) (K ())))

(define b-graph '((A (B)) (B (C)) (C(A))))
(define c-graph '((A ()) (B ()) (C ())))



(check-expect (valid-route? '(A D J H) a-graph) true)
(check-expect (valid-route? '(A) a-graph) true)
(check-expect (valid-route? '(H) a-graph) true)
(check-expect (valid-route? '(A B) a-graph) false)
(check-expect (valid-route? '(A C) a-graph) true)
(check-expect (valid-route? '(B E H) a-graph) false)
(check-expect (valid-route? '(B E K) a-graph) true)
(check-expect (valid-route? '(D F K) a-graph) true)
(check-expect (valid-route? '(D F K D) a-graph) false)
(check-expect (valid-route? '(A B C) b-graph) true)
(check-expect (valid-route? '(A B C A B C) b-graph) true)
(check-expect (valid-route? '(A B) c-graph) false)
(check-expect (valid-route? '(A empty) c-graph) false)
(check-expect (valid-route? '(D F H) a-graph) true)
(check-expect (valid-route? '(B J H) a-graph) true)







  
           
        