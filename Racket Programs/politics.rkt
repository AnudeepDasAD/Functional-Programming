;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname politics) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
;;
;;************************************
;;   Anudeep Das (20782887)
;;   CS 135 Fall 2018
;;   Assignment 06, Problem 3                                    
;;*************************************

;;PART A

;; A Perk is a (list Nat Str)
;; requires: Nat > 0


;; A Perk-list is either
;; * empty
;; * (cons (list Str (listof Perk)) Perk-list)
;; requires: Perks in (listof Perks) sorted by compliancy score
;; (non-increasing)


(define short-perklist
(list
 (list "Peter Smith" (list
                      (list 83 "50,000 dollars campaign funding")
                      (list 32 "Public support by your company")
                      (list 13 "Opera tickets")))
 (list "Jennifer O'Brien" (list
                           (list 137 "Position on the Board of Directors")
                           (list 22 "Arranging photo-op with CEO")))
 (list "Steve Li" (list
                   (list 91 "Sponsored TV ads")
                   (list 56 "Invitation as keynote-speaker")
                   (list 9 "Positive press release in his favour")
                   (list 5 "Business dinner with CTO")))))


;;(sorter existing-perks) sorts a (listof Perks) by their score
;;sorter: (listof Perks) -> (listof Perks)

(check-expect (sorter (list
                      (list 83 "50,000 dollars campaign funding")
                      (list 13 "Opera tickets")
                      (list 32 "Public support by your company")))
                      (list
                      (list 83 "50,000 dollars campaign funding")
                      (list 32 "Public support by your company")
                      (list 13 "Opera tickets")))
                      
                      
(define (sorter existing-perks)
  (cond [(empty? existing-perks) empty]
        [else (insert (first existing-perks) (sorter (rest existing-perks)))]))


;;(insert new-perk existing-perks) inserts a new Perk into a sorted
;;  (listof Perk) (sorted by score number), such that the final list is sorted

;;insert: Perk (listof Perk) -> (listof Perk)
;;requires: exisitng-perks must be sorted from highest to lowest score

(check-expect (insert (list 32 "Some other thing")
                      (list
                      (list 83 "50,000 dollars campaign funding")
                      (list 32 "Public support by your company")))
              (list
                      (list 83 "50,000 dollars campaign funding")
                      (list 32 "Some other thing")
                      (list 32 "Public support by your company")))

(define (insert new-perk existing-perks)
  (cond [(empty? existing-perks) (cons new-perk existing-perks)]
        [(>= (first new-perk) (first(first existing-perks)))
         (cons new-perk existing-perks)]
        [else (cons (first existing-perks)
                    (insert new-perk (rest existing-perks)))]))

        

;;(adding-perks new-perks existing-perks) appends the new perks to the existing
;;  perk list

;;adding-perks: (listof Perk) (listof Perk) -> (listof Perk)

(check-expect (adding-perks (list
                          (list 30 "Two free flights in company jet")
                          (list 3 "Guided company tour"))
                          (list
                           (list 137 "Position on the Board of Directors")
                           (list 22 "Arranging photo-op with CEO")))
              (list
               (list 30 "Two free flights in company jet")
               (list 3 "Guided company tour")
               (list 137 "Position on the Board of Directors")
               (list 22 "Arranging photo-op with CEO")))

(define (adding-perks new-perks existing-perks)
  (cond [(empty? new-perks) existing-perks]
        [else (append new-perks existing-perks)]))
         
  


;;(add-perks poli-name poli-perks perklist) consumes a politician’s name,
;;   a list of Perk items (sorted in non-increasing order by compliancy score),
;;   and a Perk-list. If the politician does not exist in the Perk-list,
;;   a new Perk-list is produced consisting of the politician and their perks
;;   added to the end of the Perk-list. If the politician does exist, then the
;;   resulting Perk-list consists of the politician’s new Perks merged into
;;   their existing list of Perks. If a Perk with the same compliancy scores
;;   as an existing perk is added to the Perk-list,
;;   the new Perks take precedence, i.e., it occurs before the existing one in
;;   the new list.

;;add-perks: Str (listof Perk) Perk-list -> Perk-list


(check-expect (add-perks "Jennifer O'Brien"
                         (list
                          (list 30 "Two free flights in company jet")
                          (list 3 "Guided company tour"))
                         short-perklist)
              (list
               (list "Peter Smith" (list
                                    (list 83 "50,000 dollars campaign funding")
                                    (list 32 "Public support by your company")
                                    (list 13 "Opera tickets")))
               (list "Jennifer O'Brien" (list
                               (list 137 "Position on the Board of Directors")
                               (list 30 "Two free flights in company jet")
                                         (list 22 "Arranging photo-op with CEO")
                                         (list 3 "Guided company tour")))
               (list "Steve Li" (list
                                 (list 91 "Sponsored TV ads")
                                 (list 56 "Invitation as keynote-speaker")
                                 (list 9 "Positive press release in his favour")
                                 (list 5 "Business dinner with CTO")))))



(define (add-perks poli-name new-perks perklist)
  (cond [(empty? perklist) (cons (list poli-name new-perks) empty)]
        [(string=? poli-name (first (first perklist)))
         (cons (cons poli-name
               (cons
                (sorter (adding-perks new-perks (second (first perklist))))
                           empty))
               (rest perklist))]
        [else (cons (first perklist)
                    (add-perks poli-name new-perks (rest perklist)))]))


(check-expect (add-perks "James McAvoy"
                         (list
                          (list 30 "Two free flights in company jet")
                          (list 3 "Guided company tour")
                          (list 5 "Some other thing"))
                         empty)
              (list (list "James McAvoy"
                         (list
                          (list 30 "Two free flights in company jet")
                          (list 3 "Guided company tour")
                          (list 5 "Some other thing")))))


(check-expect (add-perks "Jennifer O'Brien"
                         (list
                          (list 30 "Two free flights in company jet")
                          (list 3 "Guided company tour")
                          (list 5 "Some other thing"))
                         short-perklist)
              (list
               (list "Peter Smith" (list
                                    (list 83 "50,000 dollars campaign funding")
                                    (list 32 "Public support by your company")
                                    (list 13 "Opera tickets")))
               (list "Jennifer O'Brien"
                     (list
                                (list 137 "Position on the Board of Directors")
                                (list 30 "Two free flights in company jet")
                                (list 22 "Arranging photo-op with CEO")
                                (list 5 "Some other thing")
                                (list 3 "Guided company tour")))
               (list "Steve Li" (list
                                 (list 91 "Sponsored TV ads")
                                 (list 56 "Invitation as keynote-speaker")
                                 (list 9 "Positive press release in his favour")
                                 (list 5 "Business dinner with CTO")))))

(check-expect (add-perks "J LO"
                         (list
                          (list 30 "Two free flights in company jet")
                          (list 3 "Guided company tour")
                          (list 5 "Some other thing"))
                         short-perklist)
              (list
               (list "Peter Smith" (list
                                    (list 83 "50,000 dollars campaign funding")
                                    (list 32 "Public support by your company")
                                    (list 13 "Opera tickets")))
               (list "Jennifer O'Brien" (list
                                (list 137 "Position on the Board of Directors")
                                (list 22 "Arranging photo-op with CEO")))
               (list "Steve Li" (list
                                 (list 91 "Sponsored TV ads")
                                 (list 56 "Invitation as keynote-speaker")
                                 (list 9 "Positive press release in his favour")
                                 (list 5 "Business dinner with CTO")))
               (list "J LO" (list
                          (list 30 "Two free flights in company jet")
                          (list 3 "Guided company tour")
                          (list 5 "Some other thing")))))

(check-expect (add-perks "Steve Li" empty short-perklist)
              short-perklist)

(check-expect (add-perks "Steve Li" (list
                          (list 30 "Two free flights in company jet")
                          (list 3 "Guided company tour")
                          (list 5 "Some other thing"))
                         empty)
              (list (list "Steve Li"
                    (list
                          (list 30 "Two free flights in company jet")
                          (list 3 "Guided company tour")
                          (list 5 "Some other thing")))))








;;PART B



(define not-on-list/not-qualified 'wristwatch)
(define negative-compliancy-points 'smear-campaign)
(define no-compliancy-points 0)


;;The following is a helper function
;;(perk-determination comp-score list-of-perk) determines the perk that a
;;  politician receives based on their compliancy score and the perks they are
;;  qualified for.

;perk-determination: Num (listof Perk) -> (listof (listof Perk))

(check-expect (perk-determination 25 (list
                                (list 137 "Position on the Board of Directors")
                                     (list 22 "Arranging photo-op with CEO")
                                     (list 3 "A stricker with my face")))
               (list(list 22 "Arranging photo-op with CEO")
                    (list 3 "A stricker with my face")))

(define (perk-determination comp-score list-of-perk)
  (cond 
        [(empty? list-of-perk) empty]
        [(>= comp-score (first(first list-of-perk)))
         (cons (first list-of-perk)
               (perk-determination comp-score (rest list-of-perk)))]
        [else (perk-determination comp-score (rest list-of-perk))]))
        


;; (perk-received poli-name comp-score perklist) consumes a politician’s name,
;;     compliancy score, and Perk-list and produces either a symbol or a string
;;     according to their compliancy score.

;; perk-received: Str Num Perk-list -> (anyof Str 'smear-campaign 'wristwatch)

(check-expect (perk-received "Steve Li" 12 short-perklist)
              "Positive press release in his favour")
(check-expect (perk-received "Peter Smith" -25 short-perklist)
              'smear-campaign)

(define (perk-received poli-name comp-score perklist)
  (cond [(< comp-score no-compliancy-points) negative-compliancy-points] 
        [(empty? perklist) not-on-list/not-qualified]
        [(string=? poli-name (first(first perklist)))
         ;the second element is the string, the first of the sorted list is the
         ; perk that has the best (highest compliancy requirement)
         (cond
           [(empty? (perk-determination comp-score (second(first perklist))))
            not-on-list/not-qualified]
           [else
(second(first(sorter(perk-determination comp-score (second(first perklist))))))])]
        [else (perk-received poli-name comp-score (rest perklist))]))


(check-expect(perk-received "Steve Li" 56 short-perklist)
             "Invitation as keynote-speaker")
(check-expect(perk-received "Steve Li" 1 short-perklist)
             'wristwatch)
(check-expect (perk-received "Jennifer O'Brien" 25 short-perklist)
              "Arranging photo-op with CEO")
(check-expect (perk-received "Noton Thelist" 43 short-perklist)
              'wristwatch)




;;PART C

;; An Action is a (list Str Int Str)

(define-struct actionnode (name score actions left right))
;; An ActionNode is a (make-actionnode Str Int (listof Action)
;;    ActionSearchTree ActionSearchTree)
;; requires:
;; (string<? x name) is true for every (actionnode-name x)
;;    in the left subtree
;; (string>? x name) is true for every (actionnode-name x)
;;    in the right subtree


;; An ActionSearchTree is one of:
;; * empty
;; * an ActionNode

(define short-ast (make-actionnode "Amanda Byers" -5
                              (list
                               (list "Amanda Byers" -5 "Met with competitor"))
empty empty))


;;(add-action myaction action-tree) consumes an Action and an ActionSearchTree.
;;   It produces an ActionSearchTree consisting of the original tree
;;   with the new Action added, either creating an ActionNode
;;   or updating an existing ActionNode’s score and action list.

(check-expect (add-action (list "Amanda Byers" -5 "Met with competitor") empty)
short-ast)

(check-expect(add-action (list "Jennifer O'Brien" -5 "Met with competitor")
                          (make-actionnode "Amanda Byers" 3
                                           (list "Walked my dog") empty
                                           empty))
              (make-actionnode "Amanda Byers" 3 (list "Walked my dog") empty
                               (make-actionnode "Jennifer O'Brien" -5
                               (list
                            (list "Jennifer O'Brien" -5 "Met with competitor"))
                                                empty empty)))

(define (add-action myaction action-tree)
  (cond [(empty? action-tree) (make-actionnode (first myaction)
                                               (second myaction)
                                            (cons myaction empty) empty empty)]
        ;creates the current node as well
        [(string<? (first myaction) (actionnode-name action-tree))
         (make-actionnode (actionnode-name action-tree)
                          (actionnode-score action-tree)
                          (actionnode-actions action-tree)
                    (add-action myaction (actionnode-left action-tree))
                    (actionnode-right action-tree))]
        
        [(string>? (first myaction) (actionnode-name action-tree))
         (make-actionnode (actionnode-name action-tree)
                          (actionnode-score action-tree)
                          (actionnode-actions action-tree)
                     (actionnode-left action-tree)
                     (add-action myaction (actionnode-right action-tree)))]
        
        [(string=? (first myaction) (actionnode-name action-tree))
         (make-actionnode (first myaction)
                        (+ (second myaction) (actionnode-score action-tree))
                        (cons myaction (actionnode-actions action-tree))
               (actionnode-left action-tree) (actionnode-right action-tree))])) 


(check-expect
 (add-action (list "Amanda Byers" 7 "Argued on talk radio against raising minimum wage") short-ast)
 (make-actionnode "Amanda Byers" 2
     (list
    (list "Amanda Byers" 7 "Argued on talk radio against raising minimum wage")
      (list "Amanda Byers" -5 "Met with competitor"))
                  empty empty))
                 

(check-expect
 (add-action (list "Alna May" 6 "Recommended my movies.")
              (make-actionnode "Amanda Byers" 2
   (list
    (list "Amanda Byers" 7 "Argued on talk radio against raising minimum wage")
        (list "Amanda Byers" -5 "Met with competitor"))
                  empty empty))
 (make-actionnode "Amanda Byers" 2
   (list
    (list "Amanda Byers" 7 "Argued on talk radio against raising minimum wage")
         (list "Amanda Byers" -5 "Met with competitor"))

                  (make-actionnode "Alna May" 6
                                   (list
                                   (list "Alna May" 6 "Recommended my movies."))
                                   empty empty)
                                   empty))
 


;;PART D

;; (progressor ast listofperk) determines whether to send the left node or the
;;   right node to be processed by perk-list function, depneding on which of
;;   the nodes are empty

;;progressor: ActionSearchTree (listof Perk) ->
;;      (listof (list Str (anyof (anyof 'smear-campaign 'wristwatch) Str)))
;;      (through mutual recursion of perk-list, thus it should produce the same
;;      output as perk-list for the same parameters.
                          
                          
(define (progressor ast listofperk)
  (cond [(empty? (actionnode-right ast))
         (perk-list (actionnode-left ast) listofperk)]
        [(empty? (actionnode-left ast))
         (perk-list (actionnode-right ast) listofperk)]
        [else
         (cons (perk-list (actionnode-right ast) listofperk)
               (perk-list (actionnode-left ast) listofperk))]))


;; (insert-string new-perk-pair perk-received) inserts a
;;   (list Str (anyof Str 'wristwatch 'smear-campaign)) into a list of
;;   (list Str (anyof Str 'wristwatch 'smear-campaign)) that are sorted in
;;   alphabetical order based on the first Str (which is the name of the person)

;;insert-string: (list Str (anyof Str 'wristwatch 'smear-campaign)) ->
;;               (list Str (anyof Str 'wristwatch 'smear-campaign))

(check-expect (insert-string (list "Amanda Byers" 'smear-campaign)
                          (list
                        (list "Jennifer O'Brien" "Arranging photo-op with CEO")
               (list "Steve Li" "Positive press release in his favour")))
              (list
               (list "Amanda Byers" 'smear-campaign)
               (list "Jennifer O'Brien" "Arranging photo-op with CEO")
               (list "Steve Li" "Positive press release in his favour")))
              
                             
(define (insert-string new-perk-pair perk-received)
  (cond [(empty? perk-received) (cons new-perk-pair empty)]
        [(string<? (first new-perk-pair) (first(first perk-received)))
         (cons new-perk-pair perk-received)]
        [else (cons (first perk-received)
                    (insert-string new-perk-pair (rest perk-received)))]))


;;(string-sorter perk-received) takes a
;;  (listof (list Str (anyof Str 'wristwatch 'smear-campaign)) and sorts them
;;  in ascending alphabetical order based on the first letter of the first
;;  string

;;string-sorter: (listof (list Str (anyof Str 'wristwatch 'smear-campaign))->
;;               (listof (list Str (anyof Str 'wristwatch 'smear-campaign))



(define (string-sorter perk-received)
  (cond [(empty? perk-received) empty]
        [else (insert-string (first perk-received)
                             (string-sorter (rest perk-received)))]))



;;perk-list-disorg is a wrapper function of the perk-list and produces the
;;  necessary final list thatis not sorted in order

;;perk-list-disorg: ActionSearchTree Perk-List ->
;;           (listof (list Str (anyof (anyof 'smear-campaign 'wristwatch) Str)))


(define (perk-list-disorg ast listofperk)
  (cond [(empty? ast) empty]
        [else (cons (list (actionnode-name ast) (perk-received
                                                 (actionnode-name ast)
                                                         (actionnode-score ast)
                                                         listofperk))
                    (progressor ast listofperk))]))



;;(perk-list ast listofperk) consumes an ActionSearchTree and a Perk-list,
;;    and produces a list of pairs. The first element of each pair is the
;;    politician’s name, and the second is the perk the politician receives
;;    (which might be a string representing the received gift, or one of the
;;    symbols ’wristwatch or ’smear-campaign). The result should be in
;;    increasing alphabetical order of politician’s name,
;;    with all names in the ActionSearchTree represented. (If a name is in the
;;    Perk-list but not in the ActionSearchTree it is ignored.)


;;perk-list: ActionSearchTree Perk-List ->
;;           (listof (list Str (anyof (anyof 'smear-campaign 'wristwatch) Str)))


  
                   ;(perk-list (actionnode-left ast) listofperk)
                   ;(perk-list (actionnode-right ast) listofperk)))]))

(check-expect (perk-list
               (make-actionnode "Amanda Byers" -5
               (list (list "Amanda Byers" -5 "Met with competitor"))
                empty
               (make-actionnode "Steve Li" 12
               (list (list "Steve Li" 12 "Plays golf with your second cousin"))
              (make-actionnode "Jennifer O'Brien" 25
              (list (list "Jennifer O'Brien" 30 "Pushed major contract for your
                      company")
                    (list "Jennifer O'Brien" 5 "Mentioned your company on 
                     morning TV")
                    (list "Jennifer O'Brien" -10 "Questioned your leadership in
                     public"))
                empty
                empty)
                empty))
               (list
                (list "Jennifer O'Brien"
                      (list
                       (list 137 "Position on the Board of Directors")
                       (list 30 "Two free flights in company jet")
                       (list 22 "Arranging photo-op with CEO")
                       (list 3 "Guided company tour")))
                (list "Steve Li"
                      (list
                       (list 91 "Sponsored TV ads")
                       (list 56 "Invitation as keynote-speaker")
                       (list 9 "Positive press release in his favour")
                       (list 5 "Business dinner with CTO")))))
              (list
               (list "Amanda Byers" 'smear-campaign)
               (list "Jennifer O'Brien" "Arranging photo-op with CEO")
               (list "Steve Li" "Positive press release in his favour")))


(define (perk-list ast listofperk)
  (string-sorter (perk-list-disorg ast listofperk)))



(check-expect (perk-list
               (make-actionnode "Amanda Byers" -5
               (list (list "Amanda Byers" -5 "Met with competitor"))
                (make-actionnode "Alna May" 6
                                   (list
                                   (list "Alna May" 6 "Recommended my movies."))
                                   empty empty)
               (make-actionnode "Steve Li" 12
               (list (list "Steve Li" 12 "Plays golf with your second cousin"))
              (make-actionnode "Jennifer O'Brien" 25
              (list (list "Jennifer O'Brien" 30 "Pushed major contract for your
                      company")
                    (list "Jennifer O'Brien" 5 "Mentioned your company on 
                     morning TV")
                    (list "Jennifer O'Brien" -10 "Questioned your leadership in
                     public"))
                empty
                empty)
                empty))
               (list
                (list "Jennifer O'Brien"
                      (list
                       (list 137 "Position on the Board of Directors")
                       (list 30 "Two free flights in company jet")
                       (list 22 "Arranging photo-op with CEO")
                       (list 3 "Guided company tour")))
                (list "Steve Li"
                      (list
                       (list 91 "Sponsored TV ads")
                       (list 56 "Invitation as keynote-speaker")
                       (list 9 "Positive press release in his favour")
                       (list 5 "Business dinner with CTO")))))
              (list
               (list "Alna May" 'wristwatch)
               (list "Amanda Byers" 'smear-campaign)
               (list "Jennifer O'Brien" "Arranging photo-op with CEO")
               (list "Steve Li" "Positive press release in his favour")))
  
  
  