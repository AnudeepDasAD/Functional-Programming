;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname cs135search) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
;;
;;************************************
;;   Anudeep Das (20782887)
;;   CS 135 Fall 2018
;;   Assignment 05, Problem 4                                    
;;*************************************


;;PART A

;; A doc-list (DL) is one of:
;; * empty
;; * (cons Str DL)
;; requires: each doc (i.e. Str) only occurs once in the doc-list
;; the doc-list is in ascending alphabetical order


;; An Inverted List (IL) is one of:
;; * empty
;; * (cons (list Str DL) IL)
;; requires: each key (i.e. Str) only occurs once in the IL.
;; the keys occur in ascending alphabetical order in the IL.


;; The following is a helper function

;;(both-with-original-lists dl1 dl2 original-dl2) consumes three DLs, of which
;;  the first two are distinct, and the third list is the duplicate of the
;;  second list, which exists in order to restore the second list after
;;  recursing through it. The function produces a list of documents
;;  (possibly empty) that occur in both of the first 2 DLs.

;; both-with-original-lists: DL DL DL -> DL
;; requires: all DL's are valid DL's
;;           original-dl2 is equal to dl2 in the beginning

(check-expect(both-with-original-lists (list "b.txt") (list "b.txt" "c.txt")
                                       (list "b.txt" "c.txt"))
             (list "b.txt"))

(define (both-with-original-lists dl1 dl2 original-dl2)
  (cond [(empty? dl1) empty]
        ;will need to start over with the next value in dl1
        [(empty? dl2)
         (both-with-original-lists (rest dl1) original-dl2 original-dl2)]
        ;if there is a match, that value is added to a list and the rest of dl1
        ;is analysed 
        [(string=? (first dl1) (first dl2))
         (cons (first dl1)
               (both-with-original-lists (rest dl1) original-dl2 original-dl2))]

        ;if no match the next value in dl2 is checked for matching
        [else (both-with-original-lists dl1 (rest dl2) original-dl2)]))
       




;;(both dl1 dl2) consumes two DLs and produces a list of documents
;;               (possibly empty) that occur in both DLs.

;; both: DL DL -> DL
;; requires: all DL's are valid 

(check-expect(both (list "b.txt") (list "b.txt" "c.txt"))(list "b.txt"))
(check-expect(both (list "b.txt" "c.txt") (list "b.txt" "c.txt"))
             (list "b.txt" "c.txt"))

(define (both dl1 dl2)
  (both-with-original-lists dl1 dl2 dl2))


(check-expect(both (list "a.txt" "b.txt" "c.txt") (list "b.txt" "c.txt"))
             (list "b.txt" "c.txt"))
(check-expect(both (list "a.txt" "b.txt" "c.txt") (list "b.txt"))
             (list "b.txt"))
(check-expect(both empty (list "b.txt"))empty)
(check-expect(both (list "a.txt" "b.txt") (list "c.txt" "b.txt"))
             (list "b.txt"))
(check-expect(both (list "a.txt" "b.txt" "c.txt") empty) empty)








;;PART B

;; (exclude-with-original-lists dl1 values-in-both-lists
;;                                     values-in-both-lists-original)
;;      consumes 3 DLs and produces a list of documents
;;      (possibly empty) that occur in the first DL but not in
;;      values-in-both-lists.
;;      values-in-both-lists-original is required to restore
;;      values-in-both-lists after recursing through it.

;; exclude-with-original-lists: DL DL DL -> DL
;; requires: values-in-both-lists is (both dl1 dl2) where dl2
;;           is the dl we are comparing to dl1 to find the documents exclusive
;;           to dl1.
;;           values-in-both-lists-original is equal to values-in-both-lists
;;           in the beginning.
;;           All DL's must be valid.

(check-expect(exclude-with-original-lists (list "b.txt" "c.txt")
                                   (both (list "b.txt" "c.txt") (list "b.txt"))
                                   (both (list "b.txt" "c.txt") (list "b.txt")))
             (list "c.txt"))
              
(define (exclude-with-original-lists dl1 values-in-both-lists
                                     values-in-both-lists-original)
  (cond [(empty? dl1) empty]
        ;reached end of values means that the element is not common
        [(empty? values-in-both-lists) 
         (cons (first dl1) (exclude-with-original-lists (rest dl1)
                            values-in-both-lists-original
                            values-in-both-lists-original))]
        ;if there is a match, analyse rest of dl1
        [(string=? (first values-in-both-lists) (first dl1))
         (exclude-with-original-lists (rest dl1) values-in-both-lists-original
                                     values-in-both-lists-original)]

        ;else move on to the next similar value
        [else (exclude-with-original-lists dl1 (rest values-in-both-lists)
                                     values-in-both-lists-original)]))
                   
         
    


;; (exclude dl1 dl2) consumes two DLs and produces a list of documents
;;      (possibly empty) that occur in the first DL but not the second one

;;exclude: DL DL -> DL
;;requires: all DL's must be valid

(check-expect(exclude (list "b.txt" "c.txt") (list "b.txt"))(list "c.txt"))
(check-expect(exclude (list "b.txt") (list "b.txt"))empty)

(define (exclude dl1 dl2)
  (exclude-with-original-lists dl1 (both dl1 dl2) (both dl1 dl2)))


(check-expect(exclude empty (list "b.txt"))empty)
(check-expect(exclude (list "b.txt" "c.txt") empty)(list "b.txt" "c.txt"))
(check-expect(exclude (list "b.txt" "c.txt") (list "b.txt" "c.txt"))
             empty)
(check-expect(exclude (list "c.txt" "d.txt") (list "d.txt"))(list "c.txt"))







;;PART C

;;The following is a helper fucntion
;; (search-single-string str an-IL)determines the DL in an-IL (a given IL) that
;;  contains a given string

(check-expect (search-single-string "cat"
                                    (list (list "barks" (list "b.txt"))
                                          (list "cat" (list "a.txt" "c.txt"))
                                          (list "chases" (list "c.txt"))
                                          (list "dog" (list "b.txt" "c.txt"))
                                          (list "sleeps" (list "a.txt"))
                                          (list "suddenly" (list "c.txt"))
                                   (list "the" (list "a.txt" "b.txt" "c.txt"))))
              (list "a.txt" "c.txt"))

(check-expect (search-single-string "lolz"
                                    (list (list "barks" (list "b.txt"))
                                          (list "cat" (list "a.txt" "c.txt"))
                                          (list "chases" (list "c.txt"))
                                          (list "dog" (list "b.txt" "c.txt"))
                                          (list "sleeps" (list "a.txt"))
                                          (list "suddenly" (list "c.txt"))
                                   (list "the" (list "a.txt" "b.txt" "c.txt"))))
              empty)

(define (search-single-string str an-IL)
  (cond [(empty? an-IL) empty]
        [(string=? (first (first an-IL)) str) (first(rest(first an-IL)))]
        [else (search-single-string str (rest an-IL))]))



;;(search both-or-exclude str1 str2 an-IL)
;;   consumes a Sym, two Strs and an IL. It produces a (possibly empty)
;;   list of documents. The arguments for search will always be in one
;;   of two possible formats:
;;     • (search ’both str1 str2 an-IL) which produces a list of documents from
;;           an-IL that contains both the words str1 and str2 and
;;     • (search ’exclude str1 str2 an-IL) which produces a list of documents
;;           from an-IL that contains the word str1 but not the word str2.

;;search: Sym Str Str IL -> DL
;; requires: both-or-exclude is (anyof 'both or 'exclude)
;;           IL isa a valid IL

(check-expect (search 'both "cat" "barks"
                                    (list (list "barks" (list "b.txt"))
                                          (list "cat" (list "a.txt" "c.txt"))
                                          (list "chases" (list "c.txt"))
                                          (list "dog" (list "b.txt" "c.txt"))
                                          (list "sleeps" (list "a.txt"))
                                          (list "suddenly" (list "c.txt"))
                                   (list "the" (list "a.txt" "b.txt" "c.txt"))))
              empty)

(check-expect (search 'exclude "cat" "barks"
                                    (list (list "barks" (list "b.txt"))
                                          (list "cat" (list "a.txt" "c.txt"))
                                          (list "chases" (list "c.txt"))
                                          (list "dog" (list "b.txt" "c.txt"))
                                          (list "sleeps" (list "a.txt"))
                                          (list "suddenly" (list "c.txt"))
                                   (list "the" (list "a.txt" "b.txt" "c.txt"))))
              (list "a.txt" "c.txt"))


(define (search both-or-exclude str1 str2 an-IL)
  (cond [(symbol=? both-or-exclude 'both)
         (both (search-single-string str1 an-IL)
               (search-single-string str2 an-IL))]
        [(symbol=? both-or-exclude 'exclude)
         (exclude (search-single-string str1 an-IL)
                  (search-single-string str2 an-IL))]
        [else "First argument must be 'both or 'exclude"]))


(check-expect (search 'exclude "barks" "barks"
                                    (list (list "barks" (list "b.txt"))
                                          (list "cat" (list "a.txt" "c.txt"))
                                          (list "chases" (list "c.txt"))
                                          (list "dog" (list "b.txt" "c.txt"))
                                          (list "sleeps" (list "a.txt"))
                                          (list "suddenly" (list "c.txt"))
                                   (list "the" (list "a.txt" "b.txt" "c.txt"))))
              empty)

(check-expect (search 'exclude "the" "chases"
                                    (list (list "barks" (list "b.txt"))
                                          (list "cat" (list "a.txt" "c.txt"))
                                          (list "chases" (list "c.txt"))
                                          (list "dog" (list "b.txt" "c.txt"))
                                          (list "sleeps" (list "a.txt"))
                                          (list "suddenly" (list "c.txt"))
                                   (list "the" (list "a.txt" "b.txt" "c.txt"))))
              (list "a.txt" "b.txt"))

(check-expect (search 'both "the" "chases"
                                    (list (list "barks" (list "b.txt"))
                                          (list "cat" (list "a.txt" "c.txt"))
                                          (list "chases" (list "c.txt"))
                                          (list "dog" (list "b.txt" "c.txt"))
                                          (list "sleeps" (list "a.txt"))
                                          (list "suddenly" (list "c.txt"))
                                   (list "the" (list "a.txt" "b.txt" "c.txt"))))
              (list "c.txt"))

(check-expect (search 'both "dog" "the"
                                    (list (list "barks" (list "b.txt"))
                                          (list "cat" (list "a.txt" "c.txt"))
                                          (list "chases" (list "c.txt"))
                                          (list "dog" (list "b.txt" "c.txt"))
                                          (list "sleeps" (list "a.txt"))
                                          (list "suddenly" (list "c.txt"))
                                   (list "the" (list "a.txt" "b.txt" "c.txt"))))
              (list "b.txt" "c.txt"))

(check-expect (search 'exclude "dog" "the"
                                    (list (list "barks" (list "b.txt"))
                                          (list "cat" (list "a.txt" "c.txt"))
                                          (list "chases" (list "c.txt"))
                                          (list "dog" (list "b.txt" "c.txt"))
                                          (list "sleeps" (list "a.txt"))
                                          (list "suddenly" (list "c.txt"))
                                   (list "the" (list "a.txt" "b.txt" "c.txt"))))
              empty)

(check-expect (search 'exclude "the" "dog"
                                    (list (list "barks" (list "b.txt"))
                                          (list "cat" (list "a.txt" "c.txt"))
                                          (list "chases" (list "c.txt"))
                                          (list "dog" (list "b.txt" "c.txt"))
                                          (list "sleeps" (list "a.txt"))
                                          (list "suddenly" (list "c.txt"))
                                   (list "the" (list "a.txt" "b.txt" "c.txt"))))
              (list "a.txt"))

(check-expect (search 'both "cat" "cat"
                                    (list (list "barks" (list "b.txt"))
                                          (list "cat" (list "a.txt" "c.txt"))
                                          (list "chases" (list "c.txt"))
                                          (list "dog" (list "b.txt" "c.txt"))
                                          (list "sleeps" (list "a.txt"))
                                          (list "suddenly" (list "c.txt"))
                                   (list "the" (list "a.txt" "b.txt" "c.txt"))))
              (list "a.txt" "c.txt"))

(check-expect (search 'lol "barks" "barks"
                                    (list (list "barks" (list "b.txt"))
                                          (list "cat" (list "a.txt" "c.txt"))
                                          (list "chases" (list "c.txt"))
                                          (list "dog" (list "b.txt" "c.txt"))
                                          (list "sleeps" (list "a.txt"))
                                          (list "suddenly" (list "c.txt"))
                                   (list "the" (list "a.txt" "b.txt" "c.txt"))))
              "First argument must be 'both or 'exclude")

(check-expect (search 'exclude "barks" "chases"
                                    (list (list "barks" (list "b.txt"))
                                          (list "cat" (list "a.txt" "c.txt"))
                                          (list "chases" (list "c.txt"))
                                          (list "dog" (list "b.txt" "c.txt"))
                                          (list "sleeps" (list "a.txt"))
                                          (list "suddenly" (list "c.txt"))
                                   (list "the" (list "a.txt" "b.txt" "c.txt"))))
              (list "b.txt"))

(check-expect (search 'exclude "cat" "chases"
                                    (list (list "barks" (list "b.txt"))
                                          (list "cat" (list "a.txt" "c.txt"))
                                          (list "chases" (list "c.txt"))
                                          (list "dog" (list "b.txt" "c.txt"))
                                          (list "sleeps" (list "a.txt"))
                                          (list "suddenly" (list "c.txt"))
                                   (list "the" (list "a.txt" "b.txt" "c.txt"))))
              (list "a.txt"))

(check-expect (search 'both "cat" "chases"
                                    (list (list "barks" (list "b.txt"))
                                          (list "cat" (list "a.txt" "c.txt"))
                                          (list "chases" (list "c.txt"))
                                          (list "dog" (list "b.txt" "c.txt"))
                                          (list "sleeps" (list "a.txt"))
                                          (list "suddenly" (list "c.txt"))
                                   (list "the" (list "a.txt" "b.txt" "c.txt"))))
              (list "c.txt"))

(check-expect (search 'both "" "chases"
                                    (list (list "barks" (list "b.txt"))
                                          (list "cat" (list "a.txt" "c.txt"))
                                          (list "chases" (list "c.txt"))
                                          (list "dog" (list "b.txt" "c.txt"))
                                          (list "sleeps" (list "a.txt"))
                                          (list "suddenly" (list "c.txt"))
                                   (list "the" (list "a.txt" "b.txt" "c.txt"))))
              empty)

(check-expect (search 'both "chases" ""
                                    (list (list "barks" (list "b.txt"))
                                          (list "cat" (list "a.txt" "c.txt"))
                                          (list "chases" (list "c.txt"))
                                          (list "dog" (list "b.txt" "c.txt"))
                                          (list "sleeps" (list "a.txt"))
                                          (list "suddenly" (list "c.txt"))
                                   (list "the" (list "a.txt" "b.txt" "c.txt"))))
              empty)

(check-expect (search 'exclude "chases" ""
                                    (list (list "barks" (list "b.txt"))
                                          (list "cat" (list "a.txt" "c.txt"))
                                          (list "chases" (list "c.txt"))
                                          (list "dog" (list "b.txt" "c.txt"))
                                          (list "sleeps" (list "a.txt"))
                                          (list "suddenly" (list "c.txt"))
                                   (list "the" (list "a.txt" "b.txt" "c.txt"))))
              (list "c.txt"))

(check-expect (search 'exclude "" "chases"
                                    (list (list "barks" (list "b.txt"))
                                          (list "cat" (list "a.txt" "c.txt"))
                                          (list "chases" (list "c.txt"))
                                          (list "dog" (list "b.txt" "c.txt"))
                                          (list "sleeps" (list "a.txt"))
                                          (list "suddenly" (list "c.txt"))
                                   (list "the" (list "a.txt" "b.txt" "c.txt"))))
              empty)

(check-expect (search 'exclude "" ""
                                    (list (list "barks" (list "b.txt"))
                                          (list "cat" (list "a.txt" "c.txt"))
                                          (list "chases" (list "c.txt"))
                                          (list "dog" (list "b.txt" "c.txt"))
                                          (list "sleeps" (list "a.txt"))
                                          (list "suddenly" (list "c.txt"))
                                   (list "the" (list "a.txt" "b.txt" "c.txt"))))
              empty)

(check-expect (search 'both "lklk" "lolol"
                                    (list (list "barks" (list "b.txt"))
                                          (list "cat" (list "a.txt" "c.txt"))
                                          (list "chases" (list "c.txt"))
                                          (list "dog" (list "b.txt" "c.txt"))
                                          (list "sleeps" (list "a.txt"))
                                          (list "suddenly" (list "c.txt"))
                                   (list "the" (list "a.txt" "b.txt" "c.txt"))))
              empty)

(check-expect (search 'both "blah" "barks"
                                    (list (list "barks" (list "b.txt"))
                                          (list "cat" (list "a.txt" "c.txt"))
                                          (list "chases" (list "c.txt"))
                                          (list "dog" (list "b.txt" "c.txt"))
                                          (list "sleeps" (list "a.txt"))
                                          (list "suddenly" (list "c.txt"))
                                   (list "the" (list "a.txt" "b.txt" "c.txt"))))
              empty)

(check-expect (search 'both "barks" "blha"
                                    (list (list "barks" (list "b.txt"))
                                          (list "cat" (list "a.txt" "c.txt"))
                                          (list "chases" (list "c.txt"))
                                          (list "dog" (list "b.txt" "c.txt"))
                                          (list "sleeps" (list "a.txt"))
                                          (list "suddenly" (list "c.txt"))
                                   (list "the" (list "a.txt" "b.txt" "c.txt"))))
              empty)

(check-expect (search 'exclude "blah" "barks"
                                    (list (list "barks" (list "b.txt"))
                                          (list "cat" (list "a.txt" "c.txt"))
                                          (list "chases" (list "c.txt"))
                                          (list "dog" (list "b.txt" "c.txt"))
                                          (list "sleeps" (list "a.txt"))
                                          (list "suddenly" (list "c.txt"))
                                   (list "the" (list "a.txt" "b.txt" "c.txt"))))
              empty)

(check-expect (search 'exclude "barks" "hln"
                                    (list (list "barks" (list "b.txt"))
                                          (list "cat" (list "a.txt" "c.txt"))
                                          (list "chases" (list "c.txt"))
                                          (list "dog" (list "b.txt" "c.txt"))
                                          (list "sleeps" (list "a.txt"))
                                          (list "suddenly" (list "c.txt"))
                                   (list "the" (list "a.txt" "b.txt" "c.txt"))))
              (list "b.txt"))
        
         

