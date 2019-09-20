;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname rainbow) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
;;
;;************************************
;;   Anudeep Das (20782887)
;;   CS 135 Fall 2018
;;   Assignment 04, Problem 4                                    
;;*************************************


;;PART A

;; The following is a helper function
(define number-for-red 1)
(define number-for-orange 2)
(define number-for-yellow 3)
(define number-for-green 4)
(define number-for-blue 5)
(define number-for-indigo 6)
(define number-for-violet 7)
(define error-case-value 100)

;;(colour-to-number-converter colour) converts each colour to it's corresponding
;;     number based on the pre-defined constants above

;;colour-to-number-converter: Sym -> Nat
;; requires: colour is (anyof 'red 'orange 'yellow 'green 'blue 'indigo 'violet)

(check-expect (colour-to-number-converter 'orange) 2)

(define (colour-to-number-converter colour)
  (cond [(not(symbol? colour)) error-case-value]
        [(symbol=? colour 'red) number-for-red]
        [(symbol=? colour 'orange) number-for-orange]  
        [(symbol=? colour 'yellow) number-for-yellow]
        [(symbol=? colour 'green) number-for-green]
        [(symbol=? colour 'blue) number-for-blue]
        [(symbol=? colour 'indigo) number-for-indigo]
        [(symbol=? colour 'violet) number-for-violet]
        [else error-case-value]
        )
  )




;; This is another helper function
;;(colour-list-to-number-list list-of-colours) converts a list of colours into
;;   a list of numbers (representing the colours) using the above function

;;(colour-list-to-number-list: listof Sym -> Listof Nat
;;    requires: list-of-colours must be one of:
;;                                    * empty
;;                                    * (cons colour (listof colour))
;;        a colour is (anyof 'red 'orange 'yellow 'green 'blue 'indigo 'violet) 

(check-expect(colour-list-to-number-list
              (cons 'red (cons 'red (cons 'blue empty))))
             (cons 1 (cons 1 (cons 5 empty))))

 (define (colour-list-to-number-list list-of-colours)
   (cond [(empty? list-of-colours) empty]
         [else (cons (colour-to-number-converter (first list-of-colours))
                     (colour-list-to-number-list(rest list-of-colours)))]
         )
   )




;; This is another helper function.
;;(rainbow?-with-numbers list-of-colours-num) is a predicate that
;;    determines if a given rainbow,
;;    with all colours converted to numbers, is a valid rainbow based on the
;;    pre-determined conditions given in the question, returning true if it is
;;    valid and false if it not.It does the job of the main program "rainbow?"
;;    but with a listofnat representing the rainbow list
;;    

;; rainbow?-with-numbers: listof Nat -> Bool
;;    requires: list-of-colours-nat must be one of:
;;                                    * empty
;;                                    * (cons Nat (listof Nat))
;;        The Nat is (anyof 1 2 3 4 5 6 or 7) 

(check-expect (rainbow-with-numbers? (cons 1 (cons 1 (cons 5 empty)))) false)

 (define (rainbow-with-numbers? list-of-colours-nat)
   (cond [(empty? list-of-colours-nat) true]
         [(= (first list-of-colours-nat) error-case-value) false]

         ;the number before empty would have had to have been a smaller number
         ;than the number before, thus if it isn't an error case, it is valid
         [(empty? (rest list-of-colours-nat)) true]
         [(>= (first list-of-colours-nat)
                   (first(rest list-of-colours-nat))) false]
         [else (rainbow-with-numbers? (rest list-of-colours-nat))]
         )
   )



;;(rainbow? list-of-colours) determines if a list is a valid rainbow, according
;;    to the pre-determined conditions in the question.

;;rainbow?: listof Sym -> Bool
;; requires: list-of-colours must be one of:
;;                                    * empty
;;                                    * (cons colour (listof colour))
;;        a colour is (anyof 'red 'orange 'yellow 'green 'blue 'indigo 'violet)

(check-expect(rainbow? (cons 'red (cons 'yellow (cons 'indigo empty))))true)
(check-expect(rainbow? (cons 'red (cons 'yellow (cons 'violet empty))))true)


(define (rainbow? list-of-colours)
  (rainbow-with-numbers? (colour-list-to-number-list list-of-colours)))


(check-expect(rainbow? (cons 'red (cons 'green (cons 'indigo empty))))true)
(check-expect(rainbow? (cons 'pineapple (cons 'pizza (cons pi empty))))false)
(check-expect(rainbow? (cons 'red (cons 'yellow (cons pi empty))))false)

(check-expect(rainbow? (cons 'red (cons 'red (cons 'blue empty))))false)
(check-expect(rainbow? (cons 'red (cons 'bleu (cons 'blue empty))))false)
(check-expect(rainbow? (cons 'red (cons 'blue (cons 'blue empty))))false)
(check-expect(rainbow? (cons 'yellow (cons 'blue (cons 'indigo empty))))true)
(check-expect(rainbow? (cons 'red (cons 'yellow (cons 'orange empty))))false)
(check-expect(rainbow? (cons 'red (cons 'violet (cons 'green empty))))false)
(check-expect(rainbow? (cons 'red (cons 'orange (cons 'yellow empty))))true)

(check-expect (rainbow? empty) true)
(check-expect(rainbow? (cons 'blue empty))true)






;;PART B

;; (unicorn colour rainbow) produces a rainbow with a given colour removed
;; unicorn: Sym listof Sym -> Listof Sym
;;    requires: colour is
;;             (anyof 'red 'orange 'yellow 'green 'blue 'indigo 'violet)
;;              rainbow is a valid rainbow, ie. (rainbow? rainbow) is true

(check-expect(unicorn 'blue (cons 'red (cons 'yellow (cons 'blue empty))))
             (cons 'red (cons 'yellow empty)))
(check-expect(unicorn 'green (cons 'red (cons 'yellow (cons 'blue empty))))
             (cons 'red (cons 'yellow (cons 'blue empty))))

(define (unicorn colour rainbow)
  (cond [(empty? rainbow) empty]
        [(symbol=? (first rainbow) colour)
         (unicorn colour (rest rainbow))] ;skips "colour"
        [else (cons (first rainbow)(unicorn colour (rest rainbow)))]
        )
  )


(check-expect(unicorn 'blue empty) empty)
(check-expect(unicorn 'blue (cons 'blue empty)) empty)
(check-expect(unicorn 'blue (cons 'red empty)) (cons 'red empty))
(check-expect(unicorn 'red (cons 'blue (cons 'indigo empty)))
             (cons 'blue (cons 'indigo empty)))






;;PART C


;;(number-to-colour-converter colour-code-nat) returns the colour symbol
;;  that corresponds to a given colour number. It is the reverse of
;;  colour-to-number-converter

;;number-to-colour-converter: Nat -> Sym
;; requires: colour-code-nat is (anyof 1 2 3 4 5 6 7)

(check-expect (number-to-colour-converter 4) 'green)
(check-expect (number-to-colour-converter 6) 'indigo)

(define (number-to-colour-converter colour-code-nat)
  (cond
        [(= colour-code-nat number-for-red) 'red]
        [(= colour-code-nat number-for-orange) 'orange]
        [(= colour-code-nat number-for-yellow) 'yellow]
        [(= colour-code-nat number-for-green) 'green]
        [(= colour-code-nat number-for-blue) 'blue]
        [(= colour-code-nat number-for-indigo) 'indigo]
        [(= colour-code-nat number-for-violet) 'violet]
        )
  )




;;(number-list-to-colour-list list-of-colours-nat) converts a numbered version
;;  of a colour list to a symbol version of the colour list, it is basically
;;  the reverse of colour-list-to-number-list

;;number-list-to-colour-list: listof Nat -> listof Sym
;; ;;    requires: list-of-colours must be one of:
;;                                    * empty
;;                                    * (cons colour (listof colour))
;;        a colour is (anyof 'red 'orange 'yellow 'green 'blue 'indigo 'violet) 

(check-expect (number-list-to-colour-list
               (cons 1 (cons 3 (cons 5  empty))))
              (cons 'red (cons 'yellow (cons 'blue empty))))
              
(define (number-list-to-colour-list list-of-colours-nat)
   (cond [(empty? list-of-colours-nat) empty]
         [else (cons (number-to-colour-converter (first list-of-colours-nat))
                     (number-list-to-colour-list(rest list-of-colours-nat)))]
         )
   )




;;(leprechaun-with-numbers wanted-colours-number list-of-colours-nat)
;;   produces a number version of a rainbow with the specified colour number 
;;   added to it. If the numbered rainbow already contains the specified colour
;;   number, the numbered rainbow is unchanged. It is basically the same as
;;   the leprechaun function except using numbers.

;;leprechaun-with-numbers: Nat listof Nat -> listof Sym
;;requires: wanted-colours-number is (anyof 1 2 3 4 5 6 7)
;;          list-of-colours-nat must be one of:
;;                                    * empty
;;                                    * (cons Nat (listof Nat))
;;          The Nat is (anyof 1 2 3 4 5 6 or 7) 


(check-expect(leprechaun-with-numbers 7 (cons 1 (cons 3 (cons 5 empty))))
(cons 1 (cons 3 (cons 5 (cons 7 empty)))))

(define (leprechaun-with-numbers wanted-colours-number list-of-colours-nat)
  (cond ;if the end of the list has been reached, the wanted colour inserts
        [(empty? list-of-colours-nat) (cons wanted-colours-number empty)]
        
        ;if the colour has a lower number than the (first list)
        ;it inserts itself before (first list) and the iteration ends
        [(< wanted-colours-number (first list-of-colours-nat))
            (cons wanted-colours-number list-of-colours-nat)]
        
        ;if the wanted colour is already there, iteration ends
        [(= wanted-colours-number (first list-of-colours-nat))
            (cons (first list-of-colours-nat) (rest list-of-colours-nat))]
        
        ; whatever is first is added to the new list and rest is analyzed
        [else (cons (first list-of-colours-nat)
                    (leprechaun-with-numbers wanted-colours-number
                                       (rest list-of-colours-nat))
                   )]))




;;(leprechaun colour rainbow) produces a rainbow with the specified colour added
;;  to it. If the rainbow already contains the colour the rainbow is unchanged.

;;leprechaun: Sym listof Sym -> listof Sym
;;requires: colour is (anyof 'red 'orange 'yellow 'green 'blue 'indigo 'violet)
;;          (rainbow? rainbow) is true

(check-expect(leprechaun 'violet (cons 'red (cons 'yellow (cons 'blue empty))))
(cons 'red (cons 'yellow (cons 'blue (cons 'violet empty)))))




(define (leprechaun colour rainbow)
  (number-list-to-colour-list ;makes colour rainbow
   (leprechaun-with-numbers (colour-to-number-converter colour)
                             (colour-list-to-number-list rainbow))))



(check-expect(leprechaun 'orange (cons 'red (cons 'yellow (cons 'blue empty))))
(cons 'red (cons 'orange (cons 'yellow (cons 'blue empty)))))

(check-expect(leprechaun 'red (cons 'orange (cons 'yellow (cons 'blue empty))))
(cons 'red (cons 'orange (cons 'yellow (cons 'blue empty)))))

(check-expect(leprechaun 'green (cons 'green empty)) (cons 'green empty))
(check-expect (leprechaun 'violet empty) (cons 'violet empty))
(check-expect (leprechaun 'red empty) (cons 'red empty))
(check-expect (leprechaun 'green empty) (cons 'green empty))
(check-expect(leprechaun 'orange (cons 'red empty))
             (cons 'red (cons 'orange empty)))

(check-expect(leprechaun 'blue
    (cons 'red (cons 'orange (cons 'blue (cons 'indigo (cons 'violet empty))))))
    (cons 'red (cons 'orange (cons 'blue (cons 'indigo (cons 'violet empty))))))

(check-expect(leprechaun 'red
    (cons 'red (cons 'orange (cons 'blue (cons 'indigo (cons 'violet empty))))))
    (cons 'red (cons 'orange (cons 'blue (cons 'indigo (cons 'violet empty))))))

(check-expect(leprechaun 'violet
    (cons 'red (cons 'orange (cons 'blue (cons 'indigo (cons 'violet empty))))))
    (cons 'red (cons 'orange (cons 'blue (cons 'indigo (cons 'violet empty))))))

(check-expect(leprechaun 'indigo
             (cons 'red (cons 'yellow (cons 'blue (cons 'violet empty)))))
(cons 'red (cons 'yellow (cons 'blue (cons 'indigo (cons 'violet empty))))))

