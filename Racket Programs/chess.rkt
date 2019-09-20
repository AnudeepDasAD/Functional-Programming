;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname chess) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
;;
;;************************************
;;   Anudeep Das (20782887)
;;   CS 135 Fall 2018
;;   Assignment 03, Problem 2                                    
;;*************************************


(define-struct square (row column))
;; A Square is a (make-square Nat Sym)
;; requires: 1 <= row <= 8
;; column: (anyof 'a 'b 'c 'd 'e 'f 'g 'h)





;;PART A

(define-struct piece (pos unit))
;; A Piece is a (make-piece (Square Sym))
;; requires: pos must be a Square structure                              
;;           unit must be (anyof 'knight 'rook 'bishop 'queen)






;;PART B

;;The following is a template for the Piece structure

;;my-piece-fn: Piece -> Any
;;requires: (piece-pos mypiece) must be a Square
;;          (piece-unit mypiece) is (anyof 'knight 'rook 'bishop 'queen)     
(define (my-piece-fn mypiece)
  (...(piece-pos mypiece)...
      (piece-unit mypiece)...)
  )
     





;;PART C

(define a-columns-number 1)
(define b-columns-number 2)
(define c-columns-number 3)
(define d-columns-number 4)
(define e-columns-number 5)
(define f-columns-number 6)
(define g-columns-number 7)
(define h-columns-number 8)
(define error-case-column-number 100)

;;The following is a helper function
;;(column-to-number-converter-piece mysquare)converts the letter coordinate for
;;   a Square's column into a number, and returns that number, as defined above

;;column-to-number-converter-piece: Square -> (anyof 1 2 3 4 5 6 7 8)
;;      requires: (square-column mysquare) is (anyof 'a 'b 'c 'd 'e 'f 'g 'h)
;;                mysquare is a Square struct
 

(check-expect (column-to-number-converter (make-square 1 'a)) 1)

(define (column-to-number-converter mysquare)
  (cond [(symbol=? (square-column mysquare) 'a) a-columns-number]
        [(symbol=? (square-column mysquare) 'b) b-columns-number]
        [(symbol=? (square-column mysquare) 'c) c-columns-number]
        [(symbol=? (square-column mysquare) 'd) d-columns-number]
        [(symbol=? (square-column mysquare) 'e) e-columns-number]
        [(symbol=? (square-column mysquare) 'f) f-columns-number]
        [(symbol=? (square-column mysquare) 'g) g-columns-number]
        [(symbol=? (square-column mysquare) 'h) h-columns-number]
        [else error-case-column-number]
   )
  )



;;(rook-movment-valid mypiece desired-location)returns true if a rook
;;    can move to the desired location and false otherwise. A move is
;;    permissible if the desired location is on the same row or column

;; rook-movement-valid?: Piece Square -> Bool
;; requires:               (piece-unit mypiece) must be 'rook
;;                         desired-location must be a Square

(check-expect (valid-move? (make-piece (make-square 5 'f) 'rook)
                           (make-square 8 'f))true)

(define (rook-movement-valid? mypiece desired-location)
  (or (= (square-row (piece-pos mypiece))(square-row desired-location))
              (= (column-to-number-converter (piece-pos mypiece))
                 (column-to-number-converter desired-location))
          )
  )


;;(knight-movment-valid mypiece desired-location)returns true if a knight
;;    can move to the desired location and false otherwise. The following
;;    are permissible: (right 1 or left 1 and up 2 or down 2)
;;    or (right 2 or left 2 and up 1 or down 1)

;; knight-movement-valid?: Piece Square -> Bool
;; requires:               (piece-unit mypiece) must be 'knight
;;                         desired-location must be a Square

(check-expect (valid-move? (make-piece (make-square 3 'h) 'knight)
                           (make-square 2 'f))true)

(define (knight-movement-valid? mypiece desired-location)
  (or
           ;right(1)
           (and(or (=(column-to-number-converter desired-location) 
                      (+ (column-to-number-converter (piece-pos mypiece))
                         movement-of-1-space))
          ;or left(1)
               (= (column-to-number-converter desired-location)  
                 (- (column-to-number-converter (piece-pos mypiece))
                    movement-of-1-space)))              
          ; and up(2) or down(2)
               (or(=(square-row desired-location)
                    (+(square-row (piece-pos mypiece)) movement-of-2-spaces))
                  (=(square-row desired-location)
                    (-(square-row (piece-pos mypiece)) movement-of-2-spaces))
                  )
               )
           ;OR
           
           ;right(2)
           (and(or (=(column-to-number-converter desired-location) 
                      (+ (column-to-number-converter (piece-pos mypiece))
                         movement-of-2-spaces))
          ; or left(2)
               (= (column-to-number-converter desired-location)  
                 (- (column-to-number-converter (piece-pos mypiece))
                    movement-of-2-spaces)))   
          ; and up(1) or down(1)
           (or(=(square-row desired-location)
                    (+(square-row (piece-pos mypiece)) movement-of-1-space))
                  (=(square-row desired-location)
                    (-(square-row (piece-pos mypiece)) movement-of-1-space))
                  )
               )
           )
  )

;;   (bishop-movement-valid? mypiece desired-location)
;;   returns true if a bishop can move to the desired location
;;   and false otherwise.Difference between current and desired column
;;   is same as difference between current and desired row for bishop

;; bishop-movement-valid?: Piece Square -> Bool
;; requires:               (piece-unit mypiece) must be 'bishop
;;                         desired-location must be a Square

(check-expect (bishop-movement-valid? (make-piece (make-square 4 'e) 'bishop)
                           (make-square 2 'g))true)

(define (bishop-movement-valid? mypiece desired-location)
  (= (abs(- (column-to-number-converter desired-location)
                         (column-to-number-converter (piece-pos mypiece))))
             (abs (- (square-row desired-location)
                         (square-row (piece-pos mypiece))))
           )
  )



(define movement-of-1-space 1)
(define movement-of-2-spaces 2)
(define top-row-of-board-coordinate 8)
(define bottom-row-of-board-coordinate 1)


;;(valid-move? piece square) shall determine if the location
;;    on the board (denoted by the second parameter) is a valid move for
;;    the piece (denoted by the first parameter) to make.
;;    Making no move is also a valid move.

;;valid-move?: Piece Square -> Bool
;;requires:    mypiece must be a Piece struct
;;             desired-location must be a Square struct

(check-expect (valid-move? (make-piece (make-square 4 'e) 'knight)
                           (make-square 2 'f))true)
(check-expect (valid-move? (make-piece (make-square 4 'e) 'knight)
                           (make-square 3 'g))true)

(define (valid-move? mypiece desired-location)
  (cond

          ;no movement is permissible
    [(and (= (square-row desired-location) (square-row (piece-pos mypiece)))
          (=(column-to-number-converter desired-location) 
              (column-to-number-converter (piece-pos mypiece))))
         true] 

    
          ;if row is off-board: false. This is here because Racket does not
          ;   enforce contract
    [(or  (>(square-row desired-location) top-row-of-board-coordinate) 
          (<(square-row desired-location) bottom-row-of-board-coordinate)
          (=(column-to-number-converter desired-location)
            error-case-column-number) ;error case 
           )
         false]


    ;as long as the rook stays in it's original column, move is allowed
    [(and (symbol=? (piece-unit mypiece) 'rook)
          (rook-movement-valid? mypiece desired-location))
      true]


    ;knight valid moves
    [(and (symbol=? (piece-unit mypiece) 'knight)
          (knight-movement-valid? mypiece desired-location))
     true]
 
    
    [(and (symbol=? (piece-unit mypiece) 'bishop) 
          (bishop-movement-valid? mypiece desired-location))
     true]


    ;queen can move the same way as rook and bishop
    [ (and(symbol=? (piece-unit mypiece) 'queen)
          (or (bishop-movement-valid? mypiece desired-location)
              (rook-movement-valid? mypiece desired-location)
          )
      )
     true]

    [else false]
    )
  )



(check-expect (valid-move? (make-piece (make-square 5 'f) 'rook)
                           (make-square 1 'f))true)
(check-expect (valid-move? (make-piece (make-square 5 'f) 'rook)
                           (make-square 8 'f))true)
(check-expect (valid-move? (make-piece (make-square 8 'a) 'rook)
                           (make-square 8 'f))true)
(check-expect (valid-move? (make-piece (make-square 7 'e) 'rook)
                           (make-square 7 'b))true)
(check-expect (valid-move? (make-piece (make-square 1 'a) 'rook)
                           (make-square 2 'f))false)
(check-expect (valid-move? (make-piece (make-square 1 'a) 'rook)
                           (make-square 1 'a))true)
(check-expect (valid-move? (make-piece (make-square 4 'e) 'rook)
                           (make-square 3 'f))false)
(check-expect (valid-move? (make-piece (make-square 1 'a) 'rook)
                           (make-square 0 'f))false)
(check-expect (valid-move? (make-piece (make-square 1 'a) 'rook)
                           (make-square 1 'z))false)

(check-expect (valid-move? (make-piece (make-square 5 'f) 'queen)
                           (make-square 1 'f))true)
(check-expect (valid-move? (make-piece (make-square 5 'f) 'queen)
                           (make-square 8 'f))true)
(check-expect (valid-move? (make-piece (make-square 8 'a) 'queen)
                           (make-square 8 'f))true)
(check-expect (valid-move? (make-piece (make-square 7 'e) 'queen)
                           (make-square 7 'b))true)
(check-expect (valid-move? (make-piece (make-square 3 'h) 'queen)
                           (make-square 8 'c))true)
(check-expect (valid-move? (make-piece (make-square 1 'a) 'queen)
                           (make-square 0 'f))false)
(check-expect (valid-move? (make-piece (make-square 1 'a) 'queen)
                           (make-square 1 'z))false)
(check-expect (valid-move? (make-piece (make-square 4 'e) 'queen)
                           (make-square 7 'h))true)
(check-expect (valid-move? (make-piece (make-square 4 'e) 'queen)
                           (make-square 8 'a))true)
(check-expect (valid-move? (make-piece (make-square 4 'e) 'queen)
                           (make-square 1 'b))true)
(check-expect (valid-move? (make-piece (make-square 4 'e) 'queen)
                           (make-square 2 'g))true)
(check-expect (valid-move? (make-piece (make-square 4 'e) 'queen)
                           (make-square 4 'e))true)

(check-expect (valid-move? (make-piece (make-square 3 'h) 'bishop)
                           (make-square 8 'h))false)
(check-expect (valid-move? (make-piece (make-square 4 'e) 'bishop)
                           (make-square 7 'h))true)
(check-expect (valid-move? (make-piece (make-square 4 'e) 'bishop)
                           (make-square 8 'a))true)
(check-expect (valid-move? (make-piece (make-square 4 'e) 'bishop)
                           (make-square 1 'b))true)
(check-expect (valid-move? (make-piece (make-square 4 'e) 'bishop)
                           (make-square 2 'g))true)
(check-expect (valid-move? (make-piece (make-square 8 'a) 'bishop)
                           (make-square 8 'f))false)
(check-expect (valid-move? (make-piece (make-square 1 'a) 'bishop)
                           (make-square 1 'f))false)
(check-expect (valid-move? (make-piece (make-square 1 'a) 'bishop)
                           (make-square 1 'a))true)
(check-expect (valid-move? (make-piece (make-square 1 'a) 'bishop)
                           (make-square 1 'z))false)
(check-expect (valid-move? (make-piece (make-square 1 'a) 'bishop)
                           (make-square 0 'a))false)

(check-expect (valid-move? (make-piece (make-square 3 'h) 'knight)
                           (make-square 2 'f))true)
(check-expect (valid-move? (make-piece (make-square 1 'b) 'knight)
                           (make-square 2 'a))false)
(check-expect (valid-move? (make-piece (make-square 4 'e) 'knight)
                           (make-square 4 'e))true)
(check-expect (valid-move? (make-piece (make-square 4 'e) 'knight)
                           (make-square 6 'd))true)
(check-expect (valid-move? (make-piece (make-square 4 'e) 'knight)
                           (make-square 6 'f))true)
(check-expect (valid-move? (make-piece (make-square 4 'e) 'knight)
                           (make-square 5 'c))true)
(check-expect (valid-move? (make-piece (make-square 4 'e) 'knight)
                           (make-square 5 'g))true)
(check-expect (valid-move? (make-piece (make-square 4 'e) 'knight)
                           (make-square 3 'c))true)
(check-expect (valid-move? (make-piece (make-square 4 'e) 'knight)
                           (make-square 3 'g))true)
(check-expect (valid-move? (make-piece (make-square 4 'e) 'knight)
                           (make-square 2 'd))true)
(check-expect (valid-move? (make-piece (make-square 4 'e) 'knight)
                           (make-square 2 'f))true)
(check-expect (valid-move? (make-piece (make-square 8 'h) 'knight)
                           (make-square 7 'f))true)
(check-expect (valid-move? (make-piece (make-square 8 'h) 'knight)
                           (make-square 6 'g))true)
(check-expect (valid-move? (make-piece (make-square 8 'h) 'knight)
                           (make-square 6 'h))false)
(check-expect (valid-move? (make-piece (make-square 8 'h) 'knight)
                           (make-square 10 'g))false)

;; PART D


;;The following is a helper function
;; (number-to-column-converter mysquare) takes in a number that represents
;;     a Square's column,and outputs it's Sym equivalent,
;;     as dictated in column-to-number-converter. Essentially it is the
;;     opposite of the column-to-number-converter

;; number-to-column-converter: Nat -> Sym (anyof 'b 'c 'd 'e 'f 'g 'h)
;; requires: column is (anyof 1 2 3 4 5 6 7 or 8)
 

  (define (number-to-column-converter column)
    (cond [(= column b-columns-number) 'b]
          [(= column c-columns-number) 'c]
          [(= column d-columns-number) 'd]
          [(= column e-columns-number) 'e]
          [(= column f-columns-number) 'f]
          [(= column g-columns-number) 'g]
          [(= column h-columns-number) 'h]
          )
    )


(define second-row-from-bottom-coordinate 2)
(define second-row-from-top-coordinate 7)
(define third-row-from-bottom-coordinate 3)

;; (knight-no-movement-condition mypiece) returns true if the knight
;;    in question will not move; if it moves it will go further away from h1,
;;    thus it cannot.
;; knight-no-movement-condition: Square -> Bool
;; requires: (piece-unit mypiece) must be 'knight
;;           (square-column (piece-pos mypiece)) must be 'g or 'h
;;           (square-row (piece-pos mypiece))  must be 1 or 2

(check-expect(knight-no-movement-condition
             (make-piece (make-square 1 'g) 'knight))true)

(define (knight-no-movement-condition mypiece)
  (and
           (and(>= (column-to-number-converter (piece-pos mypiece))
                   g-columns-number)
               (<= (column-to-number-converter (piece-pos mypiece))
                   h-columns-number)
           )
           (and (>= (square-row (piece-pos mypiece))
                    bottom-row-of-board-coordinate)
                (<= (square-row (piece-pos mypiece))
                    second-row-from-bottom-coordinate)
           )
   )
  )


;; (knight-movement-if-between-row-3-and-8 mypiece) returns true if the knight
;;    is between row 3 and row 8 and is between column a and g

;; knight-movement-if-between-row-3-and-8: Square -> Bool
;; requires: (piece-unit mypiece) must be 'knight
;;       (square-column (piece-pos mypiece)) is (anyof 'a 'b 'c 'd 'e 'f or 'g)
;;       (square-row (piece-pos mypiece))  is (anyof 3 4 5 6 7 8)

(check-expect(knight-movement-if-between-row-3-and-8-and-column-a-and-g
              (make-piece (make-square 8 'g) 'knight))
              true)

(define (knight-movement-if-between-row-3-and-8-and-column-a-and-g mypiece)
  (and (and (>= (square-row(piece-pos mypiece))
                third-row-from-bottom-coordinate)
                    (<= (square-row(piece-pos mypiece))
                        top-row-of-board-coordinate)
               )
               (and (>= (column-to-number-converter (piece-pos mypiece))
                        a-columns-number)
                    (<= (column-to-number-converter (piece-pos mypiece))
                        g-columns-number)
               )
            )
)




;; (knight-next-move mypiece) determines the square to which the piece must move
;;   It moves as close to H1 as possible
;;knight-next-move: Piece -> Square
  ;; requires: (piece-unit mypiece) must be 'knight

(check-expect(knight-next-move (make-piece (make-square 8 'd) 'knight))
             (make-square 6 'e))
(check-expect(knight-next-move (make-piece (make-square 1 'h) 'knight))
             (make-square 1 'h))

 (define (knight-next-move mypiece)
   (cond
         [(not(symbol=? (piece-unit mypiece) 'knight))
          "Sorry, the piece must be a knight"]

         
         ;if the position is within some distance of h1, it doesn't move
         [(knight-no-movement-condition mypiece)
          
          (make-square (square-row (piece-pos mypiece))
                       (square-column (piece-pos mypiece)))]


         
         ;under most circumstances, bottom left square is closest
         [(knight-movement-if-between-row-3-and-8-and-column-a-and-g mypiece)
          
          (make-square (- (square-row(piece-pos mypiece)) movement-of-2-spaces)
                       (number-to-column-converter
                        (+ (column-to-number-converter (piece-pos mypiece))
                           movement-of-1-space))
          )]


         
         ;returns the furthest right square if at complete bottom row
         [(= (square-row (piece-pos mypiece)) bottom-row-of-board-coordinate)

          (make-square (+ (square-row (piece-pos mypiece)) movement-of-1-space)
                       (number-to-column-converter
                        (+(column-to-number-converter (piece-pos mypiece))
                          movement-of-2-spaces))
                       )]


         ;returns the furthest right bottom square if at second row from bottom 
         [(= (square-row (piece-pos mypiece)) second-row-from-bottom-coordinate)

          (make-square (- (square-row (piece-pos mypiece)) movement-of-1-space)
                       (number-to-column-converter
                        (+(column-to-number-converter (piece-pos mypiece))
                          movement-of-2-spaces))
                       )]


         ;returns the bottom left square if the piece is far right
         [(= (column-to-number-converter (piece-pos mypiece))
                   h-columns-number)
          
          (make-square (-(square-row (piece-pos mypiece)) movement-of-2-spaces)
                       (number-to-column-converter
                          (-(column-to-number-converter (piece-pos mypiece))
                            movement-of-1-space))
                       )]

         [else "Sorry, position is invalid"]

         )
   )
                
(check-expect (knight-next-move (make-piece (make-square 4 'e) 'knight))
              (make-square 2 'f))
(check-expect(knight-next-move (make-piece (make-square 2 'g) 'knight))
             (make-square 2 'g))
(check-expect(knight-next-move (make-piece (make-square 8 'h) 'knight))
             (make-square 6 'g))
(check-expect(knight-next-move (make-piece (make-square 1 'e) 'knight))
             (make-square 2 'g))
(check-expect(knight-next-move (make-piece (make-square 2 'e) 'knight))
             (make-square 1 'g))
(check-expect(knight-next-move (make-piece (make-square 2 'a) 'knight))
             (make-square 1 'c))
(check-expect(knight-next-move (make-piece (make-square 1 'a) 'knight))
             (make-square 2 'c))
(check-expect(knight-next-move (make-piece (make-square 5 'h) 'knight))
             (make-square 3 'g))
(check-expect(knight-next-move (make-piece (make-square 8 'g) 'knight))
             (make-square 6 'h))
(check-expect(knight-next-move (make-piece (make-square 4 'a) 'knight))
             (make-square 2 'b))
(check-expect(knight-next-move (make-piece (make-square 6 'c) 'knight))
             (make-square 4 'd))
(check-expect(knight-next-move (make-piece (make-square 6 'c) 'bishop))
             "Sorry, the piece must be a knight")
(check-expect(knight-next-move (make-piece (make-square 1 'h) 'knight))
             (make-square 1 'h))
(check-expect(knight-next-move (make-piece (make-square 1 'g) 'knight))
             (make-square 1 'g))
(check-expect(knight-next-move (make-piece (make-square 2 'g) 'knight))
             (make-square 2 'g))
(check-expect(knight-next-move (make-piece (make-square 2 'h) 'knight))
             (make-square 2 'h))
(check-expect(knight-next-move (make-piece (make-square 6 'b) 'knight))
             (make-square 4 'c))
(check-expect(knight-next-move (make-piece (make-square 5 'a) 'knight))
             (make-square 3 'b))
(check-expect(knight-next-move (make-piece (make-square 3 'g) 'knight))
             (make-square 1 'h))
(check-expect(knight-next-move (make-piece (make-square 8 'a) 'knight))
             (make-square 6 'b))
(check-expect(knight-next-move (make-piece (make-square 3 'h) 'knight))
             (make-square 1 'g))
(check-expect(knight-next-move (make-piece (make-square 0 'b) 'knight))
             "Sorry, position is invalid")
(check-expect(knight-next-move (make-piece (make-square 4 'i) 'knight))
             "Sorry, position is invalid")
(check-expect(knight-next-move (make-piece (make-square 2 'd) 'knight))
             (make-square 1 'f))
(check-expect(knight-next-move (make-piece (make-square 1 'd) 'knight))
             (make-square 2 'f))
(check-expect(knight-next-move (make-piece (make-square 2 'f) 'knight))
             (make-square 1 'h))
           






         
                
 
          
         
          
      
        
             
        


