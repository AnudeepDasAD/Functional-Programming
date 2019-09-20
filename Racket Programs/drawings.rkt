;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname drawings) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
;;
;;************************************
;;   Anudeep Das (20782887)
;;   CS 135 Fall 2018
;;   Assignment 07, Problem 2                                    
;;*************************************



(require "a07drawinglib.rkt")

;; A Coordinate is a (make-posn Int Int)

;; An ImageColor is a Str
;; requires: the Str is from the racket/draw colour database:
;; https://docs.racket-lang.org/draw/color-database___.html


;(define-struct prim-circle (center radius color))
;; A PrimCircle is a (make-prim-circle Coordinate Nat ImageColor)

;(define-struct prim-triangle (p1 p2 p3 color))
;; A PrimTriangle is a (make-prim-triangle)
;; Coordinate Coordinate Coordinate ImageColor)


;; A PrimElement is (anyof PrimTriangle PrimCircle)

;; (render-image image-size prim-shapes) produces an
;; Image on an image of size image-size containing the list of
;; shapes listed by prim-shapes.

;; render-image: Posn (listof PrimElements) -> Image
;; requires: canvas-size is a (make-posn Nat Nat)


(define overlap-prims
  (list
   (make-prim-circle (make-posn 30 30) 4 "White")
   (make-prim-triangle
    (make-posn 10 40) (make-posn 50 40) (make-posn 30 10) "DarkBlue")
   (make-prim-circle (make-posn 30 30) 15 "Chocolate")))


;;PART A

(define canvas-width 300)
(define canvas-height 300)

(define prim-picture
  (list
   ;tear system
   (make-prim-triangle (make-posn 165 150) (make-posn 175 150)
                       (make-posn 170 165) "Cyan")
   (make-prim-triangle (make-posn 125 150) (make-posn 135 150)
                       (make-posn 130 165) "Cyan")

   ;;nose system
   (make-prim-triangle (make-posn 145 160) (make-posn 155 160)
                       (make-posn 150 175) "Green")
   (make-prim-triangle (make-posn 145 160) (make-posn 155 160)
                       (make-posn 150 145) "Green")

   ;teeth system
   (make-prim-circle (make-posn 150 175) 25 "Blue")
   (make-prim-triangle (make-posn 130 190) (make-posn 140 190)
                       (make-posn 135 215) "Red")
   (make-prim-triangle (make-posn 160 190) (make-posn 170 190)
                       (make-posn 165 215) "Red")

   ;smile
   ;(make-prim-circle (make-posn 150 175) 25 "Blue")
   (make-prim-circle (make-posn 150 180) 25 "White")
   ;eyes
   (make-prim-circle (make-posn 130 140) 10 "White")
   (make-prim-circle (make-posn 170 140) 10 "White")
   ;face
   (make-prim-circle (make-posn 150 150) 60 "Blue")
   ;ears
   (make-prim-circle (make-posn 130 110) 27 "Red")
   (make-prim-circle (make-posn 180 110) 20 "Red")
   

   )
  )

(render-image (make-posn canvas-width canvas-height) prim-picture)


;;PART B

;;(rect top-left-vertex width/length color) constructs a rectangle
;;   out of two triangles, one on top of the other. The function consumes a
;;   Coordinate representing the offset of the rectangle from the the top left
;;   corner (0,0), a Coordinate representing the width and height of the
;;   rectangle (both of which should be Nats), and an ImageColor.
;;   The function produces a list of PrimTriangles representing the rectangle.

;;rect: Posn Posn ImageColor -> (listof PrimTriangles)



(check-expect(rect (make-posn 0 290) (make-posn 50 10) "Blue")
(list
 (make-prim-triangle
  (make-posn 0 290)
  (make-posn 50 290)
  (make-posn 50 300)
  "Blue")
 (make-prim-triangle
  (make-posn 0 290)
  (make-posn 50 300)
  (make-posn 0 300)
  "Blue")))

(check-expect (rect (make-posn 0 0) (make-posn 10 10) "Blue")
(list
 (make-prim-triangle
  (make-posn 0 0)
  (make-posn 10 0)
  (make-posn 10 10)
  "Blue")
 (make-prim-triangle
  (make-posn 0 0)
  (make-posn 10 10)
  (make-posn 0 10)
  "Blue")))


(define (rect top-left-vertex width/length colour)
  (list
   (make-prim-triangle (make-posn (posn-x top-left-vertex)
                                  (posn-y top-left-vertex))
                       (make-posn (+ (posn-x top-left-vertex)
                                     (posn-x width/length))
                                  (posn-y top-left-vertex))
                       (make-posn (+ (posn-x top-left-vertex)
                                     (posn-x width/length))
                                  (+ (posn-y top-left-vertex)
                                     (posn-y width/length)))
                       colour)
   
   (make-prim-triangle (make-posn (posn-x top-left-vertex)
                                  (posn-y top-left-vertex))
                       
                       (make-posn (+ (posn-x top-left-vertex)
                                     (posn-x width/length))
                                  (+ (posn-y top-left-vertex)
                                     (posn-y width/length)))
                       
                       (make-posn (posn-x top-left-vertex)
                                  (+ (posn-y top-left-vertex)
                                     (posn-y width/length)))
                       colour)))




(check-expect(rect (make-posn 150 150) (make-posn 70 70) "Blue")
(list
 (make-prim-triangle
  (make-posn 150 150)
  (make-posn 220 150)
  (make-posn 220 220)
  "Blue")
 (make-prim-triangle
  (make-posn 150 150)
  (make-posn 220 220)
  (make-posn 150 220)
  "Blue")))

(check-expect(rect (make-posn 200 0) (make-posn 100 290) "Blue")
(list
 (make-prim-triangle
  (make-posn 200 0)
  (make-posn 300 0)
  (make-posn 300 290)
  "Blue")
 (make-prim-triangle
  (make-posn 200 0)
  (make-posn 300 290)
  (make-posn 200 290)
  "Blue")))


(check-expect(rect (make-posn 200 200) (make-posn 100 100) "Blue")
(list
 (make-prim-triangle
  (make-posn 200 200)
  (make-posn 300 200)
  (make-posn 300 300)
  "Blue")
 (make-prim-triangle
  (make-posn 200 200)
  (make-posn 300 300)
  (make-posn 200 300)
  "Blue")))
;(render-image (make-posn canvas-width canvas-height)
 ;               (rect (make-posn 150 150) (make-posn 70 70) "Blue"))






;; PART C

;; A Point is a (list Int Int)
;; An Offset is a Point

;; A ShapeID is a Sym
;; requires: ShapeID is not 'circle, 'triangle, 'rectangle, 'component

;; A Shape is one of:
;; - (list 'circle ShapeID radius ImageColor)
;; - (list 'triangle ShapeID Point Point Point ImageColor)
;; - (list 'rectangle ShapeID width height ImageColor)
;; - (list 'component ShapeID Picture)
;; requires: radius,width,height are Nat

;; The ShapeID of a component does not appear in its Picture
;; when recursively expanded.
;; (i.e. there are no circular definitions)

;; A Picture is a (listof (list Offset ShapeID))

;; A ShapeList is a (listof Shape)
;; requires: every ID in the ShapeList is unique

;; A BundledDrawing is a (list width height Picture ShapeList)
;; requires: width, height are Nat
;; Every ShapeID in the Picture occurs in ShapeList.



(define fun-shapes
  '((circle face 60 "Blue")
    (triangle right-eye (50 70) (60 80) (70 100) "Cyan")
    (triangle left-eye (10 20) (20 30) (30 40) "Red")
    (circle upper-lip 25 "Blue")
    (rectangle lips 10 5 "Brown")
    (circle mouth 25 "White")
    (component eyes
               (((130 140) left-eye)
                ((170 140) right-eye)))
    (component smile
               (((150 175) upper-lip)
                ((150 180) mouth)))
    (component smiley-eye-mess
               (((30 30) eyes)
                ((100 100) smile)))
    (component lip-test
               (((40 40) lips)
               ((30 30) eyes)))
    (triangle nose-top (145 160) (155 160) (150 175) "Green")
    (triangle nose-bottom (145 160) (155 160) (155 145) "Green")))

(define fun-pic
  '(
    ((10 50) eyes)
    ((10 10) eyes)
    ((30 30) smile)
    ((100 50) nose-top)
    ((150 150) face)))


(define fun-drawing (list canvas-height canvas-width fun-pic fun-shapes))



;;PART D

;;(get-picture-ids-dup mypic myshapelist original-shapelist)consumes a Picture
;;   and 2 ShapeLists, and produces a list of ShapeIDs that occur in the
;;   Picture, with duplicates allowed.

;;get-picture-ids-dup: Picture ShapeList ShapeList -> (listof ShapeID)
;; requires: original-shapelist and myshapelist are identical at first

(define (get-picture-ids-dup mypic myshapelist original-shapelist)
  (cond [(empty? mypic) empty]
        [(empty? myshapelist)
       (get-picture-ids-dup (rest mypic) original-shapelist original-shapelist)]
        
        [(symbol=? (second(first mypic)) (second(first myshapelist)))
         (cond [(not (symbol=? (first(first myshapelist)) 'component))
                ;if the symbol isn't 'component, then the second element
                ;   is the shapeid
                (cons (second(first mypic))
                      (get-picture-ids-dup
                       (rest mypic) original-shapelist original-shapelist))]

               ;if the symbol is 'component, then there is an inner list of
               ;  id's that must be searched
               [else (cons (second(first mypic))
                         (append(get-picture-ids-dup(third (first myshapelist))
                                     original-shapelist original-shapelist) 
                                (get-picture-ids-dup (rest mypic)
                                     original-shapelist original-shapelist)))])]
        
      [else (get-picture-ids-dup mypic (rest myshapelist) original-shapelist)]))



;; (remove-dup listofid)removes all duplicate elements from a list
;;remove-dup: (listof Any) -> (listof Any)

(check-expect(remove-dup '(a b b c d d e))(list 'a 'b 'c 'd 'e))

(define (remove-dup listofid)
   
  (local ;(remove-empty listofid) removes all of the occurences of "empty"
          ;    that appear in a list (ie. it removes all lists of empty)
          ;    remove-empty: (listof Any) -> (listof Any)
          [(define (remove-empty listofid)
            (cond [(empty? listofid) empty]
                [(empty? (first listofid)) (remove-empty (rest listofid))]
                [else (cons (first listofid) (remove-empty (rest listofid)))]))]
          (cond [(empty? listofid)empty]
                [else 
                 (local
                   ;(checker listofid first-element) checks if an element
                   ;    (first element) appears any other times in a list
                   ;    If it doesn't, that element is returned.
                   ;checker: (listof X) X -> (anyof X empty)
                   ;requires: first-element was the first element of a list
                   ;          listofid is the rest of the list from which
                   ;          first-element was taken
                   [(define (checker listofid first-element)
                     (cond [(empty? listofid) first-element]
                           [(symbol=? first-element (first listofid))
                            empty]
                           [else (checker (rest listofid) first-element)]))]
             (remove-empty(cons (checker (rest listofid) (first listofid))
                                (remove-dup (rest listofid)))))])))





;;(get-picture-ids- mypic myshapelist)consumes a Picture
;;   and a ShapeList, and produces a  list of ShapeIDs that occur in the
;;   Picture, with no duplicates.

;;get-picture-ids-dup: Picture ShapeList -> (listof ShapeID)
;; requires: 


(check-expect(get-picture-ids '(((100 100) smiley-eye-mess)) fun-shapes)
(list 'smiley-eye-mess 'eyes 'left-eye 'right-eye 'smile 'upper-lip 'mouth))

(define (get-picture-ids mypic myshapelist)
  (local
    [(define id-production (get-picture-ids-dup mypic myshapelist myshapelist))]
    (remove-dup id-production)))

(check-expect(get-picture-ids fun-pic fun-shapes)
(list 'eyes 'left-eye 'right-eye 'smile 'upper-lip 'mouth 'nose-top 'face))

(check-expect(get-picture-ids '(((100 100) face)) fun-shapes)(list 'face))

(check-expect(get-picture-ids '(((100 100) notface)) fun-shapes)empty)


   
  
                     
    



;;PART E

;;(picture->primitives mypicture myshapelist)
;;  consumes a Picture and a ShapeList, and produces
;;  a list of PrimElements that can be rendered

;;picture->primitives: Picture ShapeList -> (listof PrimElements)


(check-expect(picture->primitives fun-pic fun-shapes)
             (list
              (make-prim-triangle
               (make-posn 150 210)
               (make-posn 160 220)
               (make-posn 170 230)
               "Red")
              (make-prim-triangle
               (make-posn 230 260)
               (make-posn 240 270)
               (make-posn 250 290)
               "Cyan")
              (make-prim-triangle
               (make-posn 150 170)
               (make-posn 160 180)
               (make-posn 170 190)
               "Red")
              (make-prim-triangle
               (make-posn 230 220)
               (make-posn 240 230)
               (make-posn 250 250)
               "Cyan")
              (make-prim-circle
               (make-posn 180 205)
               25
               "Blue")
              (make-prim-circle
               (make-posn 180 210)
               25
               "White")
              (make-prim-triangle
               (make-posn 245 210)
               (make-posn 255 210)
               (make-posn 250 225)
               "Green")
              (make-prim-circle
               (make-posn 150 150)
               60
               "Blue")))

(define (picture->primitives mypicture myshapelist)
  (local [(define original-shapelist myshapelist)]
    (cond [(empty? mypicture) empty]
          [else
           (local
           ;(shapelist-search part-of-pic myshapelist) searches for the shapeID
           ;  for the (first Picture) in a given ShapeList, and determines the
           ;  type of shape that shapeID refers to, and using the offset
           ;  determined in the picture, it creates the primitive(s)as dictated
           ;  by the shape type. If the shape type is a component, the pictures
           ;  inside the component must go through mutual recursion. If there
           ;  no match for the shapeID, it returns empty

           ;  shapelist-search: (list Offset ShapeID) ShapeList ->
           ;                                (listof PrimElements)
             
             [(define (shapelist-search part-of-pic myshapelist) 
                   (local [(define recursive-call
                     (picture->primitives (rest mypicture) original-shapelist))]

                       ;reaches empty only if shape is not in shapelist
                       (cond [(empty? myshapelist) empty]
                             
                           [(symbol=? (second part-of-pic)
                                      (shape-id (first myshapelist)))
                            (local
                              ;(prim-outputs part-of-pic myshapelist)
                              ;   determines which primitives to output
                              ;   depending on the necessary shape-type

                              ;prim-outputs: (list Offset ShapeID) ShapeList
                              ;             -> (listof PrimElements)
                              ;requires: 
                              [(define (prim-outputs part-of-pic myshapelist)
                                 (cond
                             [(symbol=? (shape-type (first myshapelist))'circle)
                                   
                                   (cons (make-prim-circle
                                         (make-posn (first(first part-of-pic))
                                                    (second(first part-of-pic)))
                                          (circle-radius(first myshapelist))
                                          (circle-color(first myshapelist)))
                                          recursive-call)]
                                         
                                
                  [(symbol=? (first (first myshapelist)) 'triangle)
                               
                  (cons (make-prim-triangle
                      (make-posn (+ (point-x (first part-of-pic))
                                    (point-x (triangle-p1 (first myshapelist))))
                                (+ (point-y (first part-of-pic))
                                   (point-y (triangle-p1 (first myshapelist)))))
                      (make-posn (+ (point-x (first part-of-pic))
                                    (point-x (triangle-p2 (first myshapelist))))
                                (+ (point-y (first part-of-pic))
                                   (point-y (triangle-p2 (first myshapelist)))))
                      (make-posn (+ (point-x (first part-of-pic))
                                    (point-x (triangle-p3 (first myshapelist))))
                                (+ (point-y (first part-of-pic))
                                   (point-y (triangle-p3 (first myshapelist)))))
                      (triangle-color (first myshapelist)))
                         recursive-call)]

                             [(symbol=? (first (first myshapelist)) 'rectangle)
                              (append
                               (rect (make-posn (first(first (first mypicture)))
                                              (second(first (first mypicture))))
                                               (make-posn
                                                (third(first myshapelist))
                                                (fourth (first myshapelist)))
                                               (rect-color (first myshapelist)))
                                         recursive-call)]

                                  
                              [(symbol=? (first (first myshapelist)) 'component)
                               (local
                                   ;(offset-adder offset component-pic)adds the
                                   ;   x and y values in the offset to each of
                                   ;   the corresponding values in the offsets
                                   ;   of the picture part of the component.
                                   ;   offset-adder: Posn Picture -> Picture
                               [(define (offset-adder offset component-pic)
                                 (cond [(empty? component-pic) empty]
                                    [else
                                    (cons(list (list (+ (first offset)
                                          (first (first (first component-pic))))
                                                     (+ (second offset)
                                        (second (first (first component-pic)))))                                               
                                                (second (first component-pic)))
                                         
                                 (offset-adder offset (rest component-pic)))]))]
                                                   
                               (append (picture->primitives
                                        (offset-adder (first part-of-pic)
                                                    (third (first myshapelist)))
                                                            original-shapelist)
                                                                     
                                         recursive-call))]))]
                              
                              (prim-outputs part-of-pic myshapelist))]

                   [else (shapelist-search part-of-pic (rest myshapelist))])))]
             (shapelist-search (first mypicture) myshapelist))])))
                                                                 

;(render-image (make-posn 300 300)
 ;             (picture->primitives '(((90 100) lips)) fun-shapes))

(check-expect(picture->primitives '(((90 100) lips)) fun-shapes)
(list
 (make-prim-triangle
  (make-posn 90 100)
  (make-posn 100 100)
  (make-posn 100 105)
  "Brown")
 (make-prim-triangle
  (make-posn 90 100)
  (make-posn 100 105)
  (make-posn 90 105)
  "Brown")))



(check-expect(picture->primitives '(((90 100) lips)
                       ((10 100) eyes)) fun-shapes)
(list
 (make-prim-triangle
  (make-posn 90 100)
  (make-posn 100 100)
  (make-posn 100 105)
  "Brown")
 (make-prim-triangle
  (make-posn 90 100)
  (make-posn 100 105)
  (make-posn 90 105)
  "Brown")
 (make-prim-triangle
  (make-posn 150 260)
  (make-posn 160 270)
  (make-posn 170 280)
  "Red")
 (make-prim-triangle
  (make-posn 230 310)
  (make-posn 240 320)
  (make-posn 250 340)
  "Cyan")))

(check-expect(picture->primitives '(((90 100) lip)) fun-shapes)empty)

(check-expect(picture->primitives '(((90 100) smiley-eye-mess)) fun-shapes)
(list
 (make-prim-triangle (make-posn 260 290) (make-posn 270 300)
                     (make-posn 280 310) "Red")
 (make-prim-triangle (make-posn 340 340) (make-posn 350 350)
                     (make-posn 360 370) "Cyan")
 (make-prim-circle (make-posn 340 375) 25 "Blue")
 (make-prim-circle (make-posn 340 380) 25 "White")))

(check-expect(picture->primitives '(((90 100) smiley-eye-mess)
                       ((40 50) lips))fun-shapes)
(list
 (make-prim-triangle (make-posn 260 290) (make-posn 270 300)
                     (make-posn 280 310) "Red")
 (make-prim-triangle (make-posn 340 340) (make-posn 350 350)
                     (make-posn 360 370) "Cyan")
 (make-prim-circle (make-posn 340 375) 25 "Blue")
 (make-prim-circle (make-posn 340 380) 25 "White")
 (make-prim-triangle (make-posn 40 50) (make-posn 50 50)
                     (make-posn 50 55) "Brown")
 (make-prim-triangle (make-posn 40 50) (make-posn 50 55)
                     (make-posn 40 55) "Brown")))

(check-expect(picture->primitives '(((0 0) lip-test)) fun-shapes)
(list
 (make-prim-triangle (make-posn 40 40) (make-posn 50 40)
                     (make-posn 50 45) "Brown")
 (make-prim-triangle (make-posn 40 40) (make-posn 50 45)
                     (make-posn 40 45) "Brown")
 (make-prim-triangle (make-posn 170 190) (make-posn 180 200)
                     (make-posn 190 210) "Red")
 (make-prim-triangle (make-posn 250 240) (make-posn 260 250)
                     (make-posn 270 270) "Cyan")))

;(picture->primitives '(((40 50) lips)
 ;                                   ((90 100) smiley-eye-mess))fun-shapes)


;;PART F

;;(drawing->image mybundle) consumes a BundledDrawing and produces an Image.
;;drawing->image: BundledDrawing -> Image 

(define (drawing->image mybundle)
  (render-image (make-posn (first mybundle) (second mybundle))
                (picture->primitives (third mybundle) (fourth mybundle))))

(drawing->image fun-drawing)

                       
