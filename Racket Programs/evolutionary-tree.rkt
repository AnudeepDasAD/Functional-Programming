;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname evolutionary-tree) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
(define-struct animal (name danger))
(define-struct event (name age left right))

(define (tree-template t)
  (cond
    [(animal? t)...]
    [(event? t)...]
    ))



(define (event-template e)
  (...(event-name e)...
  (event-age e)...
  (event-left e)...
  (event-right e)...))


(define (animal-template a)
  (... (animal-name a)...
       (animal-danger)...))

;(define invertebrate (make...))
;(define animals (make-event "Animal" 535 vertebrate invertebrate))


;(define (process/tree t)
 ; ;dealing with event or animal?
  ;(cond
   ; [(animal? t)(process/animal t)]
    ;[(event? t) (process/event t)]
    ;))

(define (process/animal a)
  (list (animal-name a)))

(define (process/event e)
        (append (list (event-name e)) ;list from event node 
                (process/tree (event-left e))  ;plus all names in the left subtree
                (process/tree (event-right e)))) ;and right tree


;(process/tree animals)


(define (process/tree1 t names/acc)
  (cond
    [(animal? t)(process/animal t names/acc)]
    [(event? t) (process/event t names/acc)]
    ))


(define (process/animal1 a names/acc)
  (cons (animal-name a) names/acc))


;; consing the right subtree to the names accumulator and then doing that to the left tree, and then do it to the event name
(define (process/event1 e names/acc)
        (cons  (event-name e)  (process/tree1 (event-left e)  (process/tree1 (event-right e) names/acc))))


;(process/tree mammal (list))


(define-struct opnode (op left right))

(define equation (make-opnode '* 10 (make opnode '+ 2 5)))

(define (process/tree2 t)
  (cond
    [(number? t) (process/number t)]
    [(symbol=? (opnode-op t) '+) (process/plus t)]
    [(symbol=? (opnode-op t) '*) (process/mult t)]))

(define (process/number n)
  n)


;;takes whatever is on the left and adds it to the right
(define (process/plus opn)
  (+ (process-tree2 (opnode-left opn)) (process/tree2 (opnode-right))))

(define (process/mult opn)
  (* (process-tree2 (opnode-left opn)) (process/tree2 (opnode-right))))

(process/tree2 equation)
