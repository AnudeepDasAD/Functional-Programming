;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname general-tree) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
(define-struct opnode (op params))
;;params is basivally the different numbers/ internal nodes

(define equation (make-opnode '* 10 (make opnode '+ 2 5)))

(define (process/tree2 t)
  (cond
    [(number? t) (process/number t)]
    [(symbol=? (opnode-params t) '+) (process/plus t)]
    [(symbol=? (opnode-params t) '*) (process/mult t)]))

(define (process/number n)
  n)


;;must now call process/tree on all the children, present in the list


;;takes whatever is on the left and adds it to the right
(define (process/plus params)
  (cond
    ;processing addition
    [(empty? params)0]
    ;a the list treated as a subtree, and determines what to do next
    ;;then deals with all the other parameters
    [else (+ (process-tree2 (first params)) (process/plus (rest params)))]))

(define (process/mult params)
  (cond
    ;processing addition
    [(empty? params) 1]
    ;a the list treated as a subtree, and determines what to do next
    ;;then deals with all the other parameters
    [else (* (process-tree2 (first params)) (process/mult (rest params)))]))

(process/tree2 equation)