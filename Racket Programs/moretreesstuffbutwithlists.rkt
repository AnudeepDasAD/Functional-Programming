;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname moretreesstuffbutwithlists) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
(define my-list '((1 (2 3) 4 (5 (6 7 8) 9 ()))))

(define (traverse/list list)
  (cond
    [(empty? list)...]
    [(number? (first list))(...(first list)...(traverse/list (rest list))...)]
    ;element is a list, thus must be traversed
    ;and the rest has to traversed once again
    [else (... (traverse/list (first list))... (traverse/list (rest list)))]))
    

(define (add/list list)
  (cond
    [(empty? list) 0]
    ;adds the number to the sum of all the elements in the list
    [(number? (first list))(+ (first list)(add/list (rest list)))]
    ;adding numbers so just add the things together
    [else (+ (add/list (first list)) (add/list (rest list)))]))

(add/list my-list)

(define (flatten/list list)
  (cond [(empty? list) empty];a list of the numbers is returned
        [(number? (first list)) (cons (first list)(flatten/list (rest list)))]
        ;if it is a list, its flattened version must be appended to the rest
        [else (append (flatten/list (first list)) (flatten/list (rest list)))]))

(flatten/list my-list)