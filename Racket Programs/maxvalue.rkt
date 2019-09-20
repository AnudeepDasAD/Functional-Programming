;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname maxvalue) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
;; What's the largest number ib my list

;(define (max-list/s lst)
;  (cond ;[(empty? lst)...]
;         [(empty (rest lst)) (first lst)] ;if the rest of the list is empty, you have found the largest number;;

         ;if the current number is larger than the largest number in the rest of the list, found the largest number
;         [(> (first lst) (max-list/s (rest lst))) (first lst)] 
;         [else (max-list/s (rest lst))]
;         ))

;(max-list/s (list 1 2 3 4 5 6 7 8 9 10))

(define (max-list/a lst max-so-far)
  (cond [(empty? (rest lst)) (first lst)] ;if the rest is empty, the current number (first) is the largest
        ;comparison, (first lst) is the new largest number
        [(> (first lst) max-so-far) (max-list/a (rest lst) (first lst))]
        [else (max-list/a (rest lst) max-so-far)]))

(max-list/a (list 1 2 3 4 5 6 7 8 9 10) (first (list 1 2 3 4 5 6 7 8 9 10)))





;;need to get first element to the end

(define (reverse/s lst)
  (cond [(empty? lst)empty]
        ;;append takes two lists, and smashes them together
        [else (append (reverse/s (rest lst)) (list (first lst)))])) ;puts the first value at the end everytime

;;doesn't smell nice apparently (accumulate and list called too many times

(define (reverse/a lst list-so-far)
  (cond [(empty? lst) list-so-far] ;;reverse list is at the accumulator
        
        [else (reverse/a (rest lst) (cons (first lst) list-so-far))]))

;;the first element will be added first and the rest of the elements will be added in in front of it, thus reversing it


(reverse/a (list 10 11 12 13 14 15) empty)
        

        