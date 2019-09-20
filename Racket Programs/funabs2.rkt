;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname funabs2) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))

;these two are the same 
(define (f n) (..))
;(define f (lambda (n)...))


;;just taking away the name (that is sometimes)
;;lambda is the actual function, sortof like defining a constant that is a funcction

(define (divisble30? n)
  (zero? (remainder n 3)))

(define divisble3?
  (lambda (n) (zero? (remainder n 3))))


;;tfae

;(define (make-adder0 c)
 ;      (local [(define (adderX y)) (+ x y)]
  ;       adderX))

;;adds 5 to a number
((make-adder0 5)10)

(define (make-adder x)
  (lambda (y) (+ x y)))

(define make-adder2
  (lambda (x)
    (lambda (y) (+ x y))))

(((lambda (x) (lambda (y) (+ x y)))5)10)



(define (a->b/c chars)
  (cons
   [(empty? chars) empty]
   [(char=? (first chars) #\a) (cons #\b (a->b/c (rest chars)))]
   [else (cons (first chars) (a->b/c (rest chars)))]))
  
(define (a->b str)
  (list->string(a->b/c (string->list str))))

(a->b "abracadabara")


(define (isChar? (lambda (c) (char=? c char))))

(define (makeChar char) (lambda (c) char))


;;need to use type variables for the contract


;;X can be anything, but each X is the same Anything
;;filter: (X->Bool)(listof X) -> (listof X)


(define (negate numbers)
  (cond
   [(empty? numbers) empty]
   [else (cons (- (first numbers)) (negate (rest numbers)))]))


(define (negate-general func numbers)
  (cond
   [(empty? numbers) empty]
   [else (cons (func (first numbers)) (negate (rest numbers)))]))

;;can replace function with anything, mapping

(define (my-map func numbers)
  (cons
   [(empty? numbers) empty]
   [else (cons (func (first numbers)) (my-map func (rest numnbers)))]))

;;function is now a halfing function
(map (my-map (lambda (n) n/2) '(1 2 3 4 5 6)))

;;MAP CONTRACT
;;mapping x to y and doing a thing
;; (X->Y) (listof X) -> (listof Y)

;;can replace the pus with any iother functino as long as the outputs are proper
(define (add/l numbers)
  (cond
    [(empty? numbers) 0]
    [else (+ (first numbers) (add/l (rest numbers)))]))

(add/l '(1 2 3 4 5))

(define (add/lf func base numbers)
  (cond
    [(empty? numbers) base]
    [else (func (first numbers) (add/lf func base (rest numbers)))]))

;folding new numbers to the old numbres, this folding
(add/lf + 0 '(1 2 3 4))