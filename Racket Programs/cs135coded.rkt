;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname cs135coded) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
;;
;;************************************
;;   Anudeep Das (20782887)
;;   CS 135 Fall 2018
;;   Assignment 05, Problem 2                                    
;;*************************************



;; PART A


;;The folowing is a helper function

;;(get-char-from-char-list string-as-char-list index) gets the char
;;   located at the given index of the list.

;;get-char-from-char-list: (listof Char) Nat -> Char

(check-expect(get-char-from-char-list
              (cons #\a (cons #\b (cons #\c (cons #\d (cons #\e
                                      (cons #\f (cons #\g empty))))))) 0) #\a)

(define (get-char-from-char-list string-as-char-list index)
  (cond
        [(or (< index 0) (empty? string-as-char-list)) #\*]
        [(= index 0) (first string-as-char-list)]
        [else (get-char-from-char-list (rest string-as-char-list) (- index 1))]
        )
  )



;;(get-char string-to-search index) determines the character
;;   located at the certain index of a string.

;;get-char: Str Nat -> Char

(check-expect(get-char "abcdefg" 0) #\a)
(check-expect(get-char "abcdefg" 3) #\d) 

(define (get-char string-to-search index)
  (cond [(empty? (string->list string-to-search)) #\*]
        [else (get-char-from-char-list (string->list string-to-search) index)]))

(check-expect(get-char "abcdefg" 20)#\*)
(check-expect(get-char "" 1)#\*)
(check-expect(get-char "a" 1)#\*)
(check-expect(get-char "a" 0)#\a)
(check-expect(get-char "abcdefg" 2)#\c)
(check-expect(get-char "abcdefg" 6)#\g)






;;PART B

;;(coded-3-char string-to-decrypt Decryptor)
;;    consumes a string and a Decryptor (defined below),
;;    and produces a string containing the 3-character “secret message” hidden
;;    in the consumed string.

;;coded-3-char: Str Decryptor -> Str
;; requires: A Decryptor is a (list Nat Nat Nat)

(check-expect(coded-3-char "abcdefg" (list 0 0 0)) "abc") 
(check-expect(coded-3-char "abcdefg" (list 2 1 0)) "cef")

(define (coded-3-char string-to-decrypt Decryptor)
  (list->string (cons (get-char string-to-decrypt (first Decryptor)) 
                (cons (get-char string-to-decrypt
                                (+ (first Decryptor) (second Decryptor) 1))
                (cons (get-char string-to-decrypt
                                (+ (first Decryptor) (second Decryptor)
                                   (third Decryptor) 2)) empty)))))

         
(check-expect(coded-3-char "abcdefg" (list 2 10 0)) "c**")
(check-expect(coded-3-char "abcdefg" (list 2 2 2)) "cf*")
(check-expect(coded-3-char "abcdefg" (list 6 0 0)) "g**")
(check-expect(coded-3-char "abcdefg" (list 4 0 0)) "efg")
(check-expect(coded-3-char "abcdefg" (list 4 0 1)) "ef*")
(check-expect(coded-3-char "abcdefg" (list 5 0 0)) "fg*")
(check-expect(coded-3-char "" (list 0 0 0 )) "***")
(check-expect(coded-3-char "abcdefg" (list 4 0 0)) "efg")








;;PART C

(define error-case -100)
(define increase-by-1-index 1)
(define value-if-first-value-matches 0)

;;The following is a helper function

;;(index-finder big-string-as-char-list char-of-secret) determines the index
;;  of a given char in a given char list.

;;index-finder: (listof Char) Char -> Nat


(check-expect (index-finder (cons #\a (cons #\b (cons #\c (cons #\d (cons #\e
                                 (cons #\f (cons #\g empty))))))) #\b) 1)
(check-expect (index-finder empty #\b) -100)


(define (index-finder big-string-as-char-list char-of-secret)
  (cond [(empty? big-string-as-char-list) error-case]
        [(char=? (first big-string-as-char-list) char-of-secret)
         value-if-first-value-matches]
        [else (+ increase-by-1-index
                 (index-finder(rest big-string-as-char-list)char-of-secret))]))



;; The following is another helper function      

;;(enc-possible-char-lists? big-string-as-char-list secret-message-as-char-list)
;;   does the same thing as enc-possible, but using (listof Char) instead of
;;   strings for the parameters. It consumes two (listof Char) and produces
;;   true if the second (listof Char) may be hidden inside the first string
;;   using a Decryptor, and false otherwise.

(check-expect (enc-possible-char-lists? "abcdeg" empty) true)
                                        
(define (enc-possible-char-lists? big-string-as-char-list
                                  secret-message-as-char-list)
  (cond [(empty? secret-message-as-char-list) true]
        [(empty? (rest secret-message-as-char-list)) true]
        [(>= (index-finder big-string-as-char-list
                           (first secret-message-as-char-list))
             (index-finder big-string-as-char-list
                           (first (rest secret-message-as-char-list))))
         false]
        [else (enc-possible-char-lists? big-string-as-char-list
                                        (rest secret-message-as-char-list))]))
            



;;(enc-possible? big-string secret-message)consumes two strings and produces
;;   true if the second string may be hidden inside the first string using a
;;   Decryptor, and false otherwise.

;;enc-possible: Str Str -> Bool
;; requires:    secret-message must have a length of 3

(check-expect (enc-possible? "abcdefg" "bdg") true)
(check-expect(enc-possible? "abcdefg" "abz")false)

(define (enc-possible? big-string secret-message)
  (enc-possible-char-lists? (string->list big-string)
                            (string->list secret-message)))

               
(check-expect(enc-possible? "abcdefg" "bac")false)
(check-expect(enc-possible? "abcdefg" "abc")true)
(check-expect(enc-possible? "abcdefg" "abd")true)
(check-expect(enc-possible? "abcdefg" "acb")false)
(check-expect(enc-possible? "abcdefg" "cde")true)
(check-expect(enc-possible? "abcdefg" "cfg")true)
(check-expect(enc-possible? "abcdefg" "cdg")true)
(check-expect(enc-possible? "abcdefg" "cbd")false)
(check-expect(enc-possible? "abcdefg" "***")false)                  

         
         




