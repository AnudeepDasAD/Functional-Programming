;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname all-route) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
;; (neighbours v G) produces list of neighbours of v in G
;; neighbours: Node Graph -> (listof Node)
;; requires: v is a node in G
(define (neighbours v G)
  (second (first (filter (λ (i) (symbol=? (first i) v)) G))))



;; (find-all-routes orig dest G) finds all the routes from orig to dest in G
;;    if it exists
;; find-route: Node Node Graph -> (listof (listof Node))

(define (find-all-routes orig dest G)
  (cond [(symbol=? orig dest) (list (list orig))]
        [else (map (λ (i) (cons orig i))
                   (find-all-routes/list (neighbours orig G) dest G))]))


;; (find-all-routes/list los dest G) produces route from
;; an element of los to dest in G, if one exists
;; find-all-routes/list: (listof Node) Node Graph -> (listof (listof Node))
(define (find-all-routes/list los dest G)
  (foldr append '()
         (map (λ (i) (find-all-routes i dest G)) los)))


(define a-graph '((A (B D))
                     (B (C))
                     (C ())
                     (D (C))))

(find-all-routes 'A 'C a-graph)