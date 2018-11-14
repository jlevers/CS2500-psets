;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname 11b) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; A Graph is a (make-graph [List-of Symbol] [Symbol -> [List-of Symbol]])
(define-struct graph [nodes neighbors])
; and represents the nodes and edges in a graph
; All of the symbols in nodes are assumed to be unique, as are the symbols in
; any list returned by neighbors, and all of the symbols returned by neighbors
; are assumed to be in nodes.

(define G-1 (make-graph '(a b c)
                        (λ (n) (cond [(symbol=? n 'a) '(b c)]
                                     [(symbol=? n 'b) '(b)]
                                     [(symbol=? n 'c) '(a)]))))
(define G-2 (make-graph '(d e f)
                        (λ (n) (cond [(symbol=? n 'd) '(d f)]
                                     [(symbol=? n 'e) '(e d f)]
                                     [(symbol=? n 'f) '(e)]))))
(define G-3 (make-graph '(b c a)
                        (λ (n) (cond [(symbol=? n 'a) '(b c)]
                                     [(symbol=? n 'b) '(b)]
                                     [(symbol=? n 'c) '(a)]))))

; Exercise #2
; neighbor-of? : Graph Symbol Symbol -> Boolean
; Determines if s1 is a neighbor of s2 in g
(define (neighbor-of? g s1 s2)
  (member? s1 ((graph-neighbors g) s2)))

(check-expect (neighbor-of? G-1 'b 'c) #false)
(check-expect (neighbor-of? G-1 'c 'a) #true)


; Exercise #3
; both-neighbors : Graph Symbol Symbol -> [List-of Symbol]
; Returns de-duped list of the neighbors of both symbols
(define (both-neighbors g s1 s2)
  (local [(define neighbors-s1 ((graph-neighbors g) s1))
          (define neighbors-s2 ((graph-neighbors g) s2))]
    (append neighbors-s1 (filter (λ (s) (not (neighbor-of? g s s1))) neighbors-s2))))

(check-expect (both-neighbors G-1 'b 'c) '(b a))
(check-expect (both-neighbors G-1 'a 'b) '(b c))
(check-expect (both-neighbors G-1 'c 'c) '(a))


; Exercise #4
; graph=? : Graph Graph -> Boolean
; Determines if g1 and g2 are equal
(define (graph=? g1 g2)
  (local [(define g1-nodes (graph-nodes g1))
          (define g1-neigh (graph-neighbors g1))
          (define g2-nodes (graph-nodes g2))
          (define g2-neigh (graph-neighbors g2))
          (define same-nodes (andmap (λ (n) (member? n g1-nodes)) g2-nodes))]
    (cond [(not same-nodes) #false]  ; Make sure each graph has the same nodes
          [(not (andmap
                 identity
                 (map (λ (s) (=
                              (length (g1-neigh s))
                              (length (g2-neigh s))))
                      g1-nodes))) #false]  ; Make sure nodes have same # of neighbors in each graph
          [(not (andmap
                 identity
                 (map (λ (s) (andmap (λ (n) (member? n (g1-neigh s))) (g2-neigh s)))
                      g1-nodes))) #false]  ; Make sure each node has the same neighbors
          [else #true])))

(check-expect (graph=? G-1 G-1) #true)
(check-expect (graph=? G-1 G-3) #true)
(check-expect (graph=? G-1 G-2) #false)
(check-expect (graph=? G-1
                       (make-graph '(a b c) (λ (n) (list n)))) #false)
(check-expect (graph=? G-1
                       (make-graph '(a b c)
                                   (λ (n)
                                     (cond [(symbol=? n 'a) '(a b)]
                                           [(symbol=? n 'b) '(c)]
                                           [(symbol=? n 'c) '(a)])))) #false)


