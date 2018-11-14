;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname 11b) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require racket/string)

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

; graph=?/curried : Graph -> [Graph -> Boolean]
; Curried graph=?
(define graph=?/curried (λ (g1) (λ (g2) (graph=? g1 g2))))

; Exercise #5
; collapse : Symbol Symbol Symbol Graph -> Graph
; Collapses nodes s1 and s2 into new in g
(define (collapse s1 s2 new g)
  (local [(define new-neigh (both-neighbors g s1 s2))
          ; filter-old : [List-of Symbol] -> [List-of Symbol]
          ; Scrapes out old symbols and inserts new symbol
          (define (filter-old los)
            (cons new
                  (filter (λ (n) (not (or (symbol=? n s1) (symbol=? n s2))))
                          los)))
          (define new-nodes (filter-old (graph-nodes g)))
          ; update-neighbors: [List-of Symbol] -> [List-of Symbol]
          ; Updates a list of symbols, removing instances of s1 and s2 and replacing them with new
          (define (update-neighbors los)
            (if (or (member? s1 los) (member? s2 los))
                (filter-old los)
                los))
          (define new-graph
            (make-graph
             new-nodes
             (λ (s) (cond [(symbol=? s new) (update-neighbors new-neigh)]
                          [(or (symbol=? s s1) (symbol=? s s2)) (error "Node does not exist")]
                          [else (update-neighbors ((graph-neighbors g) s))]))))]
    new-graph))
    

(check-satisfied (collapse 'a 'b 'q G-1)
                 (graph=?/curried (make-graph '(q c)
                                              (λ (n) (cond [(symbol=? n 'q) '(q c)]
                                                           [(symbol=? n 'c) '(q)])))))
(check-satisfied (collapse 'd 'f 'z G-2)
                 (graph=?/curried (make-graph '(z e)
                                              (λ (n) (cond [(symbol=? n 'z) '(z e)]
                                                           [(symbol=? n 'e) '(e z)])))))

; Exercise #6
; reverse-edges : Graph -> Graph
; Reverses the edges of the given graph
(define (reverse-edges g)
  (local [; get-connected : Symbol -> [List-of Symbol]
          ; Gets all nodes connected to the given node
          (define (get-connected s)
            (filter (λ (n) (neighbor-of? g s n)) (graph-nodes g)))]
    (make-graph (graph-nodes g)
                (λ (s) (get-connected s)))))

(check-satisfied (reverse-edges G-1)
                 (graph=?/curried (make-graph '(a b c)
                                              (λ (n) (cond [(symbol=? n 'a) '(c)]
                                                           [(symbol=? n 'b) '(b a)]
                                                           [(symbol=? n 'c) '(a)])))))
(check-satisfied (reverse-edges G-2)
                 (graph=?/curried (make-graph '(d e f)
                                              (λ (n) (cond [(symbol=? n 'd) '(d e)]
                                                           [(symbol=? n 'e) '(e f)]
                                                           [(symbol=? n 'f) '(d e)])))))

; Exercise #7
; rename : Graph [List-of Symbol] -> Graph
; Renames the symbols in the graph to the new symbol names
(define (rename g los)
  (local [; rename-recur : [List-of Symbol] [List-of Symbol] Graph -> Graph
          ; Recursively renames each symbol with its new name
          (define (rename-recur l1 l2 base)
            (cond [(empty? l1) base]
                  [(cons? l1) (collapse (first l1) (first l1) (first l2)
                                        (rename-recur (rest l1) (rest l2) base))]))]
    (rename-recur (graph-nodes g) los g)))

(check-satisfied (rename G-1 '(d e f))
                 (graph=?/curried (make-graph '(d e f)
                                              (λ (n) (cond [(symbol=? n 'd) '(e f)]
                                                           [(symbol=? n 'e) '(e)]
                                                           [(symbol=? n 'f) '(d)])))))
(check-satisfied (rename G-2 '(a b c))
                 (graph=?/curried (make-graph '(a b c)
                                              (λ (n) (cond [(symbol=? n 'a) '(a c)]
                                                           [(symbol=? n 'b) '(b a c)]
                                                           [(symbol=? n 'c) '(b)])))))

; node-name->numbers : Symbol -> (list Nat Nat)
; Convert a symbol of the form 'n1->n2 to (list n1 n2)
(define (node-name->numbers s)
  (map string->number (string-split (symbol->string s) "->")))
(check-expect (node-name->numbers '0->3) '(0 3))

; Exercise #8
; swap : Graph -> Graph
; Swaps a graph's nodes with its edges
(define (swap g)
  (local [(define og-nodes (graph-nodes g))
          ; symbol->index : [List-of Symbol] Symbol Number -> Natural
          ; Given a list of symbols and a symbol that exists in that list, returns its index (0 based)
          (define (symbol->index los s index)
            (cond [(empty? los) index]
                  [(cons? los) (if (symbol=? (first los) s)
                                   index
                                   (symbol->index (rest los) s (add1 index)))]))
          (define new-nodes (apply append (map (λ (s)
                                                 (map (λ (n)
                                                        (string->symbol
                                                         (string-append
                                                          (number->string
                                                           (symbol->index og-nodes s 0))
                                                          "->"
                                                          (number->string
                                                           (symbol->index og-nodes n 0)))))
                                                      ((graph-neighbors g) s)))
                                               (graph-nodes g))))
          ; new-edges : Symbol -> [List-of Symbol]
          ; Given a symbol in the new format, returns the symbol's new edges
          (define (new-edges s)
            (filter (λ (n) (= (first (node-name->numbers n)) (second (node-name->numbers s))))
                    new-nodes))]
    (make-graph new-nodes new-edges)))

(check-satisfied (swap G-1)
                 (graph=?/curried (make-graph '(0->1 0->2 1->1 2->0)
                             (λ (n) (cond [(symbol=? n '0->1) '(1->1)]
                                          [(symbol=? n '0->2) '(2->0)]
                                          [(symbol=? n '1->1) '(1->1)]
                                          [(symbol=? n '2->0) '(0->1 0->2)])))))
(check-satisfied (swap G-2)
                 (graph=?/curried (make-graph '(0->0 0->2 1->1 1->0 1->2 2->1)
                             (λ (n) (cond [(symbol=? n '0->0) '(0->0 0->2)]
                                          [(symbol=? n '0->2) '(2->1)]
                                          [(symbol=? n '1->1) '(1->1 1->0 1->2)]
                                          [(symbol=? n '1->0) '(0->0 0->2)]
                                          [(symbol=? n '1->2) '(2->1)]
                                          [(symbol=? n '2->1) '(1->1 1->0 1->2)])))))

; Exercise #9
; close? : Graph Symbol Symbol Natural -> Boolean
; Determines if s2 is within n steps of s1 in the given graph g
(define (close? g s1 s2 n)
  (local
    ; close?/recur : Symbol Number -> Boolean
    ; Determines if s2 is within dist steps of curr-node
    [(define (close?/recur curr-node dist)
       (local
         [(define neighbors ((graph-neighbors g) curr-node))]
       (cond
         [(<= dist 0) #false]
         [(> dist 0) (or (member? s2 neighbors)
                          (ormap (λ (node) (close?/recur node (sub1 dist))) neighbors))])))]
    (close?/recur s1 n)))


(check-expect (close? G-1 'a 'b 1) #true)
(check-expect (close? G-1 'c 'a 2) #true)
(check-expect (close? G-1 'c 'b 1) #false)
(check-expect (close? G-1 'c 'b 2) #true)
(check-expect (close? G-1 'c 'a 0) #false)