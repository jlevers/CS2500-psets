;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname 8b) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define-struct leaf [])
(define-struct node [key info left right])
; A [BST X Y] is one of:
; - (make-leaf)
; - [Node X Y]
; Interpretation: A binary search tree containing keys of type X and info of type Y
; Examples:

(define bst0 (make-leaf))
(define bst1 (make-node 1 "one" bst0 (make-node 4 "four" bst0 bst0)))
(define bst2 (make-node "lemon" 12
                        (make-node "draw" 22 bst0 bst0)
                        (make-node "red" 11 bst0 bst0)))
(define bst3 (make-node "5long" 8
                        (make-node "4lon" 5
                                   (make-node "2l" 22 bst0 bst0)
                                   bst0)
                        (make-node "9longaaaa" 34 bst0 bst0)))

(define bst4 (make-node 4 ""
                        (make-node 3 "" bst0 bst0)
                        (make-node 7 "" bst0 bst0)))

; Template:
#;(define (bst-temp bst)
    (cond
      [(leaf? bst) ...]
      [(node? bst) (node-temp bst)]))

; A [Node X Y] is a (make-node X Y [BST X Y] [BST X Y]):
; Interpretation:
; - Where the first field represents the key of the node.
; - Where the second field represents the info mapped to the key.
; - Where the third field represents the binary tree located on the left.
; - Where the fourth field represents the binary tree located on the right.

; Examples:
(define node1 (make-node 1 "one" bst0 bst0))
(define node2 (make-node 1 "one" (make-node 2 "two" bst0 bst0) bst0))

; Template:
#;(define (node-temp node)
    (... (node-key node) ... (node-info node) ... (node-left node) ... (node-right node) ...))


(define-struct tree-map [tree comp])
; A [TreeMap X Y] is a (make-tree-map [BST X Y] [Comparison X])
; Examples:
(define tm0 (make-tree-map bst0 -))
(define tm1 (make-tree-map bst1 -))
(define string-comp-to-num (λ (input key)
                             (cond [(string<? input key) -1]
                                   [(string>? input key) 1]
                                   [else 0])))
(define string-length-to-diff (λ (input key)
                                (- (string-length input) (string-length key))))
(define tm2 (make-tree-map bst2 string-comp-to-num))
(define tm3 (make-tree-map bst3 string-length-to-diff))
; Template:
(define (tree-map-temp tm)
  (... (bst-temp (tree-map-tree tm)) ... (tree-map-comp tm) ...))


; EXERCISE 3
; ==========

; insert : X Y [TreeMap X Y] -> [TreeMap X Y]
; Inserts the X Y key-value pair into the treemap, replacing if the key already exists.
(define (insert key val tm)
  (make-tree-map (insert-bst key val (tree-map-comp tm) (tree-map-tree tm)) (tree-map-comp tm)))

(check-expect (tree-map-tree (insert -1 "negative" tm1))
              (insert-bst -1 "negative" - bst1))

; insert-bst : X Y [Comparison X] [BST X Y] -> [BST X Y]
; Inserts the X Y key-value pair into the given BST, replacing if the key already exists
(define (insert-bst key info comp bst)
  (cond
    [(leaf? bst) (make-node key info (make-leaf) (make-leaf))]
    [(node? bst) (new-node key info comp bst)]))

(check-expect (insert-bst -1 "negative" - bst1)
              (make-node 1 "one"
                         (make-node -1 "negative" bst0 bst0)
                         (make-node 4 "four" bst0 bst0)))

(check-expect (insert-bst 6 "six" - bst1)
              (make-node 1 "one"
                         bst0
                         (make-node 4 "four"
                                    bst0
                                    (make-node 6 "six" bst0 bst0))))

(check-expect (insert-bst 1 "notone" - bst1)
              (make-node 1 "notone"
                         bst0
                         (make-node 4 "four" bst0 bst0)))

(check-expect (insert-bst "ernie" 42 string-comp-to-num bst2)
              (make-node "lemon" 12
                         (make-node "draw" 22
                                    bst0
                                    (make-node "ernie" 42 bst0 bst0))
                         (make-node "red" 11 bst0 bst0)))

(check-expect (insert-bst "12looooooong" 89 string-length-to-diff bst3)
              (make-node "5long" 8
                         (make-node "4lon" 5
                                    (make-node "2l" 22 bst0 bst0)
                                    bst0)
                         (make-node "9longaaaa" 34
                                    bst0
                                    (make-node "12looooooong" 89 bst0 bst0))))


; new-node : [BST X Y] X Y [X X -> X] -> [BST X Y]
; A helper function that defines the behaviour of insert-bst when the BST is a node.
(define (new-node key info comp bst)
  (local [(define result (comp key (node-key bst)))]
    (cond [(> result 0) (make-node (node-key bst)
                                   (node-info bst)
                                   (node-left bst)
                                   (insert-bst key info comp
                                               (node-right bst)))]
          [(< result 0) (make-node (node-key bst)
                                   (node-info bst)
                                   (insert-bst key info comp
                                               (node-left bst))
                                   (node-right bst))]
          [(= result 0) (make-node key info
                                   (node-left bst)
                                   (node-right bst))])))

(check-expect (new-node -1 "negative" - bst1)
              (make-node 1 "one"
                         (insert-bst -1 "negative" - bst0)
                         (make-node 4 "four" bst0 bst0)))

; EXERCISE 4
; ==========

; Constants.
(define ERROR-TEXT "Key does not exist.")

; find : X [TreeMap X Y] -> Y
; finds the information tied to the passed key in the given tree, errors if it cannot be found.
(define (find key tm)
  (find-bst key (tree-map-comp tm) (tree-map-tree tm)))

(check-error (find 0 tm0) ERROR-TEXT)
(check-expect (find 4 tm1) (find-bst 4 - (node-right bst1)))

; find-bst : X [Comparison X] [BST X Y] -> Y
; Searches bst for key using comp
(define (find-bst key comp bst)
  (cond
    [(leaf? bst) (error ERROR-TEXT)]
    [(node? bst) (find-node bst comp key)]))

(check-error (find-bst 0 - bst0) ERROR-TEXT)
(check-expect (find-bst 4 - bst1) "four")
(check-expect (find-bst "draw" string-comp-to-num bst2) 22)
(check-expect (find-bst "9longaaaa" string-length-to-diff bst3) 34)

; find-node : [Node X Y] [Comparison X] X -> Y
; checks a node for the given key, search the left when low, right when hig, and return info if same.
(define (find-node node comp key)
  (local [(define key-comp (comp key (node-key node)))]
    (cond [(> key-comp 0) (find-bst key comp (node-right node))]
          [(< key-comp 0) (find-bst key comp (node-left node))]
          [(zero? key-comp) (node-info node)])))

(check-expect (find-node bst1 - 1) "one")
(check-expect (find-node bst2 string-comp-to-num "draw")
              (find-bst "draw" string-comp-to-num (node-left bst2)))

; EXERCISE 5
; ==========

; submap : X X [TreeMap X Y] -> [TreeMap X Y]
; returns the subtree of nodes with keys lower than hi and higher than lo.
(define (submap lo hi tm)
  (make-tree-map (bst-submap lo hi (tree-map-comp tm) (tree-map-tree tm)) (tree-map-comp tm)))

; (check-expect (submap 0 1 tm0) tm0)

; bst-submap : X X [Comparison X] [BST X Y] -> [BST X Y]
(define (bst-submap lo hi comp bst)
  (cond
    [(leaf? bst) (make-leaf)]
    [(node? bst) (submap-node bst comp lo hi)]))

(check-expect (bst-submap 3 5 - bst1)
              (node-right bst1))
(check-expect (bst-submap "b" "k" string-comp-to-num bst2)
              (make-node "draw" 22 bst0 bst0))
(check-expect (bst-submap "3lo" "8loooong" string-length-to-diff bst3)
              (make-node "5long" 8
                         (make-node "4lon" 5
                                    bst0 bst0)
                         bst0))

; submap-node : [Node X Y] [Comparison X] X X -> [Node X Y]
; returns a node correctly submapped by comparison to the given lo and hi values.
(define (submap-node node comp lo hi)
  (local [(define key-comp-lo (comp (node-key node) lo))
          (define key-comp-hi (comp (node-key node) hi))
          ; Variables to help with comprehension of comp.
          (define lo<key (> key-comp-lo 0))
          (define lo=key (= key-comp-lo 0))
          (define lo>key (< key-comp-lo 0))
          (define hi<key (> key-comp-hi 0))
          (define hi=key (= key-comp-hi 0))
          (define hi>key (< key-comp-hi 0))
          (define key (node-key node))
          (define info (node-info node))
          (define left (node-left node))
          (define right (node-right node))]
    (cond [(and lo<key hi>key) (make-node key info
                                          (bst-submap lo hi comp left)
                                          (bst-submap lo hi comp right))]
          [(and lo<key hi=key) (make-node key info (bst-submap lo hi comp left) (make-leaf))]
          [(and lo<key hi<key) (bst-submap lo hi comp left)]
          [(and lo=key hi>key) (make-node key info (make-leaf) (bst-submap lo hi comp right))]
          [(and lo=key hi=key) (make-node key info (make-leaf) (make-leaf))]
          [(and lo>key hi>key) (bst-submap lo hi comp right)])))

(check-expect (submap-node bst4 - 1 8) bst4)
(check-expect (submap-node bst4 - 1 4) (make-node 4 "" (make-node 3 "" bst0 bst0) bst0))
(check-expect (submap-node bst4 - 0 3) (make-node 3 "" bst0 bst0))
(check-expect (submap-node bst4 - 4 11) (make-node 4 "" bst0 (make-node 7 "" bst0 bst0)))
(check-expect (submap-node bst4 - 4 4) (make-node 4 "" bst0 bst0))
(check-expect (submap-node bst4 - 5 7) (make-node 7 "" bst0 bst0))

; EXERCISE 6
; ==========

; A SExpr is one of:
; - Symbol
; - [List-of SExpr]

#;(define (sexp-temp sexp)
    (cond [(symbol? sexp) ...]
          [else (... (ls-temp sexp) ...)]))

#;(define (ls-temp ls)
    (cond [(empty? ls) ...]
          [(cons? ls) (... (sexp-temp (first ls)) ... (ls-temp (rest ls)) ...)]))
; Examples

(define sexp1 '())
(define sexp2 'a)
(define sexp3 'b)
(define sexp4 (list sexp2 sexp3))
(define sexp5 (list sexp3 sexp2))
(define sexp6 (list sexp4 sexp5))

; topsy-turvy : SExpr -> SExpr
; inverts the order of the given s-expression.
(define (topsy-turvy s)
  (cond [(symbol? s) s]
        [else (topsy-turvy-list s)]))

(check-expect (topsy-turvy sexp1) sexp1)
(check-expect (topsy-turvy sexp2) sexp2)
(check-expect (topsy-turvy sexp4) sexp5)

; topsy-turvy-list : SExpr -> SExpr
; inverts the order of the given s-expression (which we assume is a list).
(define (topsy-turvy-list ls)
  (cond [(empty? ls) '()]
        [(cons? ls) (reverse (cons (topsy-turvy (first ls))
                                   (topsy-turvy-list (rest ls))))]))

(check-expect (topsy-turvy-list sexp1) sexp1)
(check-expect (topsy-turvy-list sexp4) sexp5)
(check-expect (topsy-turvy-list sexp6) sexp6)

; find-path : SExpr Symbol -> [List-of Natural]
; finds the shortest path to s in sexp and outputs it using a list of indices.
(define (find-path sexp s)
  (foldl (λ (sexp-inner lon) (number-path sexp-inner lon s)) '() sexp))

; number-path : SExpr [List-of-Natural] Symbol -> [List-of-Natural]
(define (number-path sexp lon s)
  (cond [(symbol? sexp) (if (symbol=? sexp s)
                            lon
                            (cons (add1 (first lon)) (rest lon)))]
        [(empty? sexp) lon] ; TODO: Come back to this.
        [(cons? sexp)]))