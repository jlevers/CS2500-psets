;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname 8b) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define-struct leaf [])
(define-struct node [key info left right])
; A [BST X Y] is one of:
; - (make-leaf)
; - (make-node X Y [BST X Y] [BST X Y])
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
; Template:
#;(define (bst-temp bst)
    (cond
      [(leaf? bst) ...]
      [(node? bst) ... (node-key bst)
                   ... (node-info bst)
                   ... (bst-temp (node-left bst))
                   ... (bst-temp (node-right bst))]))

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
  
; insert-bst : X Y [Comparison X] [BST X Y] -> [BST X Y]
; Inserts the X Y key-value pair into the given BST, replacing if the key already exists
(define (insert-bst key info comp bst)
  (local [; new-node : [BST X Y] -> [BST X Y]
          (define (new-node bst)
            (cond [(> (comp key (node-key bst)) 0) (make-node (node-key bst)
                                                              (node-info bst)
                                                              (node-left bst)
                                                              (insert-bst key info comp
                                                                          (node-right bst)))]
                  [(< (comp key (node-key bst)) 0) (make-node (node-key bst)
                                                              (node-info bst)
                                                              (insert-bst key info comp
                                                                          (node-left bst))
                                                              (node-right bst))]
                  [(= (comp key (node-key bst)) 0) (make-node key info
                                                              (node-left bst)
                                                              (node-right bst))]))
          ]
    (cond
      [(leaf? bst) (make-node key info (make-leaf) (make-leaf))]
      [(node? bst) (new-node bst)])))

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

; insert : X Y [TreeMap X Y] -> [TreeMap X Y]
; Inserts the X Y key-value pair into the treemap, replacing if the key already exists.
(define (insert key val tm)
  (make-tree-map (insert-bst key val (tree-map-comp tm) (tree-map-tree tm)) (tree-map-comp tm)))

(check-expect (tree-map-tree (insert -1 "negative" tm1))
              (insert-bst -1 "negative" - bst1))