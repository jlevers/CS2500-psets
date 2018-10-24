;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname 8b) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define-struct leaf)
(define-struct node [key info left right])
; A [BST X Y] is one of:
; - (make-leaf)
; - (make-node X Y [BST X Y] [BST X Y])
; Interpretation: A binary search tree containing keys of type X and info of type Y
; Examples:

(define bst0 (make-leaf))
(define bst1 (make-node 1 "one" bst0 (make-node 4 "four" bst0 bst0)))
(define bst2 (make-node "lmn" 12
                        (make-node "def" 22 bst0 bst0)
                        (make-node "rst" 11 bst0 bst0)))
(define bst3 (make-node "5long" 8
                        (make-node "4lon" 5
                                   (make-node "2l" 22 bst0 bst0)
                                   bst0)
                        (make-node "9longaaaa" 34 bst0 bst0)))
; Template:
#;(define (bst-temp bst)
  (cond
    [(leaf? bst)]
    [(node? bst) ... (node-key bst)
                 ... (node-info bst)
                 ... (bst-temp (node-left bst))
                 ... (bst-temp (node-right bst))]))

(define-struct tree-map [tree comp])
; A [TreeMap X Y] is a (make-treemap [BST X Y] [Comparison X])
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
  (make-tree-map (tree-map-comp tm) (insert-bst key val tm (tree-map-comp tm))))

(check-expect (insert -1 "none" bst1)
              (make-node 1 "one"
                         (make-node -1 "none" bst0 bst0)
                         (make-node 4 "four" bst0 bst0)))
(check-expect (insert "12looooooong" 89 bst3)
              (make-node "5long" 8
                        (make-node "4lon" 5
                                   (make-node "2l" 22 bst0 bst0)
                                   bst0)
                        (make-node "9longaaaa" 34
                                   bst0
                                   (make-node "12looooooong" 89 bst0 bst0))))

; insert-bst : X Y [Comparison X] [BST X Y] -> [BST X Y]
; Inserts the X Y key-value pair into the given BST, replacing if the key already exists
(define (insert-bst key val comp bst)
  (cond
    [(leaf? bst) (make-node key val (make-leaf) (make-leaf))]
    [(node? bst) (local [(define comparison (comp key (node-key bst)))]
                   [(< comparison 0) (insert-bst (node-left bst))]
                   [(> comparison 0) (insert-bst (node-right bst))]
                   [(zero? comparison) (make-node key val
                                                  (node-left bst)
                                                  (node-right bst))])]))