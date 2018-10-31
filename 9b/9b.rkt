;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname 9b) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; A Bool is a [X X -> X]

(define BTRUE (λ (x1 x2) x1))
(define BFALSE (λ (x1 x2) x2))

; bool->boolean : Bool -> Boolean
; Converts a bool to a boolean
(define (bool->boolean b)
  (b #true #false))

(check-expect (bool->boolean BTRUE) #true)
(check-expect (bool->boolean BFALSE) #false)

; and/bool : Bool Bool -> Bool
; Functions analogously to and
(define (and/bool b1 b2)
  (λ (x1 x2) (b1 (b2 x1 x2) (b1 x1 x2))))

(check-expect (bool->boolean (and/bool BTRUE BTRUE)) #true)
(check-expect (bool->boolean (and/bool BTRUE BFALSE)) #false)
(check-expect (bool->boolean (and/bool BFALSE BTRUE)) #false)
(check-expect (bool->boolean (and/bool BFALSE BFALSE)) #false)

; or/bool : Bool Bool -> Bool
; Functions analogously to or
(define (or/bool b1 b2)
  (λ (x1 x2) (b1 (b1 x1 x2) (b2 x1 x2))))

(check-expect (bool->boolean (or/bool BTRUE BTRUE)) #true)
(check-expect (bool->boolean (or/bool BTRUE BFALSE)) #true)
(check-expect (bool->boolean (or/bool BFALSE BTRUE)) #true)
(check-expect (bool->boolean (or/bool BFALSE BFALSE)) #false)

; not/bool : Bool Bool -> Bool
; Functions analogously to not
(define (not/bool b1)
  (λ (x1 x2) (b1 x2 x1)))

(check-expect (bool->boolean (not/bool BFALSE)) #true)
(check-expect (bool->boolean (not/bool BTRUE)) #false)


; A LeafyTree is one of:
; - 'leaf
; - (make-node LeafyTree LeafyTree)
(define-struct node [l r])
; Examples:
(define T0 'leaf)
(define T1 (make-node T0 T0))
(define T2 (make-node T1 T0))
(define T3 (make-node T1 T2))
; Template
(define (leafy-temp lt)
  (cond
    [(node? lt) (... (leafy-temp (node-l lt)) ... (leafy-temp (node-r lt)) ...)]
    [(symbol? lt) ...]))

; height: LeafyTree -> Number
; Determines the height of a given leafy-tree
(define (height lt)
  (cond
    [(node? lt) (max (add1 (height (node-l lt))) (add1 (height (node-r lt))))]
    [(symbol? lt) 0]))
(check-expect (height T0) 0)
(check-expect (height T1) 1)
(check-expect (height T2) 2)
(check-expect (height T3) 3)

; all-leafy-trees : Natural -> [List-of LeafyTree]
; Given a natural number n, outputs the list of all leafy trees of height n
(define (all-leafy-trees n)
  (all-leafy T0 n 0))

; all-leafy-trees-iter : LeafyTree Natural Natural -> [List-of LeafyTree]
; Given a goal height n and a current height h, outputs the next three LeafyTrees.
(define (all-leafy-trees-iter lt n h)
  (if (= n h)
      lt
      (cond [(node? lt) (make-node (all-leafy-trees-iter lt n (add1 h)) (all-leafy-trees-iter lt n (add1 h)))]
            [(symbol? lt) (all-leafy-trees-iter (make-node 'leaf 'leaf) n (add1 h))])))

(define (all-leafy lt n h)
  (local
    [(define (expand-tree _)
       (cond
         [(node? lt) (list (make-node 'leaf lt) (make-node lt 'leaf) (make-node lt lt))]
         [(symbol? lt) (list (make-node 'leaf 'leaf))]))]
  (if (= n h)
      (list lt)
      (map (λ (x) (all-leafy x n (add1 h))) (expand-tree 1)))))

(check-expect (all-leafy-trees 0) (list T0))
(check-expect (all-leafy-trees 1) (list T1))
(check-expect (all-leafy-trees 2) (list (make-node T0 T1) (make-node T1 T0) (make-node T1 T1)))
(check-expect (length (all-leafy-trees 3)) 21)
;(check-expect (length (all-leafy-trees 4)) 651)


(list
 (list
  (list
   (list (make-node 'leaf
                    (make-node 'leaf (make-node 'leaf 'leaf))))
   (list (make-node (make-node 'leaf
                               (make-node 'leaf 'leaf))
                    'leaf))
   (list (make-node (make-node 'leaf
                               (make-node 'leaf 'leaf))
                    (make-node 'leaf
                               (make-node 'leaf
                                          'leaf)))))
  (list
   (list (make-node 'leaf (make-node (make-node 'leaf 'leaf) 'leaf)))
   (list (make-node (make-node (make-node 'leaf 'leaf) 'leaf) 'leaf))
   (list (make-node (make-node (make-node 'leaf 'leaf) 'leaf) (make-node (make-node 'leaf 'leaf) 'leaf))))
  (list
   (list (make-node 'leaf (make-node (make-node 'leaf 'leaf) (make-node 'leaf 'leaf))))
   (list (make-node (make-node (make-node 'leaf 'leaf) (make-node 'leaf 'leaf)) 'leaf))
   (list (make-node (make-node (make-node 'leaf 'leaf) (make-node 'leaf 'leaf)) (make-node (make-node 'leaf 'leaf) (make-node 'leaf 'leaf)))))))


