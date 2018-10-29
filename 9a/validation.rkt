;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname validation) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define-struct tnt [timer])

(define MAT-GRASS 'grass)
(define MAT-WATER 'water)
(define MAT-ROCK 'rock)
(define MAT-WOOD 'wood)
(define MAT-GOLD 'gold)
(define MAT-TNT (make-tnt 30))
(define SYMBOL-DIR-LIST (list 'up 'down 'left 'right))


(define GRID-NOT-CONS "Expected grid to be a list of cells, received empty list or non list.")
(define WRONG-LENGTH-CELL "Expected cell to be list of length 3, received wrong length list.")
(define CELL-NOT-LIST "Expected cell to be a list, received different kind of data.")

; valid-grid? : SExpr -> Boolean
; Checks if the S-expression version of the grid is valid
(define (valid-grid? sexpr)
  (if (cons? sexpr)
      (andmap valid-cell? sexpr)
      (error GRID-NOT-CONS)))

(check-expect (valid-grid? '(((0 0) (grass (tnt 22)) ())
                            ((1 0) () ((0 left)))
                            ((0 1) (rock gold) ((1 up)))
                            ((1 1) (water) ())))
              #true)
(check-expect (valid-grid?'(((0 0) () ((0 down) (1 right)))))
              #true)
(check-expect (valid-grid? '(((0 0) () ())
                            ((1 0) () ())
                            ((0 1) () ())
                            ((1 1) () ())))
              #true)
(check-error (valid-grid? 'fake) GRID-NOT-CONS)


; valid-cell? : SExpr -> Boolean
; Checks if the S-expression is a valid cell.
(define (valid-cell? sexpr)
  (cond [(not (cons? sexpr)) (error CELL-NOT-LIST)]
        [(not (= (length sexpr) 3)) (error WRONG-LENGTH-CELL)]
        [(and (valid-posn-sexpr? (first sexpr))
              (valid-materials-sexpr? (second sexpr))
              (valid-players-sexpr? (third sexpr))) #t]))

(check-expect (valid-cell? '((0 0) () ())) #true)
(check-expect (valid-cell? '((0 1) (rock gold) ((1 up)))) #true)
(check-error (valid-cell? '((0 2))) WRONG-LENGTH-CELL)
(check-error (valid-cell? 'hello) CELL-NOT-LIST)

(define WRONG-LENGTH-POSN "Expected posn S-expression to be of length 2, was wrong length.")
(define POSN-NOT-CONS "Expected posn S-expression to be a cons, received something else.")
(define POSN-OUT-OF-BOUNDS "Given posn was out of bounds.")

; valid-posn-sexpr? : SExpr -> Boolean
; Checks if the S-expression is a valid posn
(define (valid-posn-sexpr? sexp)
  (cond [(not (cons? sexp)) (error POSN-NOT-CONS)]
        [(not (= (length sexp) 2)) (error WRONG-LENGTH-POSN)]
        [(not (and (<= 0 (first sexp) 19) (<= 0 (second sexp) 19))) (error POSN-OUT-OF-BOUNDS)]
        [else #true]))

(check-expect (valid-posn-sexpr? (list 0 0)) #true)
(check-error (valid-posn-sexpr? (list 0)) WRONG-LENGTH-POSN)
(check-error (valid-posn-sexpr? 'test) POSN-NOT-CONS)
(check-error (valid-posn-sexpr? (list -10 0)) POSN-OUT-OF-BOUNDS)


(define MATERIALS-NOT-LIST "Expected materials to be in a list, received something else.")

; valid-materials-list? : SExpr -> Boolean
; Checks if the S-expression is a valid list of materials
(define (valid-materials-sexpr? sexp)
  (cond [(not (list? sexp)) (error MATERIALS-NOT-LIST)]
        [(and (andmap valid-material? sexp) (valid-order? sexp)) #t]))

(check-expect (valid-materials-sexpr? '(rock gold)) #true)
(check-expect (valid-materials-sexpr? '(grass (tnt 22))) #true)
(check-expect (valid-materials-sexpr? '(rock rock)) #true)
(check-expect (valid-materials-sexpr? '()) #true)
(check-error (valid-materials-sexpr? 'test) MATERIALS-NOT-LIST)

(define MATERIAL-NOT-VALID "Given an invalid material.")
(define TNT-NOT-CONS "Expected TNT to be a cons, received something else.")
(define TNT-WRONG-LENGTH "Expected TNT to be of length 2, received list of wrong length.")
(define TNT-WRONG-SYMBOL "Expected TNT list to have symbol 'tnt as first element.")
(define TNT-INVALID-TIMER "Expected TNT timer to be a natural. IT WASN'T....")

; valid-material? : SExpr -> Boolean
; Checks if the material in the SExpr is valid
(define (valid-material? sexp)
  (local [; valid-tnt? : SExpr -> Boolean
          ; Checks if the S-expression is a valid TNT
          (define (valid-tnt? s)
            (cond [(not (cons? s)) (error TNT-NOT-CONS)]
                  [(not (= (length s) 2)) (error TNT-WRONG-LENGTH)]
                  [(not (symbol=? (first s) 'tnt)) (error TNT-WRONG-SYMBOL)]
                  [(not (>= (second s) 0)) (error TNT-INVALID-TIMER)]
                  [else #true]))]
  (if (symbol? sexp)
      (cond
        [(symbol=? sexp MAT-GRASS) #true]
        [(symbol=? sexp MAT-WATER) #true]
        [(symbol=? sexp MAT-ROCK) #true]
        [(symbol=? sexp MAT-WOOD) #true]
        [(symbol=? sexp MAT-GOLD) #true]
        [else (error MATERIAL-NOT-VALID)])
      (valid-tnt? sexp))))

(check-expect (valid-material? 'gold) #true)
(check-expect (valid-material? 'wood) #true)
(check-expect (valid-material? 'grass) #true)
(check-expect (valid-material? 'rock) #true)
(check-expect (valid-material? 'water) #true)
(check-expect (valid-material? '(tnt 22)) #true)
(check-error (valid-material? 'asdf) MATERIAL-NOT-VALID)

(define INVALID-MATERIAL-ORDER "The materials in a cell were in an invalid order.")

; valid-order? : SExpr -> Boolean
; Checks if the materials are in a valid order
(define (valid-order? sexp)
  (cond [(< (length sexp) 2) #true]
        [(>= (length sexp) 2)
         (if (or (material-type=? MAT-ROCK (second sexp))
                 (material-type=? MAT-WATER (second sexp)))
             (if (not (material-type=? MAT-ROCK (first sexp)))
                 (error INVALID-MATERIAL-ORDER)
                 #true)
             #true)]))

(check-expect (valid-order? '(rock water grass)) #true)
(check-expect (valid-order? '(rock rock)) #true)
(check-expect (valid-order? '(grass)) #true)
(check-error (valid-order? '(grass water)) INVALID-MATERIAL-ORDER)
(check-expect (valid-order? '(grass rock)) INVALID-MATERIAL-ORDER)

(define MATERIAL-MISMATCH "Expected both materials to either both cons or both identical symbols")
; material-type=? : SExpr SExpr -> Boolean
; Checks if two S-expression material types are equal
; TODO Are we checking that it's one of the appropriate matieral symbols?
(define (material-type=? s1 s2)
  (cond [(and (cons? s1) (cons? s2)) #true]
        [(or (cons? s1) (cons? s2)) (error MATERIAL-MISMATCH)]
        [(symbol=? s1 s2) #true]
        [else (error MATERIAL-MISMATCH)]))

(check-expect (material-type=? 'rock 'rock) #true)
(check-expect (material-type=? 'rock 'grass) #true)
(check-expect (material-type=? 'rock '(tnt 30)) #false)
(check-expect (material-type=? '(tnt 30) '(tnt 25)) #true)


(define PLAYERS-NOT-LIST "List of players should be a list, received something else.")
(define PLAYERS-CATCH-ALL "Unknown error occured while checking if players were valid.")

; valid-players-sexpr? : SExpr -> Boolean
; Checks if the list of players is valid
(define (valid-players-sexpr? sexp)
  (cond [(not (list? sexp)) (error PLAYERS-NOT-LIST)]
        [(andmap valid-player? sexp) #true]
        [else (error PLAYERS-CATCH-ALL)]))

(check-expect (valid-players-sexpr? '()) #true)
(check-error (valid-players-sexpr? 'test) PLAYERS-NOT-LIST)


(define PLAYER-NOT-CONS "Expected player to be a cons, received something else.")
(define PLAYER-WRONG-LENGTH "Expected player to have 2 elements, recieved different length.")
(define PLAYER-INVALID-ID "Expected player's ID to be a natural number, was something else.")
(define PLAYER-INVALID-DIRECTION "Expected player's direction to be 'up/'down/'left/'right.")

; valid-player? : SExpr -> Boolean
; Checks if the player S-expression is valid
(define (valid-player? sexp)
  (cond [(not (cons? sexp)) (error PLAYER-NOT-CONS)]
        [(not (= (length sexp) 2)) (error PLAYER-WRONG-LENGTH)]
        [(or (not (integer? (first sexp))) (not (>= (first sexp) 0))) (error PLAYER-INVALID-ID)]
        [(not (member? (second sexp) SYMBOL-DIR-LIST)) (error PLAYER-INVALID-DIRECTION)]
        [else #true]))

 (check-expect (valid-player? '(1 'up)) #true)
(check-error (valid-player? '()) PLAYER-NOT-CONS)
(check-error (valid-player? '(1)) PLAYER-WRONG-LENGTH)
(check-error (valid-player? '(3.5 'left)) PLAYER-INVALID-ID)
(check-error (valid-player? '(1 'asdf)) PLAYER-INVALID-DIRECTION)