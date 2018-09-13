;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname 3a-exs-2->5) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp")) #f)))
;; EXERCISES 2-5

; A CoinOrFalse is one of:
; - #false
; - Number
; Interpretation: represents either no coin or the coin's monetary value.
; Example
(define COF-FALSE #false)
(define cofcoin1 .05)
(define cofcoin2 .25)
; Template
#;(define (cof-temp cof)
  (cond [(boolean? cof) ...]
        [(number? cof) ...]))
 
; A Guess is one of:
; - "left"
; - "middle"
; - "right
; Interpretation: A player's guess in a CoinShuffleGame.
; Example:
(define G-LEFT "left")
(define G-MIDDLE "middle")
(define G-RIGHT "right")
; Template:
#;(define (guess-temp g)
  (cond [(string=? g G-LEFT) ...]
        [(string=? g G-MIDDLE) ...]
        [(string=? g G-RIGHT) ...]))

; A CSG (CoinShuffleGame) is a (make-csg CoinOrFalse CoinOrFalse CoinOrFalse).
(define-struct csg [left middle right])
; Interpretation: represents the three cups in a coin shuffle game and what is under them.
; Examples:
(define csg1 (make-csg cofcoin1 cofcoin2 COF-FALSE))
(define csg2 (make-csg COF-FALSE cofcoin1 cofcoin2))
(define csg3 (make-csg cofcoin2 COF-FALSE cofcoin1))
(define csg4 (make-csg COF-FALSE cofcoin1 COF-FALSE))
; Template:
#;(define (csg-temp game)
  ((... csg-left game ...)
   (... csg-middle game ...)
   (... csg-right game ...)))

; shuffle-right : CSG -> CSG
; Interpreation: Moves cups in a coin shuffle game to the right, looping the rightmost member back
; around to the leftmost position.
(define (shuffle-right csg)
  (make-csg
    (csg-right csg)
    (csg-left csg)
    (csg-middle csg)))

(check-expect (shuffle-right csg1) csg2)
(check-expect (shuffle-right csg2) csg3)
(check-expect (shuffle-right csg3) csg1)

; cup-value : CSG Guess -> Number
; Interpretation: Returns the value of a guessed cup in a CoinShuffleGame.
(define (cup-value csg guess)
  (cond [(string=? guess G-LEFT)
         (if (number? (csg-left csg)) (csg-left csg) 0)]
        [(string=? guess G-MIDDLE)
         (if (number? (csg-middle csg)) (csg-middle csg) 0)]
        [(string=? guess G-RIGHT)
         (if (number? (csg-right csg)) (csg-right csg) 0)]))

(check-expect (cup-value csg1 "left") .05)
(check-expect (cup-value csg1 "right") 0)
(check-expect (cup-value csg2 "middle") .05)
(check-expect (cup-value csg2 "left") 0)
(check-expect (cup-value csg3 "right") .05)
(check-expect (cup-value csg3 "middle") 0)

; inflation : CSG Number -> CSG
; Interpretation: Adds value to all coins in the cup, leaving the empty coins as is.
(define (inflation csg num)
  (make-csg (if (number? (csg-left csg)) (+ (csg-left csg) num) #false)
            (if (number? (csg-middle csg)) (+ (csg-middle csg) num) #false)
            (if (number? (csg-right csg)) (+ (csg-right csg) num) #false)))

(check-expect (inflation csg1 3) (make-csg (+ cofcoin1 3) (+ cofcoin2 3) COF-FALSE))
(check-expect (inflation csg2 4.2) (make-csg COF-FALSE (+ cofcoin1 4.2) (+ cofcoin2 4.2)))
(check-expect (inflation csg3 73) (make-csg (+ cofcoin2 73) COF-FALSE (+ cofcoin1 73)))