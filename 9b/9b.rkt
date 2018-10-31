;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname 9b) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; A Bool is a [X X -> X]

(define B1 (λ (x1 x2) x1))
(define B2 (λ (x1 x2) x2))

; bool->boolean : Bool -> Boolean
; Converts a bool to a boolean
(define (bool->boolean b)
  (b #true #false))


; and/bool : Bool Bool -> Bool
; Functions analogously to and
(define (and/bool b1 b2)
  (λ (x1 x2) (b1 (b2 x1 x2) (b1 x1 x2))))

; or/bool : Bool Bool -> Bool
; Functions analogously to or
(define (or/bool b1 b2)
  (λ (x1 x2) (b1 (b1 x1 x2) (b2 x1 x2))))

; not/bool : Bool Bool -> Bool
; Functions analogously to not
(define (not/bool b1)
  (λ (x1 x2) (b1 x2 x1)))


#|
B1
B1
x1

B1
B2
x1

B2
B1
x1

B2
B2
x2
|#

