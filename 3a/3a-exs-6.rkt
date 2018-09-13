;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname 3a-exs-6) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp")) #f)))
;; EXERCISE 6

; A Posn is a (make-posn Number Number)
; Interpretation: Represents a 2-D coordinate.
; Examples:
(define POSN-20 (make-posn 20 20))
(define POSN-1.1 (make-posn 1.1 1.1))
(define POSN-n3-4 (make-posn -3 4))
(define POSN-0 (make-posn 0 0))
; Template:
(define (posn-temp p)
  (... (posn-x p) ... (posn-y p)))

; add-posns: Posn Posn -> Posn
; Adds the x and y values of two Posns together to make a new Posn.
(define (add-posns p1 p2)
  (make-posn (+ (posn-x p1) (posn-x p2)) (+ (posn-y p1) (posn-y p2))))

(check-expect (add-posns POSN-20 POSN-1.1) (make-posn 21.1 21.1))
(check-expect (add-posns POSN-n3-4 POSN-n3-4) (make-posn -6 8))
(check-expect (add-posns POSN-0 POSN-20) (make-posn 20 20))