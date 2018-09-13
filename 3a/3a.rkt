;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname 3a) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp")) #f)))
; A Posn is a (make-posn Number Number)
; and represents a 2d coordinate
 
(define POSN-20 (make-posn 20 20))
(define POSN-1 (make-posn 1 1))
(define POSN-3-4 (make-posn 3 4))
(define POSN-0 (make-posn 0 0))
 
; posn-temp : Posn -> ?
(define (posn-temp p)
  (... (posn-x p) ... (posn-y p)))



;; EXERCISE 6

; add-posns: Posn Posn -> Posn
; adds the x and y values of p1 and p2 together to make a new Posn.
(define (add-posns p1 p2)
  (make-posn (+ (posn-x p1) (posn-x p2)) (+ (posn-y p1) (posn-y p2))))

(check-expect (add-posns POSN-20 POSN-1) (make-posn 21 21))
(check-expect (add-posns POSN-3-4 POSN-3-4) (make-posn 6 8))
(check-expect (add-posns POSN-0 POSN-20) (make-posn 20 20))


;; EXERCISE 7-10

; A DS (Drone Shoot) is one of:
; - (make-launch Number Interval)
(define-struct launch [photographer goal])
; - (make-flight Number Interval Posn)
(define-struct flight [photographer goal drone])
; Where:
;   - photographer represents a photographer's x-coordinate on the ground
;   - goal represents the range of the desired image to to be captured at the ground-level
;   - drone (if any) represents the drone's x/y position (with 0, 0 at the bottom left)
; All numbers are measured in pixels.
; Examples:
(define ds1 (make-launch 3 (make-interval 12 24)))
(define ds2 (make-launch 0 (make-interval 1 2)))
(define ds3 (make-flight 1 (make-interval 20 200) (make-posn 1 2)))
(define ds4 (make-flight 12 (make-interval 10 15) (make-posn 3 0)))
; Template
(define (drone-shoot-temp ds)
  (cond
    [(launch? ds)
     (... (launch-photographer ds) ... (launch-goal ds) ...)]
    [(flight? ds)
     (... (flight-photographer ds) ... (flight-goal ds) ... (flight-drone ds) ...)]))
 
; An Interval is a (make-interval Number Number)
(define-struct interval [left right])
; and represents the leftmost and rightmost range of an interval in pixel coordinates (inclusive)
; Examples:
(define i1 (make-interval 1 4))
(define i2 (make-interval 2 2))
(define i3 (make-interval 0 12))
; Template:
(define (interval-temp i)
  (... (interval-left i) ... (interval-right i) ...))

; falling-drone: DS -> DS
; if there's a drone in ds, move it down by 1px.
(define (falling-drone ds)
  ...)

(check-expect (falling-drone ds1) ds1)
(check-expect (falling-drone ds3) (make-flight 1 (make-interval 20 200) (make-posn 0 2)))
(check-expect (falling-drone ds4) ds4)