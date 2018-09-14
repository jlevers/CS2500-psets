;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname JECP-HW3a) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp")) #f)))
;;; JESSE EVERS AND CASEY PANCOAST
;;; HOMEWORK 3A


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
; Interpretation: represents the three cups in a coin shuffle game, and what is under them.
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
; Interpretation: Adds value `num` to cups with coins, leaving the empty cups as is.
(define (inflation csg num)
  (make-csg
   (if (number? (csg-left csg)) (+ (csg-left csg) num) #false)
   (if (number? (csg-middle csg)) (+ (csg-middle csg) num) #false)
   (if (number? (csg-right csg)) (+ (csg-right csg) num) #false)))

(check-expect (inflation csg1 3) (make-csg (+ cofcoin1 3) (+ cofcoin2 3) COF-FALSE))
(check-expect (inflation csg2 4.2) (make-csg COF-FALSE (+ cofcoin1 4.2) (+ cofcoin2 4.2)))
(check-expect (inflation csg3 73) (make-csg (+ cofcoin2 73) COF-FALSE (+ cofcoin1 73)))


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

; add-posns : Posn Posn -> Posn
; adds the x and y values of p1 and p2 together to make a new Posn.
(define (add-posns p1 p2)
  (make-posn (+ (posn-x p1) (posn-x p2)) (+ (posn-y p1) (posn-y p2))))

(check-expect (add-posns POSN-20 POSN-1) (make-posn 21 21))
(check-expect (add-posns POSN-3-4 POSN-3-4) (make-posn 6 8))
(check-expect (add-posns POSN-0 POSN-20) (make-posn 20 20))


;; EXERCISE 7-10

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
#;(define (drone-shoot-temp ds)
  (cond
    [(launch? ds)
     (... (launch-photographer ds) ... (interval-temp (launch-goal ds)) ...)]
    [(flight? ds)
     (... (flight-photographer ds) ... (interval-temp (flight-goal ds)) ... (flight-drone ds) ...)]))


; falling-drone : DS -> DS
; if there's a drone in ds, move it down by 1px.
(define (falling-drone ds)
  (if (flight? ds)
      (make-flight
       (flight-photographer ds)
       (flight-goal ds)
       (make-posn
        (posn-x (flight-drone ds))
        (if (> (posn-y (flight-drone ds)) 0)
            (- (posn-y (flight-drone ds)) 1)
            (posn-y (flight-drone ds)))))
      ds))

(check-expect (falling-drone ds1) ds1)
(check-expect (falling-drone ds3) (make-flight 1 (make-interval 20 200) (make-posn 1 1)))
(check-expect (falling-drone ds4) ds4)


; launch-drone : DS -> DS
; if the drone isn't flying yet, start its flight 20px up and 15px right from the photographer.
(define (launch-drone ds)
  (if (launch? ds)
      (make-flight (launch-photographer ds)
                   (launch-goal ds)
                   (make-posn (+ (launch-photographer ds) 15) 20))
      ds))

(check-expect (launch-drone ds1) (make-flight 3 (make-interval 12 24) (make-posn 18 20)))
(check-expect (launch-drone ds2) (make-flight 0 (make-interval 1 2) (make-posn 15 20)))
(check-expect (launch-drone ds3) ds3)


; shoot-over? : DS -> Boolean
; checks if the drone has shot the whole goal (or the whole goal plus some extra), or has crashed.
(define (shoot-over? ds)
      (cond
        [(launch? ds) #false]
        [(= (posn-y (flight-drone ds)) 0) #true]
        [(and
          (<= (-
               (posn-x (flight-drone ds))
               (/ (posn-y (flight-drone ds)) 2))
              (interval-left (flight-goal ds)))
          (>= (+
               (posn-x (flight-drone ds))
               (/ (posn-y (flight-drone ds)) 2))
              (interval-right (flight-goal ds)))) #true]
        [else #false]))

(check-expect (shoot-over? (make-flight 2 (make-interval 10 20) (make-posn 5 5))) #false)
(check-expect (shoot-over? ds1) #false) ; drone is not in flight
(check-expect (shoot-over? (make-flight 15 (make-interval 7 13) (make-posn 12 0))) #true)  ; drone crashed
(check-expect (shoot-over? (make-flight 0 (make-interval 10 20) (make-posn 15 10))) #true)
(check-expect (shoot-over? (make-flight 31 (make-interval 2 30) (make-posn 12 38))) #true) ; shooting goal plus extra