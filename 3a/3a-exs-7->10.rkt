;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname 3a-exs-7->10) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp")) #f)))
;; EXERCISE 7-10

; An Interval is a (make-interval Number Number)
(define-struct interval [left right])
; and represents the leftmost and rightmost range of an interval in pixel coordinates (inclusive)
; Examples:
(define i1 (make-interval 1 4))
(define i2 (make-interval 2 2))
(define i3 (make-interval 0 12))
; Template:
#;(define (interval-temp i)
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
         (... (launch-photographer ds)
          ... (interval-temp (launch-goal ds)) ...)]
    [(flight? ds)
         (... (flight-photographer ds) ...
          ... (interval-temp (flight-goal ds)) ...
          ... (flight-drone ds) ...)]))


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

(check-expect
 (launch-drone ds1)
 (make-flight 3 (make-interval 12 24) (make-posn 18 20)))
(check-expect
 (launch-drone ds2)
 (make-flight 0 (make-interval 1 2) (make-posn 15 20)))
(check-expect
 (launch-drone ds3)
 ds3)


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

(check-expect
 (shoot-over? (make-flight 2 (make-interval 10 20) (make-posn 5 5)))
 #false)
(check-expect
 (shoot-over? ds1)
 #false) ; drone is not in flight
(check-expect
 (shoot-over? (make-flight 15 (make-interval 7 13) (make-posn 12 0)))
 #true)  ; drone crashed
(check-expect
 (shoot-over? (make-flight 0 (make-interval 10 20) (make-posn 15 10)))
 #true)
(check-expect
 (shoot-over? (make-flight 31 (make-interval 2 30) (make-posn 12 38)))
 #true) ; shooting goal plus extra