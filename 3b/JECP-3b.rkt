;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname JECP-3b) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp")) #f)))
;; DEFINITIONS FROM 3A

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
(define ds2 (make-launch 0 (make-interval 50 100)))
(define ds3 (make-flight 100 (make-interval 20 200) (make-posn 10 20)))
(define ds4 (make-flight 12 (make-interval 190 210) (make-posn 0 0)))
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

(check-expect (falling-drone ds1) ds1) ;There should be no change as there is no drone
(check-expect (falling-drone ds3) (make-flight 100 (make-interval 20 200) (make-posn 10 19)))
(check-expect (falling-drone ds4) ds4) ;No change, as the drone is already on the ground.


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
 (make-flight 0 (make-interval 50 100) (make-posn 15 20)))
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


;; EXERCISES 2-4

(define HEIGHT 100)
(define WIDTH 600)
(define PHOTOG-HEIGHT 20)
(define GOAL-HEIGHT 4)
(define DRONE-HEIGHT 10)
(define SCENE (empty-scene WIDTH HEIGHT))
(define PHOTOG (rectangle 5 PHOTOG-HEIGHT "solid" "chartreuse"))
(define DRONE (rectangle 20 DRONE-HEIGHT "solid" "black"))

; draw-ds : DS -> Image
; Makes an image of the current drone flying state.
(define (draw-ds ds)
  (cond
    [(launch? ds) (underlay/xy (underlay/xy SCENE
                                            (launch-photographer ds)
                                            (- HEIGHT PHOTOG-HEIGHT)
                                            PHOTOG)
                               (interval-left (launch-goal ds))
                               (- HEIGHT GOAL-HEIGHT)
                               (goal (launch-goal ds)))]
    [(flight? ds) (underlay/xy (underlay/xy (underlay/xy SCENE
                                                         (flight-photographer ds)
                                                         (- HEIGHT PHOTOG-HEIGHT)
                                                         PHOTOG)
                                            (interval-left (flight-goal ds))
                                            (- HEIGHT GOAL-HEIGHT)
                                            (goal (flight-goal ds)))
                               (posn-x (flight-drone ds))
                               (- HEIGHT (posn-y (flight-drone ds)) DRONE-HEIGHT)
                               DRONE)]))

(check-expect (draw-ds ds1) (underlay/xy (underlay/xy SCENE
                                                      3
                                                      (- HEIGHT PHOTOG-HEIGHT)
                                                      PHOTOG)
                                         12
                                         (- HEIGHT GOAL-HEIGHT)
                                         (rectangle 12 GOAL-HEIGHT "solid" "gold")))
(check-expect (draw-ds ds3) (underlay/xy (underlay/xy (underlay/xy SCENE
                                                                   100
                                                                   (- HEIGHT PHOTOG-HEIGHT)
                                                                   PHOTOG)
                                                      20
                                                      (- HEIGHT GOAL-HEIGHT)
                                                      (rectangle 180 GOAL-HEIGHT "solid" "gold"))
                                         10
                                         (- HEIGHT 20 DRONE-HEIGHT)
                                         DRONE))


; goal : Interval -> Image
; Creates a visual representation of the goal described by the interval.
(define (goal interval)
  (rectangle (- (interval-right interval) (interval-left interval)) GOAL-HEIGHT "solid" "gold"))

(check-expect (goal (make-interval 2.2 4.2)) (rectangle 2 4 "solid" "gold"))
(check-expect (goal (make-interval -3 2)) (rectangle 5 4 "solid" "gold"))


; user-input : DS KeyEvent -> DS
; Launches or moves the drone based on keyboard input from the user.
(define (user-input ds ke)
  (cond
    [(and (launch? ds) (string=? "l" ke)) (launch-drone ds)]
    [(and (flight? ds) (string=? "up" ke)) (make-flight (flight-photographer ds)
                                                        (flight-goal ds)
                                                        (make-posn
                                                         (posn-x (flight-drone ds))
                                                         (+ (posn-y (flight-drone ds)) 5)))]
    [(and (flight? ds) (string=? "left" ke)) (make-flight (flight-photographer ds)
                                                          (flight-goal ds)
                                                          (make-posn
                                                           (- (posn-x (flight-drone ds)) 5)
                                                           (posn-y (flight-drone ds))))]
    [(and (flight? ds) (string=? "right" ke)) (make-flight (flight-photographer ds)
                                                           (flight-goal ds)
                                                           (make-posn
                                                            (+ (posn-x (flight-drone ds)) 5)
                                                            (posn-y (flight-drone ds))))]
    [else ds]))

(check-expect (user-input ds1 "l") (make-flight 3 (make-interval 12 24) (make-posn (+ 3 15) 20)))
(check-expect (user-input ds3 "up") (make-flight 100 (make-interval 20 200) (make-posn 10 25)))
(check-expect (user-input ds3 "right") (make-flight 100 (make-interval 20 200) (make-posn 15 20)))
(check-expect (user-input ds3 "left") (make-flight 100 (make-interval 20 200) (make-posn 5 20)))
(check-expect (user-input ds1 "left") ds1)


; main : DS -> DS
; Runs a drone photography game and outputs whether or not it was a success.
(define (main ds)
  (big-bang ds
    [on-tick falling-drone .1]
    [to-draw draw-ds]
    [on-key user-input]
    [stop-when shoot-over?])) ;Hey Jesse -- Can't figure out how to make this return a boolean.
                              ;Also, do we have to write tests for this?

