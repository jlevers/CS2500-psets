;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname 5a) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;;;;;;;;;;;;;;;;;;
; Problem Set 5a ;
;;;;;;;;;;;;;;;;;;

(require 2htdp/image)
(require 2htdp/universe)


; Exercises 2 & 3
; ---------------

;;;;; DATA DEFINITIONS ;;;;;

; A GridPosn is a (make-posn Natural Natural)
; Interpretation
; - the first field is the player's X coordinate in grid units.
; - the second field is the player's Y coordinate in grid units.
; Examples
(define gp0 (make-posn 0 0))
(define gp1 (make-posn 15 20))
; Template
#;(define (gridposn-temp gp)
    (... (posn-x gp) ... (posn-y gp) ...))


; A Direction is one of:
; - "UP"
; - "DOWN"
; - "LEFT"
; - "RIGHT"
; Interpretation: A direction on screen where UP is towards the top of the screen.
; Examples
(define DIR-UP "UP")
(define DIR-DOWN "DOWN")
(define DIR-LEFT "LEFT")
(define DIR-RIGHT "RIGHT")
; Template
#;(define (direction-temp dir)
    (cond
      [(string=? dir "UP") ...]
      [(string=? dir "DOWN") ...]
      [(string=? dir "LEFT") ...]
      [(string=? dir "RIGHT") ...]))


; A TNT is a Natural in the range [0,TNT-FUSE].
; Interpretation: A block of TNT in the game that explodes after counting down the number.
; Examples
(define tnt0 0)
(define tnt1 15)
; Template
#;(define (tnt-temp tnt)
    (... tnt ...))


; A Block is one of:
; - "Water"
; - "Grass"
; - "Rock"
; - "Gold"
; - "Wood"
; - A TNT
; Interpretation: A block in the game of different material types.
; Examples
(define BLOCK-WA "Water")
(define BLOCK-GR "Grass")
(define BLOCK-RO "Rock")
(define BLOCK-GO "Gold")
(define BLOCK-WO "Wood")
(define BLOCK-TNT tnt1)
; Template
#;(define (block-temp b)
    (cond
      [(number? b) (tnt-temp b)]
      [(string=? b "Water") ...]
      [(string=? b "Grass") ...]
      [(string=? b "Rock") ...]
      [(string=? b "Gold") ...]
      [(string=? b "Wood") ...]))


(define-struct player [location direction selected score])
; A Player is a (make-player GridPosn Direction Block Natural).
; Interpretation
; - the first field is the player's position on the grid.
; - the second field is the player's front-facing direction.
; - the third field is the material that the player currently has selected.
; - the fourth field is the player's score.
; Examples
(define player1 (make-player gp0 DIR-RIGHT "Wood" 0))
(define player2 (make-player gp1 DIR-LEFT "Rock" 5))
; Template
#;(define (player-temp player)
    (... (gridposn-temp (player-location player)) ...
         (direction-temp (player-direction player)) ...
         (block-temp (player-selected player)) ...
         (player-score player) ...))


; A Cell is one of:
; - '()
; - (cons Block Cell)
; Interpretation: A cell in a grid row that contains a stack of Blocks.
; Examples
(define cell0 '())
(define initial-cell (list BLOCK-WA))
(define user-cell (list BLOCK-RO BLOCK-WA))
; Template
#;(define (cell-temp c)
    (cond
      [(empty? c) ...]
      [(cons? c) (... (block-temp (first c)) ... (cell-temp (rest c)) ...)]))


; A GridRow is one of:
; - '()
; - (cons Cell GridRow)
; Interpretation: A row of cells that the player plays on.
; Examples
(define gr0 '())
(define gr1 (make-list 20 initial-cell))
; Template
#;(define (gridrow-temp gr)
    (cond
      [(empty? gr) ...]
      [(cons? gr) (... (cell-temp (first gr)) ... (gridrow-temp (rest gr)) ...)]))


; A Grid is one of:
; - '()
; - (cons GridRow Grid)
; Interpretation: A grid the player plays on. (0,0) is at the upper left-hand corner of the grid.
; Examples
(define grid0 '())
(define grid1 (make-list 20 gr1))
; Template
#;(define (grid-temp g)
    (cond
      [(empty? 0g) ...]
      [(cons? g) (... (gridrow-temp (first g)) ... (grid-temp (rest g)) ...)]))


(define-struct world [player grid])
; A World is a (make-world Player Grid)
; Interpretation
; - the first field is the game player in the world.
; - the second field is state of the game grid that the player plays on.
; Examples
(define world0 (make-world player1 grid0))
(define world1 (make-world player2 grid1)) 
; Template
#;(define (world-temp w)
    (... (player-temp (world-player w)) ... (grid-temp (world-grid w)) ...))


;;;;; CONSTANTS ;;;;;;

(define TNT-FUSE 30)
(define NUM-RANDOM-ELEMENTS 3)  ; Water, Rock, Grass
(define DEFAULT-PLAYER (make-player (make-posn 0 0) "Right" "Grass" 0))


; Exercises 4 & 5 
; ---------------

; main : Natural -> Natural
; Creates a square game board with the given side length, and returns the final score
(define (main side)
  (get-score (big-bang (generate-world side)
               [on-tick update-world]
               [on-draw draw-world]
               [on-key key-handler])))

; update-world : World -> World
; Updates the state of the world
(define (update-world w)
  world1)

; draw-world : World -> Image
; Draws the current state of the world
(define (draw-world w)
  (empty-scene 20 20))

; key-handler : World KeyEvent -> World
; Handles user keyboard input
(define (key-handler w ke)
  world1)

; generate-world : Natural -> World
; Produces a world based on the given side length of the grid
(define (generate-world side)
  (make-world DEFAULT-PLAYER (generate-grid side side)))

(check-satisfied (generate-world 20) world?)
(check-satisfied (world-grid (generate-world 20)) cons?)
(check-expect (length (world-grid (generate-world 20))) 20)
(check-satisfied (world-player (generate-world 20)) player?)

; generate-grid : Natural Natural -> Grid
; Produces a random grid with the given number of rows and columns
(define (generate-grid rows cols)
  (cond
    [(zero? rows) '()]
    [(positive? rows) (cons (generate-grid-row cols) (generate-grid (sub1 rows) cols))]))

(check-expect (length (generate-grid 20 20)) 20)
(check-satisfied (first (generate-grid 20 20)) cons?)
(check-expect (length (first (generate-grid 20 20))) 20)
(check-expect (generate-grid 0 0) '())

; generate-grid-row : Natural -> GridRow
; Produces a random grid row with the given length
(define (generate-grid-row length)
  (cond
    [(zero? length) '()]
    [(positive? length) (cons (pick-cell (random NUM-RANDOM-ELEMENTS))
                              (generate-grid-row (sub1 length)))]))

(check-expect (length (generate-grid-row 20)) 20)
(check-satisfied (first (generate-grid-row 20)) cons?)
(check-expect (length (first (generate-grid-row 20))) 1)
(check-expect (generate-grid-row 0) '())

; pick-cell : Number -> Cell
; Generates a cell with the given block.
(define (pick-cell n)
  (cons (pick-block n) '()))

(check-expect (length (pick-cell 1)) 1)
(check-satisfied (pick-cell 1) cons?)
(check-member-of (first (pick-cell (random NUM-RANDOM-ELEMENTS)))
                 BLOCK-WA BLOCK-GR BLOCK-RO)

; pick-block : Natural -> Block
; Chooses a block based on the number given.
; Doesn't include Gold or TNT because Gold is placed after the grid is already generated, and
; TNT is not naturally occurring.
(define (pick-block block-num)
  (cond
    [(= block-num 0) "Water"]
    [(= block-num 1) "Grass"]
    [(= block-num 2) "Rock"]))

(check-expect (pick-block 0) "Water")
(check-expect (pick-block 1) "Grass")
(check-expect (pick-block 2) "Rock")

; get-score : World -> Natural
; Gets the score from a world state
(define (get-score world)
  (player-score (world-player world)))

(check-expect (get-score world0) 0)
(check-expect (get-score world1) 5)