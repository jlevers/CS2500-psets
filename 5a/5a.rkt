;;;;;;;;;;;;;;;;;;
; Problem Set 5a ;
;;;;;;;;;;;;;;;;;;

(require 2htdp/image)
(require 2htdp/universe)


; Exercises 2 & 3
; ---------------

;;;;; CONSTANTS ;;;;;;

(define TNTFUSE 30)
(define MAX-DEPTH 5)
(define NUM-RANDOM-ELEMENTS 4)  ; Wood, Water, Rock, Grass


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

(define-struct player [location direction score])
; A Player is a (make-player GridPosn Direction Natural).
; Interpretation
; - the first field is the player's position on the grid.
; - the second field is the player's front-facing direction.
; - the third field is the player's score.

; Examples
(define player1 (make-player gp0 DIR-RIGHT 0))
(define player2 (make-player gp1 DIR-LEFT 5))

; Template
#;(define (player-temp player)
  (... (gridposn-temp (player-location player)) ... (direction-temp (player-direction player)) ...))


; A TNT is a Natural in the range [0,TNTFUSE].
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

; A Cell is one of:
; - '()
; - (cons Block Cell)
; Interpretation: A cell in a grid row that contains a stack of Blocks.

; Examples
(define cell0 '())
(define cell1 (list BLOCK-TNT BLOCK-WA BLOCK-WO BLOCK-GO BLOCK-GR))
(define cell2 (list BLOCK-RO BLOCK-WA))

; Template
(define (cell-temp c)
  (cond
    [(empty? c) ...]
    [(cons? c) (... (block-temp (first c)) ... (cell-temp (rest c)) ...)]))

; A GridRow is one of:
; - '()
; - (cons Cell GridRow)
; Interpretation: A row of cells that the player plays on.

; Examples
(define gr0 '())
(define gr1 (make-list 20 cell1))

; Template
#;(define (gridrow-temp gr)
  (cond
    [(empty? gr) ...]
    [(cons? gr) (... (cell-temp (first gr)) ... (gridrow-temp (rest gr)) ...)]))

; A Grid is one of:
; - '()
; - (cons GridRow Grid)
; Interpretation: A grid the player plays on.

; Examples
(define grid0 '())
(define grid1 (make-list 20 gr1))

; Template
#;(define (grid-temp g)
  (cond
    [(empty? g) ...]
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


; Exercise 4
; ----------

; main : Natural -> Natural
; Creates a square game board with the given side length, and returns the final score
(define (main side)
  (get-score (big-bang (generate-world side)
            [on-tick update-world]
            [on-draw draw-world]
            [on-key key-handler])))

; generate-world : Natural -> World
; Produces a world based on the given side length of the grid
(define (generate-world side)
  (make-world (make-player (make-posn 0 0) DIR-RIGHT 0)
              (generate-grid side)))

; generate-grid : Natural -> Grid
; Produces a random grid with the given side length
(define (generate-grid side)
  (cond
    [(zero? side) '()]
    [(positive? side) (cons (generate-grid-row side) (generate-grid (sub1 side)))]))

; generate-grid-row : Natural -> GridRow
; Produces a random grid row with the given length
(define (generate-grid-row length)
  (cond
    [(zero? length) '()]
    [(positive? length) (cons (generate-random-cell (add1 (random MAX-DEPTH)))
                              (generate-grid-row (sub1 length)))]))

; generate-random-cell : Natural -> Cell
; Generates a random cell with the given depth
(define (generate-random-cell depth)
  (cond
    [(zero? depth) '()]
    [(positive? depth) (cons (pick-block (random NUM-RANDOM-ELEMENTS)) 
                             (generate-random-cell (sub1 depth)))]))

; pick-block : Natural -> Block
; Chooses a block based on the number given
(define (pick-block block-num)
  (cond
    [(= block-num 0) "Water"]
    [(= block-num 1) "Grass"]
    [(= block-num 2) "Rock"]
    [(= block-num 3) "Wood"]))

; get-score : World -> Natural
; Gets the score from a world state
(define (get-score world)
  (player-score (world-player world)))

; update-world : World -> World
; Updates the state of the world
(define (update-world w)
  w)

; draw-world : World -> Image
; Draws the current state of the world
(define (draw-world w)
  (empty-scene 20 20))

; key-handler : World KeyEvent -> World
; Handles user keyboard input
(define (key-handler w ke)
  w)
