;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname 6a) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp")) #f)))
;;;;;;;;;;;;;;;;;;;;;;;;;
; Problem Set 5a and 6b ;
;;;;;;;;;;;;;;;;;;;;;;;;;

(require 2htdp/image)
(require 2htdp/universe)
(require 2htdp/planetcute)


; Exercises 2 & 3
; ---------------

;;;;; CONSTANTS THAT MUST BE PRE-DEFINED ;;;;;;;;;

(define TICK-RATE 1/28)
(define GOLD-TIMER (* 5 (/ 1 TICK-RATE))) ; GOLD-TIMER is five seconds.
(define CELL-SIDE 50) ; Length (in px) of the side of a grid cell (which is square).

;;;;; DATA DEFINITIONS ;;;;;

; A GridPosn is a (make-posn Natural Natural)
; Interpretation
; - the first field is the player's X coordinate in grid units.
; - the second field is the player's Y coordinate in grid units.
; Examples
(define gp0 (make-posn 0 0))
(define gp1 (make-posn 15 20))
; Template
#;
(define (gridposn-temp gp)
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
#;
(define (direction-temp dir)
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
#;
(define (tnt-temp tnt)
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
#;
(define (block-temp b)
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
#;
(define (player-temp player)
  (... (gridposn-temp (player-location player)) ...
       (direction-temp (player-direction player)) ...
       (block-temp (player-selected player)) ...
       (player-score player) ...))


(define-struct cell [pos blocks])
; A Cell is a (make-cell GridPosn [List-of Block])
; Interpretation:
; - the first field is a GridPosn describing the cell's position on the grid
; - the second field is a list of the blocks in the cell
; Examples
(define cell0 (make-cell (make-posn 0 0) '()))
(define initial-cell (make-cell (make-posn 1 1) (list BLOCK-WA)))
(define user-cell (make-cell (make-posn 3 2) (list BLOCK-RO BLOCK-WA)))
; Template
#;
(define (cell-temp c)
  (... (gridposn-temp (cell-pos c)) ...
       (... (cell-blocks c) ...)))


; A Grid is one of:
; - '()
; - (cons Cell Grid)
; Interpretation: A grid the player plays on. (0,0) is at the upper left-hand corner of the grid.
; Examples
(define grid0 '())
(define grid1 (list (make-cell (make-posn 0 0) (list BLOCK-WA))
                    (make-cell (make-posn 0 1) (list BLOCK-WA))
                    (make-cell (make-posn 0 2) (list BLOCK-WA))
                    (make-cell (make-posn 1 0) (list BLOCK-WA))
                    (make-cell (make-posn 1 1) (list BLOCK-WA))
                    (make-cell (make-posn 1 2) (list BLOCK-WA))
                    (make-cell (make-posn 2 0) (list BLOCK-WA))
                    (make-cell (make-posn 2 1) (list BLOCK-WA))
                    (make-cell (make-posn 2 2) (list BLOCK-WA))))
; Template
#;
(define (grid-temp g)
  (cond
    [(empty? g) ...]
    [(cons? g) (... (cell-temp (first g)) ... (cell-temp (rest g)) ...)]))


(define-struct world [player grid clock])
; A World is a (make-world Player Grid)
; Interpretation
; - the first field is the game player in the world.
; - the second field is state of the game grid that the player plays on.
; - the third is the clock keeping track of the number of ticks elapsed.
; Examples
(define world0 (make-world player1 grid0 0))
(define world1 (make-world player2 grid1 GOLD-TIMER)) 
; Template
#;
(define (world-temp w)
  (... (player-temp (world-player w)) ...
       (grid-temp (world-grid w)) ...
       ... (world-clock w) ...))


;;;;; CONSTANTS ;;;;;;

(define TNT-FUSE 30)
(define NUM-RANDOM-ELEMENTS 3)  ; Water, Rock, Grass
(define DEFAULT-PLAYER (make-player (make-posn 0 0) "Right" "Grass" 0))
(define TNT (square CELL-SIDE "solid" "orange"))


; Exercises 4 & 5 
; ---------------

; main : Natural -> Natural
; Creates a square game board with the given side length, and returns the final score
(define (main side)
  (get-score (big-bang (generate-world side)
               [on-tick update-world TICK-RATE]
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

; draw-tnt : TNT -> Image
; Draws a TNT block with a timer on it
(define (draw-tnt tnt)
  (overlay (text (number->string tnt) 12 "black") TNT))

(check-expect (draw-tnt 12) (overlay (text "12" 12 "black") TNT))

; draw-block : Block -> Image
; Returns the image for the given block
(define (draw-block b)
  (cond
    [(number? b) (draw-tnt b)]
    [(string=? b "Water") water-block]
    [(string=? b "Grass") grass-block]
    [(string=? b "Rock") stone-block]
    [(string=? b "Gold") gem-orange]
    [(string=? b "Wood") wood-block]))

(check-expect (draw-block "Water") water-block)
(check-expect (draw-block "Grass") grass-block)
(check-expect (draw-block "Rock") stone-block)
(check-expect (draw-block "Gold") gem-orange)
(check-expect (draw-block "Wood") wood-block)

; draw-player : Player Image -> Image
; draw-cell : Cell Image -> Image
; draw-block : Block Image -> Image
; draw-grid : Grid Image -> Image
; draw-menu : Player Image -> Image
; draw-score : Number Image -> Image
; draw-materials : [List-of Block] Image -> Image
; draw-direction : Direction Image -> Image
; draw-tnt : TNT Image -> Image

; key-handler : World KeyEvent -> World
; Handles user keyboard input
(define (key-handler w ke)
  world1)

; generate-world : Natural -> World
; Produces a world based on the given side length of the grid
(define (generate-world side)
  (make-world DEFAULT-PLAYER (generate-grid side side) 0))

(check-satisfied (generate-world 20) world?)
(check-satisfied (world-grid (generate-world 20)) cons?)
(check-satisfied (world-player (generate-world 20)) player?)

; generate-grid : Natural Natural -> Grid
; Produces a random grid with the given number of rows and columns
(define (generate-grid rows cols)
  (local (
          ; create-row : Natural -> Grid
          ; Generates a grid row with the given y coordinate
          (define (create-row y)
            (generate-grid-row cols y)))
  (foldr append '() (build-list rows create-row))))
  
(check-expect (length (generate-grid 20 20)) 400)
(check-satisfied (generate-grid 20 20) cons?)
(check-satisfied (first (generate-grid 20 20)) cell?)
(check-expect (generate-grid 0 0) '())

; generate-grid-row : Natural Natural -> Grid
; Produces a random grid row with the given length
(define (generate-grid-row length y)
  (local (
          ; generate-cell : Natural -> Cell
          ; Generates a cell at the given x coordinate
          (define (generate-cell num)
            (make-cell (make-posn num y)
                       (list (pick-block (random NUM-RANDOM-ELEMENTS))))))
    (build-list length generate-cell)))

(check-expect (length (generate-grid-row 20 1)) 20)
(check-satisfied (first (generate-grid-row 20 1)) cell?)
(check-expect (length (cell-blocks (first (generate-grid-row 20 1)))) 1)
(check-expect (generate-grid-row 0 0) '())
(check-expect (posn-x (cell-pos (first (generate-grid-row 20 1)))) 0)
(check-expect (posn-x (cell-pos (second (generate-grid-row 20 1)))) 1)


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
