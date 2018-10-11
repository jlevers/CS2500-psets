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

;;;;; PROGRAM CONSTANTS ;;;;;;;;;
; These are placed here because they are used in examples.

; Time constants.
(define TICK-RATE 1/28) ; TICK-RATE is measured in number of seconds between clock ticks by big-bang.
(define GOLD-TIMER (* 5 (/ 1 TICK-RATE))) ; GOLD-TIMER is five seconds relative to the TICK-RATE.
(define TNT-FUSE 30)
; Drawing constants.
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

(define-struct tnt [fuse])
; A TNT is a (make-tnt Natural).
; Interpretation: TNT that explodes after the given natural number (fuse) counts down in seconds.
; Examples
(define tnt0 (make-tnt 0))
(define tnt1 (make-tnt TNT-FUSE))
; Template
#;
(define (tnt-temp tnt)
  (... (tnt-fuse tnt ...)))


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
       (... (list-temp (cell-blocks c)) ...))) ; We assume list-temp is implicitly defined.


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


;;;;; OTHER PROGRAM CONSTANTS ;;;;;;
; Some of these constants require the above data definitions.
  
(define NUM-RANDOM-BLOCKS 3)  ; # of random blocks that can be world generated : Water, Rock, Grass
(define DEFAULT-PLAYER (make-player (make-posn 0 0) "Right" "Grass" 0))
(define DEFAULT-GOLD-TIMER 0)
; Drawing specific constants.
; TNT
(define TNT (square CELL-SIDE "solid" "orange"))
(define TNT-TEXTSIZE 12)
(define TNT-TEXTCOLOR "black")
; Blocks

; WE SHOULD USE SHRINK

(define WATER-BLOCK water-block)
(define GRASS-BLOCK grass-block)
(define STONE-BLOCK stone-block)
(define GOLD-BLOCK gem-orange)
(define WOOD-BLOCK wood-block)

; Exercises 4 & 5 Reworked
; ------------------------

; main : Natural -> Natural
; Creates a square game board with the given side length, and returns the final score
(define (main side)
  (get-score (big-bang (generate-world side)
               [on-tick update-world TICK-RATE]
               [on-draw draw-world]
               [on-key key-handler])))

; get-score : World -> Natural
; Gets the score from a given world state.
(define (get-score world)
  (player-score (world-player world)))

(check-expect (get-score world0) 0)
(check-expect (get-score world1) 5)

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
  (overlay (text (number->string (tnt-fuse tnt))
                 TNT-TEXTSIZE
                 TNT-TEXTCOLOR)
           TNT))

(check-expect (draw-tnt (make-tnt 12)) (overlay (text "12" TNT-TEXTSIZE TNT-TEXTCOLOR) TNT))

; draw-block : Block -> Image
; Returns the image for the given block
(define (draw-block b)
  (cond
    [(tnt? b) (draw-tnt b)]
    [(string=? b "Water") WATER-BLOCK]
    [(string=? b "Grass") GRASS-BLOCK]
    [(string=? b "Rock") STONE-BLOCK]
    [(string=? b "Gold") GOLD-BLOCK]
    [(string=? b "Wood") WOOD-BLOCK]))

(check-expect (draw-block (make-tnt 12)) (overlay (text "12" TNT-TEXTSIZE TNT-TEXTCOLOR) TNT))
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
  (make-world DEFAULT-PLAYER (generate-grid side side) DEFAULT-GOLD-TIMER))

(check-satisfied (generate-world 20) world?)
(check-satisfied (world-grid (generate-world 20)) cons?)
(check-satisfied (world-player (generate-world 20)) player?)

; generate-grid : Natural Natural -> Grid
; Produces a random grid with the given number of rows and columns
(define (generate-grid rows cols)
  (local (
          ; create-cell : Natural -> Cell
          ; Generates a cell from a given index of all the cells in the grid.
          (define (create-cell i)
            (make-cell (make-posn (modulo i cols) ; x pos increases every increment. Resets at cols.
                                  (floor (/ i rows))) ; y pos increases every 'rows'th increment.
                       (list (pick-block (random NUM-RANDOM-BLOCKS))))))
    (build-list (* rows cols) create-cell)))


(check-expect (length (generate-grid 20 20)) 400)
(check-satisfied (generate-grid 20 20) cons?)
(check-expect (generate-grid 0 0) '())
(check-expect (generate-grid 20 0) '())
(check-expect (generate-grid 0 20) '())

(check-expect (andmap cell? (generate-grid 20 20)) true)
(check-expect (andmap zero? (map sub1 (map length (map cell-blocks (generate-grid 20 20))))) #t)
(check-expect (length (filter zero? (map posn-x (map cell-pos (generate-grid 20 20))))) 20)
(check-expect (length (filter zero? (map posn-y (map cell-pos (generate-grid 20 20))))) 20)


; pick-block : Natural -> Block
; Chooses a random block based on the number given.
; Doesn't include Gold or TNT because Gold is placed after the grid is already generated, and
; TNT is not naturally occurring.
(define (pick-block block-num)
  (cond
    [(= block-num 0) "Water"]
    [(= block-num 1) "Grass"]
    [(= block-num 2) "Rock"]
    ; Currently disabled functionality.
    [(= block-num 3) "Wood"]))

(check-expect (pick-block 0) "Water")
(check-expect (pick-block 1) "Grass")
(check-expect (pick-block 2) "Rock")

(check-expect (pick-block 3) "Wood")