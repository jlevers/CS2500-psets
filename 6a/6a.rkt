;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname 6a) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp")) #f)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Problem Set 6b (Based on 5a) ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Code changes were made since 5a to make the program run cleaner.
; Code changes completely specific to exercises 4 and 5 are marked with comments.

;;;;; REQUIRED LIBRARIES ;;;;;;;;

(require 2htdp/image)
(require 2htdp/universe)

;;;;; PROGRAM CONSTANTS ;;;;;;;;;
; These are placed here because they are used in examples for data definitions.
; Most of the program constants are beneath the data definitions.

; Time constants.
(define TICK-RATE 1/28) ; TICK-RATE is measured in number of seconds between clock ticks by big-bang.
(define GOLD-TIMER (* 5 (/ 1 TICK-RATE))) ; GOLD-TIMER is five seconds relative to the TICK-RATE.
(define TNT-FUSE 30)

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


(define-struct player [pos direction selected score])
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
  (... (gridposn-temp (player-pos player)) ...
       (direction-temp (player-direction player)) ...
       (block-temp (player-selected player)) ...
       (player-score player) ...))


(define-struct cell [pos blocks])
; A Cell is a (make-cell GridPosn [NEList-of Block])
; Interpretation:
; - the first field is a GridPosn describing the cell's position on the grid
; - the second field is a non-empty list of the blocks in the cell
; Examples
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

; World generation constants.
(define NUM-RANDOM-BLOCKS 3)  ; # of random blocks that can be world generated : Water, Rock, Grass
(define DEFAULT-PLAYER (make-player (make-posn 0 0) "RIGHT" "Grass" 0))
(define DEFAULT-GOLD-TIMER 0)

; Exercise 6B.4.

; Drawing specific constants.

;; Blocks
(define CELL-HEIGHT 50) ; Height (in px) of a grid cell
(define CELL-WIDTH 50) ; Width (in px) of a grid cell

; Below is a helper function for drawing blocks.

; create-block : String -> Image
; creates a block with a given color
(define (create-block color)
  (frame (rectangle CELL-WIDTH CELL-HEIGHT "solid" color)))

(define WATER-BLOCK (create-block "blue"))
(define GRASS-BLOCK (create-block "green"))
(define STONE-BLOCK (create-block "gray"))
(define GOLD-BLOCK (create-block "gold"))
(define WOOD-BLOCK (create-block "brown"))

;;; TNT
(define TNT (rectangle CELL-WIDTH CELL-HEIGHT "solid" "orange"))
(define TNT-TEXTSIZE (floor (/ CELL-HEIGHT 4)))
(define TNT-TEXTCOLOR "black")


;; Player
(define PLAYER-POLY (list (make-posn 5 (- CELL-HEIGHT 5))
                          (make-posn (floor (/ CELL-WIDTH 2)) 5)
                          (make-posn (- CELL-WIDTH 5) (- CELL-HEIGHT 5))))

(define PLAYER-UP (overlay (polygon PLAYER-POLY
                                    "outline"
                                    (make-pen "black" 3 "solid" "projecting" "miter"))
                           (polygon PLAYER-POLY
                                    "solid"
                                    "white")
                           (rectangle CELL-WIDTH CELL-HEIGHT "solid" "transparent")))

(define PLAYER-LEFT (rotate 90 PLAYER-UP))
(define PLAYER-DOWN (rotate 180 PLAYER-UP))
(define PLAYER-RIGHT (rotate -90 PLAYER-UP))

;; Arrows
(define ARROW-UP (above (triangle 40 "solid" "black")
                        (rectangle 10 20 "solid" "black")))
(define ARROW-LEFT (rotate 90 ARROW-UP))
(define ARROW-DOWN (rotate 180 ARROW-UP))
(define ARROW-RIGHT (rotate -90 ARROW-UP))


;; Menu
(define MENU-PAD (rectangle 1 50 "solid" "transparent")) ; Invisible vertical padding for menu item
(define MENU-LABEL-FONT-SIZE 24) ; Font settings for labels for menu items.
(define MENU-LABEL-FONT-COLOR "black")

;;; Standing on block constants.
(define STANDING-SEARCH-DEPTH 4) ; How deep the player can see into a cell they're standing on.

;; Score
(define SCORE-FONT-SIZE CELL-HEIGHT)
(define SCORE-FONT-COLOR "black")

; Cell Search Constants
(define NO-CELL-ERROR "The cell with the given position does not exist.")
(define MULT-CELL-ERROR "Multiple cells with the given position exist.")

;;;;;; GAME FUNCTIONS ;;;;;;

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

;;; ON-TICK FUNCTIONS ;;;

; update-world : World -> World
; Updates the state of the world
(define (update-world w)
  world1)

;;; DRAWING FUNCTIONS ;;;

; All of this was written for Exercise 4.

; draw-world : World -> Image
; Draws the current state of the world
(define (draw-world w)
  (beside (draw-menu (world-player w) (world-grid w))
          (draw-player (world-player w)
                       (draw-grid (world-grid w) empty-image))))

; Functions related to drawing the grid are below.

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
(check-expect (draw-block "Water") WATER-BLOCK)
(check-expect (draw-block "Grass") GRASS-BLOCK)
(check-expect (draw-block "Rock") STONE-BLOCK)
(check-expect (draw-block "Gold") GOLD-BLOCK)
(check-expect (draw-block "Wood") WOOD-BLOCK)

; gp->posn : GridPosn -> Posn
; Converts a given GridPosn to a Posn in terms of pixels.
(define (gp->posn gp)
  (make-posn (* (posn-x gp) CELL-WIDTH) (* (posn-y gp) CELL-HEIGHT)))

(check-expect (gp->posn gp1) (make-posn (* 15 CELL-WIDTH) (* 20 CELL-HEIGHT)))

; draw-cell : Cell Image -> Image
; Draws the given cell
(define (draw-cell cell img)
  (local (
          (define x (- (posn-x (gp->posn (cell-pos cell)))))
          (define y (- (posn-y (gp->posn (cell-pos cell)))))
          (define block (first (cell-blocks cell))))
    (overlay/xy (draw-block block) x y img)))

(check-expect (draw-cell initial-cell (empty-scene 100 100))
              (overlay/xy (draw-block "Water")
                          (- CELL-WIDTH)
                          (- CELL-HEIGHT)
                          (empty-scene 100 100)))

; draw-grid : Grid Image -> Image
; Draws the grid of cells onto the given image.
(define (draw-grid g img)
  (foldr draw-cell
         img
         g))

(check-expect (draw-grid grid0 empty-image)
              empty-image)
(check-expect (draw-grid grid1 empty-image)
              (above
               (foldr beside empty-image (make-list 3 WATER-BLOCK))
               (foldr beside empty-image (make-list 3 WATER-BLOCK))
               (foldr beside empty-image (make-list 3 WATER-BLOCK))))

; pick-player : Direction -> Image
; Outputs the player image for the given direction.
(define (pick-player dir)
  (cond
    [(string=? dir "UP") PLAYER-UP]
    [(string=? dir "DOWN") PLAYER-DOWN]
    [(string=? dir "LEFT") PLAYER-LEFT]
    [(string=? dir "RIGHT") PLAYER-RIGHT]))

(check-expect (pick-player "UP") PLAYER-UP)
(check-expect (pick-player "DOWN") PLAYER-DOWN)
(check-expect (pick-player "LEFT") PLAYER-LEFT)
(check-expect (pick-player "RIGHT") PLAYER-RIGHT)

; draw-player : Player Image -> Image
; draws a player ontop the given image (grid)
(define (draw-player player img)
  (local (
          (define x (- (posn-x (gp->posn (player-pos player)))))
          (define y (- (posn-y (gp->posn (player-pos player))))))
    (overlay/xy (pick-player (player-direction player)) x y img)))

(check-expect (draw-player player1 (empty-scene 100 100))
              (overlay/xy PLAYER-RIGHT 0 0 (empty-scene 100 100)))

(check-expect (draw-player player2 (empty-scene 1050 1050))
              (overlay/xy PLAYER-LEFT (* -1 15 CELL-WIDTH) (* -1 20 CELL-HEIGHT)
                          (empty-scene 1050 1050)))

; Functions related to drawing the menu for the player are below.

; draw-menu : Player Grid -> Image
; draws the menu for the given player and grid info.
(define (draw-menu p g)
  (above (draw-menu-item "Your Score" (draw-score (player-score p)))
         (draw-menu-item "Your Direction" (draw-direction (player-direction p)))
         (draw-menu-item "Beneath You" (draw-materials (top-blocks (player-pos p) g)))
         (draw-menu-item "Selected Material" (draw-block (player-selected p)))))

(check-expect (draw-menu player1 grid1)
              (above (draw-menu-item "Your Score" (draw-score 0))
                     (draw-menu-item "Your Direction" (draw-direction "RIGHT"))
                     (draw-menu-item "Beneath You" (draw-materials (list BLOCK-WA)))
                     (draw-menu-item "Selected Material" (draw-block "Wood"))))

; draw-menu-item : String Image -> Image
; draws a menu item complete with a label and padding.
(define (draw-menu-item name item)
  (above MENU-PAD
         (text name MENU-LABEL-FONT-SIZE MENU-LABEL-FONT-COLOR)
         item))

(check-expect (draw-menu-item "TEST" (circle 20 "solid" "black"))
              (above MENU-PAD
                     (text "TEST" MENU-LABEL-FONT-SIZE MENU-LABEL-FONT-COLOR)
                     (circle 20 "solid" "black")))

; draw-score : Number -> Image
; draws the given score
(define (draw-score s)
  (text (number->string s) SCORE-FONT-SIZE SCORE-FONT-COLOR))

(check-expect (draw-score 14) (text "14" SCORE-FONT-SIZE SCORE-FONT-COLOR))

; draw-direction : Direction Image -> Image
; Outputs the arrow image for the given direction.
(define (draw-direction dir)
  (cond
    [(string=? dir "UP") ARROW-UP]
    [(string=? dir "DOWN") ARROW-DOWN]
    [(string=? dir "LEFT") ARROW-LEFT]
    [(string=? dir "RIGHT") ARROW-RIGHT]))

(check-expect (draw-direction "UP") ARROW-UP)
(check-expect (draw-direction "DOWN") ARROW-DOWN)
(check-expect (draw-direction "LEFT") ARROW-LEFT)
(check-expect (draw-direction "RIGHT") ARROW-RIGHT)

;; All the functions below are used to provide the functionality of revealing what's underground.

; top-blocks : GridPosn Grid -> [List-of Block]
; Using the given position on the grid, finds all the blocks in the cell to a certain depth.
(define (top-blocks gp g)
  (truncate (cell-blocks (search-cell gp g)) STANDING-SEARCH-DEPTH))

(check-expect (top-blocks gp0 grid1) (list BLOCK-WA))
(check-expect (top-blocks gp1 (list (make-cell (make-posn 0 0) (list BLOCK-WA))
                                    (make-cell (make-posn 15 20) (make-list 5 BLOCK-WA))))
              (make-list STANDING-SEARCH-DEPTH BLOCK-WA))

; truncate : [List-of X] Natural -> [List-of X]
; returns a list truncated to have a given max length.
(define (truncate l depth)
  (cond [(or (zero? depth) (empty? l)) '()]
        [(positive? depth) (cons (first l) (truncate (rest l) (sub1 depth)))]))

(check-expect (truncate (make-list 10 "TEST") 2) (list "TEST" "TEST"))
(check-expect (truncate (make-list 3 "TEST") 4) (list "TEST" "TEST" "TEST"))
(check-expect (truncate '() 4) '())

; search-cell : GridPosn Grid -> Cell
; Using the given position, finds a matching cell in the grid.
(define (search-cell gp g)
  (local (; matching-cell? : Cell -> Boolean
          ; returns true if the cell has a matching position.
          (define (matching-cell? cell)
            (and (= (posn-x (cell-pos cell)) (posn-x gp))
                 (= (posn-y (cell-pos cell)) (posn-y gp))))
          
          (define filtered-cells (filter matching-cell? g)))
    (cond [(= (length filtered-cells) 0) (error NO-CELL-ERROR)]
          [(= (length filtered-cells) 1) (first filtered-cells)]
          [else (error MULT-CELL-ERROR)])))

(check-error (search-cell gp0 grid0) NO-CELL-ERROR)
(check-error (search-cell gp0 (make-list 2 (make-cell (make-posn 0 0) BLOCK-WA))) MULT-CELL-ERROR)
(check-expect (search-cell gp0 grid1) (first grid1)) ; The first cell is the head of the grid list.

; draw-materials : [List-of Block] -> Image
; Draws a list of blocks in a stack.
(define (draw-materials lob)
  (local (; draw-below : Block Image -> Image
          ; draws the image of the block below the given image
          (define (draw-below block img)
            (above img
                   (draw-block block))))
    (foldr draw-below empty-image lob)))

(check-expect (draw-materials (make-list 4 BLOCK-WA))
              (foldr above empty-image (make-list 4 WATER-BLOCK)))

;;; Key Handler Functions ;;;

; key-handler : World KeyEvent -> World
; Handles user keyboard input
(define (key-handler w ke)
  world1)

;;; World Generation Functions ;;;

; generate-world : Natural -> World
; Produces a world based on the given side length of the grid
(define (generate-world side)
  (make-world DEFAULT-PLAYER (generate-grid side) DEFAULT-GOLD-TIMER))

(check-satisfied (generate-world 20) world?)
(check-satisfied (world-grid (generate-world 20)) cons?)
(check-satisfied (world-player (generate-world 20)) player?)

; generate-grid : Natural -> Grid
; Produces a random grid with the given number of rows and columns
(define (generate-grid side)
  (local (
          ; create-cell : Natural -> Cell
          ; Generates a cell from a given index of all the cells in the grid.
          (define (create-cell i)
            (make-cell (make-posn (modulo i side) ; x pos increases every increment. Resets at cols.
                                  (floor (/ i side))) ; y pos increases every 'rows'th increment.
                       (list (pick-block (random NUM-RANDOM-BLOCKS))))))
    (build-list (sqr side) create-cell)))


(check-expect (length (generate-grid 20)) 400)
(check-satisfied (generate-grid 20) cons?)
(check-expect (generate-grid 0) '())

(check-expect (andmap cell? (generate-grid 20)) true)
(check-expect (andmap zero? (map sub1 (map length (map cell-blocks (generate-grid 20))))) #t)
(check-expect (length (filter zero? (map posn-x (map cell-pos (generate-grid 20))))) 20)
(check-expect (length (filter zero? (map posn-y (map cell-pos (generate-grid 20))))) 20)


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

(main 2)