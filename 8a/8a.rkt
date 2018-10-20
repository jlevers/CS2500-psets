;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname 8a) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
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
(define gp1 (make-posn 1 2))
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
; A Cell is a (make-cell GridPosn [List-of Block])
; Interpretation:
; - the first field is a GridPosn describing the cell's position on the grid
; - the second field is a list of the blocks in the cell
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
(define grid2 (list (make-cell (make-posn 0 0) (list BLOCK-WA))
                    (make-cell (make-posn 0 1) (list BLOCK-WA))
                    (make-cell (make-posn 0 2) (list BLOCK-WA))
                    (make-cell (make-posn 1 0) (list BLOCK-WA))
                    (make-cell (make-posn 1 1) (list BLOCK-WA (make-tnt 0)))
                    (make-cell (make-posn 1 2) (list BLOCK-WA))
                    (make-cell (make-posn 2 0) (list BLOCK-WA))
                    (make-cell (make-posn 2 1) (list BLOCK-WA))
                    (make-cell (make-posn 2 2) (list BLOCK-WA))))
(define grid3 (list (make-cell (make-posn 0 0) (list BLOCK-WA))
                    (make-cell (make-posn 0 1) (list BLOCK-WA))
                    (make-cell (make-posn 0 2) (list BLOCK-WA))
                    (make-cell (make-posn 1 0) (list BLOCK-WA))
                    (make-cell (make-posn 1 1) (list BLOCK-WA (make-tnt 5)))
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
(define world2 (make-world player2 grid2 1))

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

; Drawing specific constants. Exercise 4

;; Blocks
(define CELL-HEIGHT 50) ; Height (in px) of a grid cell
(define CELL-WIDTH 50) ; Width (in px) of a grid cell

; Below is a helper function for drawing blocks.

; create-block : String -> Image
; creates a block with a given color
(define (create-block color)
  (frame (rectangle CELL-WIDTH CELL-HEIGHT "solid" color)))

(check-expect (create-block "green") (frame (rectangle CELL-WIDTH CELL-HEIGHT "solid" "green")))
(check-expect (create-block "white") (frame (rectangle CELL-WIDTH CELL-HEIGHT "solid" "white")))

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
  (make-world (world-player w)
              (update-gold (world-clock w) (decrement-fuse (update-tnt (world-grid w))))
              (add1 (world-clock w))))

(check-expect (update-world (make-world player1 grid1 5)) (make-world player1 grid1 6))


; update-gold : Natural Grid -> Grid
; Using value of world clock, and grid, produces new grid which has new gold if enough time passed.
(define (update-gold t g)
  (if (= (modulo t GOLD-TIMER) 0)
      (place-random-gold g)
      g))

(check-expect (update-gold 1 grid2) grid2)

; Does the gold block exist in our water world?
(check-expect (member "Gold"
                      (foldr (λ (cell list) (append (cell-blocks cell) list))
                             '()
                             (update-gold GOLD-TIMER grid1)))
              #t)

; Is the gold beneath existing materials?
(check-expect (member "Gold"
                      (foldr (λ (cell list)
                               (append (rest (cell-blocks cell)) list))
                             '()
                             (update-gold GOLD-TIMER grid1)))
              #t)

; place-random-gold : Grid -> Grid
; Randomly chooses a cell to place gold in, and places gold somewhere in the blocks list.
(define (place-random-gold g)
  (local (
          (define possible-positions (map cell-pos (filter (λ (cell) (cons? (cell-blocks cell))) g)))
          (define cell-position (pick-random-element possible-positions))
          ; 
          (define (add-gold cell)
            (if (posn=? cell-position (cell-pos cell))
                (make-cell (cell-pos cell) (insert-below (cell-blocks cell) BLOCK-GO))
                cell)))
    (map add-gold g)))

(check-expect (place-random-gold (list (make-cell (make-posn 0 0) (list BLOCK-WA))))
              (list (make-cell (make-posn 0 0) (list BLOCK-WA BLOCK-GO))))
; Gold must never be placed in empty cells.
(check-expect (place-random-gold (list (make-cell (make-posn 0 0) (list BLOCK-WA))
                                       (make-cell (make-posn 0 1) '())))
              (list (make-cell (make-posn 0 0) (list BLOCK-WA BLOCK-GO))
                    (make-cell (make-posn 0 1) '())))

; pick-random-element : [List-of X] -> X
; Picks a random element from the given list
(define (pick-random-element l)
  (list-ith l (random (length l))))

(check-expect (member (pick-random-element (list 1 2 3 4)) (list 1 2 3 4)) #t)

; list-ith : [List-of X] Natural -> X
(define (list-ith l num)
  (cond
    [(zero? num) (first l)]
    [else (list-ith (rest l) (sub1 num))]))

(check-expect (list-ith (list 1 2) 1) 2)

; insert-below : [List-Of X] X -> [List-Of X]
; Randomly inserts a given element into the list.
(define (insert-below l x)
  (cond
    [(empty? l) (list x)]
    [(cons? l) (cons (first l) (cons x (rest l)))]))

(check-expect (insert-below (list "X" "X" "X") "Y") (list "X" "Y" "X" "X"))
(check-expect (insert-below '() "Y") (list "Y"))

; decrement-fuse : Grid -> Grid
; Finds all tnt blocks in the grid and decrements the fuses.
(define (decrement-fuse g)
  (local (; decrement-block : Block -> Block
          ; If the given block is a tnt, decrements the fuse.
          (define (decrement-block b)
            (if (tnt? b)
                (make-tnt (sub1 (tnt-fuse b)))
                b))
          ; decrement-cell : Cell -> Cell
          (define (decrement-cell cell)
            (make-cell (cell-pos cell) (map decrement-block (cell-blocks cell)))))
    (map decrement-cell g)))

(check-expect (decrement-fuse grid3)
              (list (make-cell (make-posn 0 0) (list BLOCK-WA))
                    (make-cell (make-posn 0 1) (list BLOCK-WA))
                    (make-cell (make-posn 0 2) (list BLOCK-WA))
                    (make-cell (make-posn 1 0) (list BLOCK-WA))
                    (make-cell (make-posn 1 1) (list BLOCK-WA (make-tnt 4)))
                    (make-cell (make-posn 1 2) (list BLOCK-WA))
                    (make-cell (make-posn 2 0) (list BLOCK-WA))
                    (make-cell (make-posn 2 1) (list BLOCK-WA))
                    (make-cell (make-posn 2 2) (list BLOCK-WA))))

; update-tnt : Grid -> Grid
; Finds all the tnt blocks in the grid. If tnt's fuse has run out, explode.
(define (update-tnt g)
  (local (; contains-primed-tnt? : Cell -> Boolean
          ; Returns true if the cell's blocks contains a primed tnt.
          (define (contains-primed-tnt? cell)
            (ormap primed-tnt? (cell-blocks cell)))
          ; explode-cell : Cell Grid -> Grid
          ; Causes an explosion in the grid from the given cell.
          (define (explode-cell cell g)
            (if (contains-primed-tnt? cell)
                (explosion g (cell-pos cell))
                g)))
    (foldr explode-cell g g)))

(check-expect (update-tnt grid2) (explosion grid2 (make-posn 1 1)))
(check-expect (update-tnt grid3) grid3)

; primed-tnt? : Block -> Boolean
; Returns true if it is a TNT at timer = 0, false otherwise
(define (primed-tnt? b)
  (and (tnt? b) (= (tnt-fuse b) 0)))

(check-expect (primed-tnt? (make-tnt 0)) #t)
(check-expect (primed-tnt? (make-tnt 4)) #f)
(check-expect (primed-tnt? BLOCK-WA) #f)

; From problem set 3a and 4b
; add-posns : Posn Posn -> Posn
; adds the x and y values of p1 and p2 together to make a new Posn.
(define (add-posns p1 p2)
  (make-posn (+ (posn-x p1) (posn-x p2)) (+ (posn-y p1) (posn-y p2))))

(check-expect (add-posns (make-posn 20 20) (make-posn 21 21)) (make-posn 41 41))

; neighbor-to-tnt : GridPosn GridPosn -> Boolean
; Determines if the first given gridposn is a neighbor to a second gridposn.
(define (neighbor-to-tnt gp1 gp2)
  (and (>= (posn-x gp1) (sub1 (posn-x gp2))) (<= (posn-x gp1) (add1 (posn-x gp2)))
       (>= (posn-y gp1) (sub1 (posn-y gp2))) (<= (posn-y gp1) (add1 (posn-y gp2)))))

(check-expect (neighbor-to-tnt (make-posn 0 0) (make-posn -1 -1)) #t)
(check-expect (neighbor-to-tnt (make-posn 0 0) (make-posn -1 0)) #t)
(check-expect (neighbor-to-tnt (make-posn 0 0) (make-posn -1 1)) #t)
(check-expect (neighbor-to-tnt (make-posn 0 0) (make-posn 0 -1)) #t)

(check-expect (neighbor-to-tnt (make-posn 0 0) (make-posn 0 2)) #f)

(check-expect (neighbor-to-tnt (make-posn 0 0) (make-posn 0 1)) #t)
(check-expect (neighbor-to-tnt (make-posn 0 0) (make-posn 1 -1)) #t)
(check-expect (neighbor-to-tnt (make-posn 0 0) (make-posn 1 0)) #t)
(check-expect (neighbor-to-tnt (make-posn 0 0) (make-posn 1 1)) #t)

; update-block-for-tnt : Cell GridPosn -> Cell
; Removes blocks in the cell unless there's a primed tnt, OR it's not a neighbor to the given posn.
(define (update-block-for-tnt cell gp)
  (cond
    [(posn=? (cell-pos cell) gp) (make-cell (cell-pos cell) '())]
    [(neighbor-to-tnt (cell-pos cell) gp)
     (make-cell (cell-pos cell) (filter primed-tnt? (cell-blocks cell)))]
    [else cell]))

(check-expect (update-block-for-tnt (make-cell gp0 (list BLOCK-WA)) (make-posn 1 0))
              (make-cell gp0 '()))
(check-expect (update-block-for-tnt (make-cell gp0 (list BLOCK-WA (make-tnt 0))) (make-posn 1 0))
              (make-cell gp0 (list (make-tnt 0))))
(check-expect (update-block-for-tnt (make-cell gp0 (list BLOCK-WA)) (make-posn 99 99))
              (make-cell gp0 (list BLOCK-WA)))

; posn=? : Posn Posn -> Boolean
; Checks if two posns are equal
(define (posn=? p1 p2)
  (and (= (posn-x p1) (posn-x p2))
       (= (posn-y p1) (posn-y p2))))

(check-expect (posn=? (make-posn 0 0) (make-posn 0 0)) #t)
(check-expect (posn=? (make-posn 1 0) (make-posn 0 0)) #f)

; explosion : Grid GridPosn -> Grid
; Given an explosion occurs at some GridPosn, update the Grid to have the results of that explosion.
(define (explosion g gp)
  (map (λ (cell) (update-block-for-tnt cell gp)) g))

(check-expect (explosion grid2 (make-posn 1 1))
              (list (make-cell (make-posn 0 0) '())
                    (make-cell (make-posn 0 1) '())
                    (make-cell (make-posn 0 2) '())
                    (make-cell (make-posn 1 0) '())
                    (make-cell (make-posn 1 1) '())
                    (make-cell (make-posn 1 2) '())
                    (make-cell (make-posn 2 0) '())
                    (make-cell (make-posn 2 1) '())
                    (make-cell (make-posn 2 2) '())))

;;; DRAWING FUNCTIONS Exercise 4 ;;;

; draw-world : World -> Image
; Draws the current state of the world
(define (draw-world w)
  (beside (draw-menu (world-player w) (world-grid w)
                     empty-image)
          (draw-player (world-player w)
                       (draw-grid (world-grid w)
                                  empty-image))))

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

(check-expect (gp->posn gp1) (make-posn (* 1 CELL-WIDTH) (* 2 CELL-HEIGHT)))

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

; draw-grid : Grid Player Image -> Image
; Draws the grid of cells.
(define (draw-grid g img)
  (foldr draw-cell img g))

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
; draws a player ontop the given image which must follow the grid system.
(define (draw-player player img)
  (local (
          (define x (- (posn-x (gp->posn (player-pos player)))))
          (define y (- (posn-y (gp->posn (player-pos player))))))
    (overlay/xy (pick-player (player-direction player)) x y img)))

(check-expect (draw-player player1 (empty-scene 100 100))
              (overlay/xy PLAYER-RIGHT 0 0 (empty-scene 100 100)))

(check-expect (draw-player player2 (empty-scene 1050 1050))
              (overlay/xy PLAYER-LEFT (* -1 1 CELL-WIDTH) (* -1 2 CELL-HEIGHT)
                          (empty-scene 1050 1050)))

; Functions related to drawing the menu for the player are below.

; draw-menu : Player Grid Image -> Image
; draws the menu for the given player and grid info onto the given background.
(define (draw-menu p g img)
  (overlay (above (draw-menu-item "Your Score" (draw-score (player-score p)))
                  (draw-menu-item "Your Direction" (draw-direction (player-direction p)))
                  (draw-menu-item "Beneath You" (draw-materials (top-blocks (player-pos p) g)))
                  (draw-menu-item "Selected Material" (draw-block (player-selected p))))
           img))

(check-expect (draw-menu player1 grid1 empty-image)
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
  (truncate (cell-blocks (search-for-cell gp g)) STANDING-SEARCH-DEPTH))

(check-expect (top-blocks gp0 grid1) (list BLOCK-WA))
(check-expect (top-blocks gp1 (list (make-cell (make-posn 0 0) (list BLOCK-WA))
                                    (make-cell (make-posn 1 2) (make-list 5 BLOCK-WA))))
              (make-list STANDING-SEARCH-DEPTH BLOCK-WA))

; truncate : [List-of X] Natural -> [List-of X]
; returns a list truncated to have a given max length.
(define (truncate l depth)
  (cond [(or (zero? depth) (empty? l)) '()]
        [(positive? depth) (cons (first l) (truncate (rest l) (sub1 depth)))]))

(check-expect (truncate (make-list 10 "TEST") 2) (list "TEST" "TEST"))
(check-expect (truncate (make-list 3 "TEST") 4) (list "TEST" "TEST" "TEST"))
(check-expect (truncate '() 4) '())

; search-for-cell : GridPosn Grid -> Cell
; Using the given position, finds a matching cell in the grid.
(define (search-for-cell gp g)
  (local (; matching-cell? : Cell -> Boolean
          ; returns true if the cell has a matching position.
          (define (matching-cell? cell)
            (and (= (posn-x (cell-pos cell)) (posn-x gp))
                 (= (posn-y (cell-pos cell)) (posn-y gp)))))
    (first (filter matching-cell? g))))

; The first cell is the head of the grid list.
(check-expect (search-for-cell gp0 grid1) (first grid1))

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
  w)

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

;(main 20)