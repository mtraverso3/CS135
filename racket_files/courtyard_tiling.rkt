#lang racket
(require racket/draw)
(require graphics/graphics)

#|
 | Marcos Traverso
 | I pledge my honor that I have abided by the Stevens Honor System.
 |
 |
 | For info on running, and expected outputs, see bottom section of file (line 175+).
 |#



;;------------Tile definitions----------------------------

;;A tile contains a x and y position, a tromino uid, and a color name for coloring the image
(define (tile x y uid color)
  (list (list x y) uid color)
)

;;get the coordinate of a tile
(define (getTileCoord tile)
  (car tile)
)


;; -------------- Coordinate Definitions ----------------------------

;;A coordinate which contains a (x,y)
(define (coord x y)
  (list x y)
)

;;get x coordinate
(define (getX coord)
  (car coord)
)

;;get y coordinate
(define (getY coord)
  (cadr coord)
)


;;---------------------------- Courtyard Tiler ---------------------


;; returns a unique id for each tromino, used for differentiating trominoes in the output.
(define (generate-id currOrigin currSize quadrant)
     (format "~v-~v-~v-~v" (car currOrigin) (cadr currOrigin) currSize quadrant)
)

;; returns a color for each tromino
(define (generate-color currOrigin currSize)
     (cond
       [(not (equal? currSize 2)) "Tomato"]
       [(equal? (modulo (car currOrigin) 4 ) (modulo (cadr currOrigin) 4 ) ) "Pale Green"]
       [else "CornflowerBlue"]
     )
  )

;;returns the center coordinate, given origin and size
(define (get-center currOrigin currSize)
  (list (+ (car currOrigin) (/ currSize 2)) (+ (cadr currOrigin) (/ currSize 2 ) ) )
  )

;;returns the quadrant a tile is in, given a center
;; 2 3
;; 0 1
(define (getQuad center tile)
   (let (
         [centerX (getX center)]
         [centerY (getY center)]
         [tileX (getX (getTileCoord tile))]
         [tileY (getY (getTileCoord tile))]
         )
   (cond
     [(and (< tileX centerX ) (< tileY centerY)) 0]
     [(and (>= tileX centerX ) (< tileY centerY)) 1]
     [(and (< tileX centerX ) (>= tileY centerY)) 2]
     [(and (>= tileX centerX ) (>= tileY centerY)) 3]
   )
  )
  )


;; retunrs a tile of a tromino to be placed around the center, given a missing tile and info on the courtyard scale
(define (createIfNeeded currOrigin currSize targetQuad existingTile)
  (let* (
        [center (get-center currOrigin currSize)]
        [existingQuad (getQuad center existingTile)])
    (if (equal? existingQuad targetQuad)
        existingTile
        (let ([r (generate-id currOrigin currSize existingQuad)]
              [c (generate-color currOrigin currSize)])
          (cond
          [(equal? targetQuad 0) (tile (sub1 (car center)) (sub1 (cadr center)) r c)]
          [(equal? targetQuad 1) (tile (car center)        (sub1 (cadr center)) r c)]
          [(equal? targetQuad 2) (tile (sub1 (car center)) (cadr center)        r c)]
          [(equal? targetQuad 3) (tile (car center)       (cadr center)        r c)]
          )
        )
    )
  )
)


;;recursively places trominoes on the grid, quadrant by quadrant until its broken down into the 2x2 base case
(define (recTile missingTile currOrigin currSize)
  (if (equal? currSize 2) ;;Base 2x2 case, does not recurse into quadrants
      (list
           (createIfNeeded currOrigin currSize 0 missingTile)
           (createIfNeeded currOrigin currSize 1 missingTile)
           (createIfNeeded currOrigin currSize 2 missingTile)
           (createIfNeeded currOrigin currSize 3 missingTile)
         )
      (let* (
             [newSize (/ currSize 2)]
            )
        (append ;; not base case, recursively tile 4 quadrants
           (recTile (createIfNeeded currOrigin currSize 0 missingTile)  currOrigin newSize)                                                       ;;quad 0
           (recTile (createIfNeeded currOrigin currSize 1 missingTile) (list (+ (car currOrigin) newSize) (cadr currOrigin))   newSize)           ;;quad 1
           (recTile (createIfNeeded currOrigin currSize 2 missingTile) (list (car currOrigin)             (+ (cadr currOrigin) newSize)) newSize) ;;quad 2
           (recTile (createIfNeeded currOrigin currSize 3 missingTile) (list (+ (car currOrigin) newSize) (+ (cadr currOrigin) newSize)) newSize) ;;quad 3
         )
      )
  )
)



;;calls the recursive tiler with starting values
(define (tileYard missingTile n)
  (recTile (tile (car missingTile) (cadr missingTile) "0" "black") '(0 0) (expt 2 n))
)

;; ------------------------------------ Imaging ----------------------------

;;recursively places remaining Tiles as pixels on the bitmap
(define (paintRemaining yard dc n)
  (if (equal? yard '())
      #t
      (let* ([aTile (car yard) ] [x (getX (getTileCoord aTile))] [y (- n (getY (getTileCoord aTile)))])

        (send dc set-pixel x y (send the-color-database find-color (caddr aTile)) )
        (paintRemaining (cdr yard) dc n)
    )
  )
)


;; calls `tileYard` to receive solution, and then `paintRemaining` to draw it to an image file
(define (genCourtyardImage missingTile n)
  (let* (
        [yard (tileYard missingTile n)]
        [target (make-bitmap (expt 2 n) (expt 2 n))]
        [dc (new bitmap-dc% [bitmap target])]
       )
    (paintRemaining yard dc (sub1(expt 2 n)))
    (send target save-file "courtyard.png" 'png)
    )
  )



;;------------------------------ test functions ----------------------------------

;; some methods used to test functionalities

;; (generate-color '(2 2) currSize 0)       ;;test color generation
;; (tileYard '(1 1) 2)                      ;;test 4x4 courtyard
;; (place-tile-center '((1 2)) '(0 2) 2 1)  ;;test base 2x2 case
;; (find-missing '((1 2)) '(0 2) 2)         ;;test finding quadrant for base 2x2 case
;; (genCourtyardImage '(4 7) 5)             ;;test courtyard image generation

;; ------------------------------ Running the Program & Interpreting Output -------------------------------------


#|
 | To get text output, call `tileYard`
 | The output will be formatted as a list of tiles, where each tile is formatted as (coordinate, tromino-id, color).
 | Tiles with the same tromino-id belong to the same tromino, these ids are unique per tromino.
 | Note: coordinates are 0-inclusive, so the bottom left corner tile of the courtyard is (0,0)
 |
 | Usage:
 | (tileYard coordinate n)
 | coordinate -> '(int int)
 | n -> int
 |
 | Example:
 | > (tileYard '(1 3) 2)             ;;tiles a 2^3 x 2^3 yard with a missing tile at (1,3)
 | '(((0 0) "0-0-2-3" "Pale Green")
 |   ((1 0) "0-0-2-3" "Pale Green")
 |   ((0 1) "0-0-2-3" "Pale Green")
 |   ((1 1) "0-0-4-2" "Tomato")
 |   ((2 0) "2-0-2-2" "CornflowerBlue")
 |   ((3 0) "2-0-2-2" "CornflowerBlue")
 |   ((2 1) "0-0-4-2" "Tomato")
 |   ((3 1) "2-0-2-2" "CornflowerBlue")
 |   ((0 2) "0-2-2-3" "CornflowerBlue")
 |   ((1 2) "0-2-2-3" "CornflowerBlue")
 |   ((0 3) "0-2-2-3" "CornflowerBlue")
 |   ((1 3) "0" "black")
 |   ((2 2) "0-0-4-2" "Tomato")
 |   ((3 2) "2-2-2-0" "Pale Green")
 |   ((2 3) "2-2-2-0" "Pale Green")
 |   ((3 3) "2-2-2-0" "Pale Green"))
 |#


#|
 | To get image output, call `genCourtyardImage`
 | The output will be a png file named `courtyard.png` in the current working directory.
 | Note: Since the image size will be 2^n x 2^n pixels, some imaging software will perform blurry upscales.(Working programs: MSPaint, GIMP)
 | Note (again): coordinates are 0-inclusive, so the bottom left corner tile of the courtyard is (0,0)
 |
 | Usage:
 | (genCourtyardImage coordinate n)
 | coordinate -> '(int int)
 | n -> int
 |
 | example:
 | > (genCourtyardImage '(4 6) 3) ;; generates a 2^3 x 2^3 courtyard image with the missing tile/pixel at (4,6), returns #t if it was successful
 | #t
 |#