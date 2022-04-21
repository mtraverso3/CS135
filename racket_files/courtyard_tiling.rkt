#lang racket
(require racket/draw)

 (require graphics/graphics)

;;------------Tile definitions---------

;;A tile contains a (x,y) value and a `value` for coloring
(define (tile x y uid color)
  (list (list x y) uid color)
)

;;get x coordinate for a tile
(define (getTileCoord tile)
  (car tile)
)


;; --------- Coordinate Definitions --------------

;;A tile contains a (x,y) value and a `value` for coloring
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


;;----------------------------


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


;;recursively places trominoes on the grid, quadrant by quadrant
(define (recTile missingTile currOrigin currSize)
  (if (equal? currSize 2)
      (list
           (createIfNeeded currOrigin currSize 0 missingTile) ;;quad 0
           (createIfNeeded currOrigin currSize 1 missingTile) ;;quad 1
           (createIfNeeded currOrigin currSize 2 missingTile) ;;quad 2
           (createIfNeeded currOrigin currSize 3 missingTile) ;;quad 3
         )
      (let* (
             [newSize (/ currSize 2)]
            )
        (append
           (recTile (createIfNeeded currOrigin currSize 0 missingTile) currOrigin newSize) ;;quad 0
           (recTile (createIfNeeded currOrigin currSize 1 missingTile) (list (+ (car currOrigin) newSize) (cadr currOrigin)) newSize) ;;quad 1
           (recTile (createIfNeeded currOrigin currSize 2 missingTile) (list (car currOrigin) (+ (cadr currOrigin) newSize)) newSize) ;;quad 2
           (recTile (createIfNeeded currOrigin currSize 3 missingTile) (list (+ (car currOrigin) newSize) (+ (cadr currOrigin) newSize)) newSize) ;;quad 3
         ) 
      )
  )
)


;; TODO: Fix duplicates
;;calls the recursive tiler with starting values and removes duplicates 
(define (tileYard missingTile n)
  (recTile (tile (car missingTile) (cadr missingTile) "0" "black") '(0 0) (expt 2 n))
  )

;; ------------------------------------ Imaging ----------------------------

(define (paintRemaining yard dc n)
  (if (equal? yard '())
      #t
      (let* ([aTile (car yard) ] [x (getX (getTileCoord aTile))] [y (- n (getY (getTileCoord aTile)))])
        
        (send dc set-pixel x y (send the-color-database find-color (caddr aTile)) )
        (paintRemaining (cdr yard) dc n)
    )
  )
)

(define (genCourtyardImage missingTile n)
  (let* (
        [yard (tileYard missingTile n)]
        [target (make-bitmap (expt 2 n) (expt 2 n))]
        [dc (new bitmap-dc% [bitmap target])]
       )
    (paintRemaining yard dc (sub1(expt 2 n)))
    (send target save-file "box.png" 'png)
    )


  )



;;------------------------------ TEST cases ----------------------------------


;; (generate-color '(2 2) currSize 0)
;; (tileYard '(1 1) 2)                      ;;test 4x4 courtyard
;; (place-tile-center '((1 2)) '(0 2) 2 1)  ;;test base 2x2 case
;; (find-missing '((1 2)) '(0 2) 2)         ;;test finding quadrant for base 2x2 case
 (genCourtyardImage '(4 13) 5)
