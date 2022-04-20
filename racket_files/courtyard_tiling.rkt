#lang racket
(require racket/draw)


;;------------Tile definitions---------

;;A tile contains a (x,y) value and a `value` for coloring
(define (tile x y uid color)
  (list (list x y) uid color)
)

;;get x coordinate for a tile
(define (getTileX tile)
  (car (car tile))
)

;;get y coordinate for a tile
(define (getTileY tile)
  (cadr (car tile))
)

;;----------------------------

;; checks if a < value < b
(define (in-range? a b value)
  (and (<= a value) (> b value))
)

;;returns only the covered tiles within the bounds given
(define (filter-yard yard currOrigin currSize)
  (for/list ([tile yard]
             #:when (and (in-range? (car currOrigin)  (+ currSize (car currOrigin))  (getTileX tile))
                         (in-range? (cadr currOrigin) (+ currSize (cadr currOrigin)) (getTileY tile))
                    )
            )
             tile
    )
)

;; finds the quadrant that contains a missing tile
;; 2 3 ^
;; 0 1 ->
(define (find-missing yard currOrigin currSize)
  (let* ([tile (car (filter-yard yard currOrigin currSize))] [x (getTileX tile)] [y (getTileY tile)])
    (+
       (if (>= x (+ (car currOrigin)  (/ currSize 2))) 1 0)
       (if (>= y (+ (cadr currOrigin) (/ currSize 2))) 2 0)
    )
  )
)

;; returns a unique id for each tromino, used for differentiating trominoes in the output.
(define (generate-id currOrigin currSize quadrant)
     (format "~v-~v-~v-~v" (car currOrigin) (cadr currOrigin) currSize quadrant)
  )

;; returns a color for each tromino
(define (generate-color currOrigin currSize)
     (cond
       [(not (equal? currSize 2)) "R"]
       [(equal? (modulo (car currOrigin) 4 ) (modulo (cadr currOrigin) 4 ) ) "G"]
       [else "B"]
     )
  )

;;returns the center coordinate, given origin and size
(define (get-center currOrigin currSize)
  (list (+ (car currOrigin) (/ currSize 2)) (+ (cadr currOrigin) (/ currSize 2 ) ) )
  )

;; places trominoes in appropriate spots, given missing quadrant and center
(define (place-tromino-center yard currOrigin currSize quadrant)
  (let* (
           [center (get-center currOrigin currSize)]
           [x (car center)]
           [y (cadr center)]
           [r (generate-id currOrigin currSize quadrant)]
           [c (generate-color currOrigin currSize)]
         )
    (append
      (cond
        [(equal? quadrant 2) (list (tile x y r c)        (tile x (sub1 y) r c)    (tile (sub1 x) (sub1 y) r c))]
        [(equal? quadrant 3) (list (tile x (sub1 y) r c) (tile (sub1 x) y r c)    (tile (sub1 x) (sub1 y) r c))]
        [(equal? quadrant 0) (list (tile x y r c)        (tile (sub1 x) y r c)    (tile x (sub1 y) r c))]
        [(equal? quadrant 1) (list (tile x y r c)        (tile (sub1 x) y r c)    (tile (sub1 x) (sub1 y) r c))]
      )
      yard
    )
  )
)


;;recursively places trominoes on the grid, quadrant by quadrant
(define (recTile yard currOrigin currSize)
  (if (equal? currSize 2)
      (place-tromino-center yard currOrigin currSize (find-missing yard currOrigin currSize))
      (let* (
             [quadrant (find-missing yard currOrigin currSize)]
             [newYard (place-tromino-center yard currOrigin currSize quadrant)]
             [newSize (/ currSize 2)]
            )
        (append
           newYard
           (recTile newYard currOrigin newSize) ;;quad 0
           (recTile newYard (list (+ (car currOrigin) newSize) (cadr currOrigin)) newSize) ;;quad 1
           (recTile newYard (list (car currOrigin) (+ (cadr currOrigin) newSize)) newSize) ;;quad 2 
           (recTile newYard (list (+ (car currOrigin) newSize) (+ (cadr currOrigin) newSize)) newSize) ;;quad 3
         ) 
      )
  )
)


;; TODO: Fix duplicates
;;calls the recursive tiler with starting values and removes duplicates 
(define (tileYard missingTile n)
  (remove-duplicates (recTile (list (tile (car missingTile) (cadr missingTile) "0" "O")) '(0 0) (expt 2 n)))
  )

;; ------------------------------------ Imaging ----------------------------

(define (paintRemaining yard dc)
  (let ([aTile (car yard) ] [x (getX aTile] [y getY aTile])
  (send dc set-pixel )
    )
  )

  
(define (genCourtyardImage missingTile n)
  (let (
        [yard (tileYard missingTile n)]
        [target (make-bitmap (expt 2 n) (expt 2 n))]
        [dc (new bitmap-dc% [bitmap target]]
       )


    )


  )


;;------------------------------ TEST cases ----------------------------------


;; (generate-color '(2 2) currSize 0)
;; (recTile '((1 1)) '(0 0) 4)              ;;test recursive tiler
;; (tileYard '(1 1) 2)                      ;;test 4x4 courtyard
;; (place-tile-center '((1 2)) '(0 2) 2 1)  ;;test base 2x2 case
;; (find-missing '((1 2)) '(0 2) 2)         ;;test finding quadrant for base 2x2 case
