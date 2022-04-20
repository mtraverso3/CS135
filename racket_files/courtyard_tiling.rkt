#lang racket


;;A tile contains a (x,y) value and a `value` for coloring
;; (tile x y value) -> ((x,y), value)
;(define (tromino x y value)
 ; (list (list x y) value)
;);

;; checks if a < value < b
(define (in-range? a b value)
  (and (<= a value) (> b value))
)

;;returns only the covered tiles within the bounds given
(define (filter-yard yard currOrigin currSize)
  (for/list ([tile yard]
             #:when (and (in-range? (car currOrigin)  (+ currSize (car currOrigin))  (car tile))
                         (in-range? (cadr currOrigin) (+ currSize (cadr currOrigin)) (cadr tile))
                    )
            )
             tile
    )
)

;; finds the quadrant that contains a missing tile
;; 2 3 ^
;; 0 1 ->
(define (find-missing yard currOrigin currSize)
  (let* ([tile (car (filter-yard yard currOrigin currSize))] [x (car tile)] [y (cadr tile)])
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

;;returns the center tile, given origin and size
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
         )
    (append
      (cond
        [(equal? quadrant 2) (list (list x y r)        (list x (sub1 y) r)    (list (sub1 x) (sub1 y) r))]
        [(equal? quadrant 3) (list (list x (sub1 y) r) (list (sub1 x) y r)    (list (sub1 x) (sub1 y) r))] 
        [(equal? quadrant 0) (list (list x y r)        (list (sub1 x) y r)    (list x (sub1 y) r))] 
        [(equal? quadrant 1) (list (list x y r)        (list (sub1 x) y r)    (list (sub1 x) (sub1 y) r))] 
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

(define (tileYard missingTile n)
  (remove-duplicates (recTile (list missingTile) '(0 0) (expt 2 n)))
  )
;;TEST cases

;; (recTile '((1 1)) '(0 0) 4) ;;
;; (tileYard '(1 1) 2) ;;test 4x4 courtyard
;; (place-tile-center '((1 2)) '(0 2) 2 1) ;;test base 2x2 case
;; (find-missing '((1 2)) '(0 2) 2) ;;test finding quadrant for 2x2 case
