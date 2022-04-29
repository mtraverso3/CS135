#lang racket

;------------------- Structures ------------------------

(define (createArgument hypotheses conclusion)
    (list hypotheses conclusion)
)

(define (createArgument2 conclusion . hypotheses)
    (list (list conclusion) hypotheses)
)

(define (addHypothesis statement hypotheses )
    (cons statement hypotheses)
)

(define (removeHypothesis statement hypotheses)
    (remove statement hypotheses)
)

;--------------- postfix Stuff (deprecated??) --------------------------------

;; operators: & | ~ -> <- <->

(define (getToken statement)
   (car statement)
)


(define (removeDoubleNot statement)
   (cond
     [(empty? statement) '()]
     [(empty? (cdr statement)) statement]
     [(and (equal? (car statement) "~") (equal? (cadr statement) "~"))  (removeDoubleNot (cddr statement))]
     [else  (cons (car statement) (removeDoubleNot (cdr statement)))]
   )
)

(define (removeDoubleNot statement)
   (cond
     [(empty? statement) '()]
     [(empty? (cdr statement)) statement]
     [(and (equal? (car statement) "~") (equal? (cadr statement) "~"))  (removeDoubleNot (cddr statement))]
     [else  (cons (car statement) (removeDoubleNot (cdr statement)))]
   )
)

(define (notOfStatement statement)
 (removeDoubleNot (append statement "~"))
)
;;https://stackoverflow.com/questions/53924157/parsing-a-text-to-the-tree-in-racket-scheme

; ----------------------------------------------------------------

(define (notOp) '("~"))
(define (orOp) '("|"))
(define (andOp) '("&"))
(define (impOp) '("->"))
(define (iffOp) '("<->"))


; (list '("~") '(E) '(E))

(define (applyNot statement)
    (list '("~") statement)
)

(define (operable? statement)
    (if (equal? (car statement) (notOp))
        (operable? (cadr statement))
        "TODO" ; TODO
    )
)



(define (getOperable statements)
    (if (null? statements)
        #f
        "TODO" ; TODO
    )
)

;------------------- Tree Method ------------------------

(define (contradiciton? statements)
    (if (null? statements)
        #t
        (member? car()
    )
)


(define (matchAndBranch statement)
    (if (car))
    (cond
    []
    []
    []
    )
)


(define (recBrancher statements)
    (let ([statement (getOperable statements)])
        (if (statement)

        statements ;;no operable statements left
        )
    )

)


