;#lang racket

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

(define (matchesOperator? op)
  (or (equal? (orOp) op)
    (or (equal? (andOp) op)
      (or (equal? (impOp) op)
        (equal? (iffOp) op)
      )
    )
  )
)

(define (operable? statement)
  (if (equal? (car statement) (notOp))
    (operable? (cadr statement))
    (matchesOperator? (car statement))
  )
)

(define (getOperable statements)
    (if (null? statements)
        #f
      (if (operable? (car statements)
        (car statements)
        (operable? (cdr statements))
      )
    )
)

(define (applyNot statement)
  (removeDoubleNot (list (notOp) statement))
)

;------------------- Tree Method ------------------------






;---------------------------------------------------------
(define (contradiciton? statements)
  (if (null? statements)
    #f
    (if (not (member? (applyNot (car statements))))
      (contradiction? (cdr statements))
      #t
    )
  )
)

(define (matchAndBranch statement, remaining)
    (if (equal? (notOp) (car statement))
      (cond
        [(equal? (cadar) (orOp)) (handleOR )  ]
        [(equal? (cadar) (andOp))  ]
        [(equal? (cadar) (impOp))  ]
        [(equal? (cadar) (iffOp))  ]
      )
      (cond
        [(equal? (cadar) (orOp)) (handleOR )  ]
        [(equal? (cadar) (andOp))  ]
        [(equal? (cadar) (impOp))  ]
        [(equal? (cadar) (iffOp))  ]
      )
    )
)


(define (recBrancher statements)
    (let (
    [statement (getOperable statements)]
    [remaining (remove statement statements)])
        (if (statement)
          (matchAndBranch statements remaining)
          statements ;;no operable statements left
        )
    )
)