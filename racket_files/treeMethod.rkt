;#lang racket

;------------------- Structures ------------------------

(define (createArgument hypotheses conclusion)
	(list hypotheses conclusion)
)

(define (createArgument2 conclusion . hypotheses)
	(list (list conclusion) hypotheses)
)

(define (addHypothesis statement hypotheses)
	(cons statement hypotheses)
)

(define (removeHypothesis statement hypotheses)
	(remove statement hypotheses)
)

;;https://stackoverflow.com/questions/53924157/parsing-a-text-to-the-tree-in-racket-scheme

; ----------------------------------------------------------------

;; operators: & | ~ -> <- <->

(define (notOp) "~")
(define (orOp) "|")
(define (andOp) "&")
(define (impOp) "->")
(define (iffOp) "<->")
(define (Ops) (list (orOp) (andOp) (impOp) (iffOp)))

(define (matchesOperator? op)
	(and (member op (Ops)) #t)
)

(define (operable? statement)
	(if (list? statement)
		(if (equal? (car statement) (notOp))
			(operable? (cadr statement))
			(matchesOperator? (car statement))
		)
		#f
	)

)

(define (getOperable statements)
	(if (null? statements)
		#f
		(if (operable? (car statements))
			(car statements)
			(getOperable (cdr statements))
		)
	)
)

(define (removeDoubleNot statement)
	(cond
		[(not (list? statement))                                                                                            statement]
		[(and (equal? (car statement) (notOp)) (not (list? (cadr statement))))                                              statement]
		[(and (equal? (car statement) (notOp)) (equal? (caadr statement) (notOp)))               (removeDoubleNot (cadadr statement))]
		[(equal? (car statement) (notOp))                                           (list (notOp) (removeDoubleNot (cadr statement)))]
		[else                           (list (car statement) (removeDoubleNot (cadr statement)) (removeDoubleNot (caddr statement)))]
	)
)

(define (applyNot statement)
	(removeDoubleNot (list (notOp) statement))
)


(define (contradiction? statements)
	(if (null? statements)
		#f
		(if (not (member (applyNot (car statements)) statements))
			(contradiction? (cdr statements))
			#t
		)
	)
)

; ------------------ Decomposition rules ---------------------------------------
(define (intelligentReturn branchA branchB)
	(cond
		[(and branchA branchB) (list branchA branchB)]
		[(and branchA (not branchB)) branchA]
		[(and (not branchA) branchB) branchB]
		[else #f]
	)
)

(define (handleOr statement remaining)
	(let* (
		      [A (cadr statement)]
		      [B (caddr statement)]
		      [branchA (recBrancher (cons A remaining))]
		      [branchB (recBrancher (cons B remaining))]
	      )
		(intelligentReturn branchA branchB)
	)
)

(define (handleAnd statement remaining)
	(let* (
		      [A (cadr statement)]
		      [B (caddr statement)]
	      )
		(recBrancher (cons B (cons A remaining)))
	)
)

(define (handleImp statement remaining)
	(let* (
		      [A (cadr statement)]
		      [B (caddr statement)]
		      [branchA (recBrancher (cons (applyNot A) remaining))]
		      [branchB (recBrancher (cons B remaining))]
	      )
		(intelligentReturn branchA branchB)
	)
)

(define (handleIff statement remaining)
	(let* (
		      [A (cadr statement)]
		      [B (caddr statement)]
		      [branchA (recBrancher (cons B (cons A remaining)))]
		      [branchB (recBrancher (cons (applyNot B) (cons (applyNot A) remaining)))]
	      )
		(intelligentReturn branchA branchB)
	)
)

(define (handleNotOr statement remaining)
	(let (
		     [A (cadr statement)]
		     [B (caddr statement)]
	     )
		(recBrancher (cons (applyNot B) (cons (applyNot A) remaining)))
	)
)

(define (handleNotAnd statement remaining)
	(let* (
		      [A (cadr statement)]
		      [B (caddr statement)]
		      [branchA (recBrancher (cons (applyNot A) remaining))]
		      [branchB (recBrancher (cons (applyNot B) remaining))]
	      )
		(intelligentReturn branchA branchB)
	)
)
(define (handleNotImp statement remaining)
	(let (
		     [A (cadr statement)]
		     [B (caddr statement)]
	     )
		(recBrancher (cons (applyNot B) (cons A remaining)))
	)
)
(define (handleNotIff statement remaining)
	(let* (
		      [A (cadr statement)]
		      [B (caddr statement)]
		      [branchA (recBrancher (cons (applyNot B) (cons A remaining)))]
		      [branchB (recBrancher (cons B (cons (applyNot A) remaining)))]
	      )
		(intelligentReturn branchA branchB)
	)
)

;------------------- Tree Method ------------------------

(define (matchAndBranch statement remaining) ;TODO
	(if (equal? (notOp) (car statement))
		(cond
			[(equal? (cadar statement) (orOp)) (handleNotOr statement remaining)]
			[(equal? (cadar statement) (andOp)) (handleNotAnd statement remaining)]
			[(equal? (cadar statement) (impOp)) (handleNotImp statement remaining)]
			[(equal? (cadar statement) (iffOp)) (handleNotIff statement remaining)]
		)
		(cond
			[(equal? (car statement) (orOp)) (handleOr statement remaining)]
			[(equal? (car statement) (andOp)) (handleAnd statement remaining)]
			[(equal? (car statement) (impOp)) (handleImp statement remaining)]
			[(equal? (car statement) (iffOp)) (handleIff statement remaining)]
		)
	)
)


(define (recBrancher statements)
	(if (contradiction? statements)
		#f
		(let (
			     [statement (getOperable statements)])
			(if statement
				(matchAndBranch statement (remove statement statements))
				(remove-duplicates statements) ;;no operable statements left
			)
		)
	)
)


;--------------------Test cases ---------------------------

(define (testStatement1) (list (orOp) "A" "B"))
(define (testStatement2) (list (notOp) (list (orOp) "A" "B")))
(define (testStatement3) (list (notOp) "A"))
(define (testStatement4) "A")
(define (testStatement5) (list (notOp) (list (notOp) "A")))
(define (testStatement6) '("~" ("~" ("|" ("|" "A" "B") ("~" ("|" "A" "B"))))))

(define (testHyp1) (list (testStatement1)))
(define (testHyp2) (list (list (orOp) "A" "B") (applyNot "A")))


; (matchesOperator? "&") -> #t
; (matchesOperator? "aaa") -> #f

; (operable? (testStatement1)) -> #t
; (operable? (testStatement2)) -> #t
; (operable? (testStatement3)) -> #f

; (getOperable (list (testStatement1) (testStatement2) (testStatement3) )) -> '("|" "A" "B")
; (getOperable (list (testStatement2) (testStatement1) (testStatement3) )) -> '("~" ("|" "A" "B"))
; (getOperable (list (testStatement3) (testStatement4)) ) -> #f

; (removeDoubleNot (testStatement3)) - > '(("~") ("A"))
; (removeDoubleNot (testStatement5)) - > '("A")

; (contradiction? (list (testStatement1) (applyNot (testStatement1))) ) -> #t
; (contradiction? (list (testStatement1) (testStatement3)) ) -> #f

;---------------------------------------------------------------

(define (hyp1) (list (impOp) "B" "C"))
(define (hyp2) (list (orOp) "D" "C"))
(define (hyp3) (list (iffOp) "B" (applyNot "D")))
(define (conc) (applyNot (list (andOp) "B" "C")))

(define (testArgument1) (recBrancher (list (hyp1) (hyp2) (hyp3) (applyNot (conc)))))