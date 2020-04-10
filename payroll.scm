(define-structure (salariedEmp keyword-constructor copier)
	type fName lName salary)
(define-structure (hourlyEmp keyword-constructor copier)
	type fName lName hours rate)
(define-structure (commEmp keyword-constructor copier)
	type fName lName minSal sales commRate)

(define (usage)
	(define _usage1
		"\nUsage: (compute employee_file action)")
	(define _usage2
		"Usage: (compute employee_file action operator threshold)")
	(define _validActions
		"Valid actions: count print min max total avg")
	(define _validOps
		"Valid operations: eq ne gt ge lt le\n")
	(define _return
		(string-append _usage1 "\nor\n" _usage2 "\n\n" _validActions "\n" _validOps))
	(display _return)
	'error
)

(define (readFile inFile)
	(let loop
		((lines '())
		(next-line (read-line inFile)))
		; when we hit the end of file
  		(if (eof-object? next-line)
			; return the lines list
       		(reverse lines)
		; else loop, keeping this line
		(loop (cons next-line lines)
			; and move to next line
            (read-line inFile)))
	)
)

(define (count readList)
	(define _partOne
		"There are ")
	(define _partTwo
		" employees\n")
	(define _empCount
		(number->string (length readList)))
	(define _return
		(string-append _partOne _empCount _partTwo))

	(display _return)
)

(define (makeHourly hourlyString)
	(define _parsedHourly ((string-splitter) hourlyString))
	(make-hourlyEmp
		'type (first _parsedHourly)
		'fName (second _parsedHourly)
		'lName (third _parsedHourly)
		'hours (string->number (fourth _parsedHourly))
		'rate (string->number (fifth _parsedHourly)))
)

(define (makeCommission commString)
	(define _parsedComm ((string-splitter) commString))
	(make-commEmp
		'type (first _parsedComm)
		'fName (second _parsedComm)
		'lName (third _parsedComm)
		'minSal (string->number (fourth _parsedComm))
		'sales (string->number (fifth _parsedComm))
		'commRate (string->number (sixth _parsedComm)))
)

(define (makeSalaried salString)
	(define _parsedSal ((string-splitter) salString)) ;; split string into parts
	(make-salariedEmp
		'type (first _parsedSal)
		'fName (second _parsedSal)
		'lName (third _parsedSal)
		'salary (string->number (fourth _parsedSal)))
)

(define (parseEmps lst)
	(let loop
		((workList lst) ;; assign passed list to var workList
		(empList (list))) ;; create empty list to put employee structs in
	(cond
		((null? workList) (reverse empList)) ; base case
		((eqv? (substring? "salaried" (first workList)) #t) ; if employee is salaried
			(loop (cdr workList) (cons (makeSalaried (first workList)) empList))) ; make salaried employee, add to list
		((eqv? (substring? "hourly" (first readList)) #t) ; if employee is hourly
			(loop (cdr workList) (cons (makeHourly (first work)) empList))) ; make hourly employee, add to list
		((eqv? (substring? "commission" (first workList)) #t) ; if employee is commission
			(loop (cdr workList) (cons (makeCommission (first workList)) empList))) ; make commission employee, add to list
		(else (loop (cdr workList) empList)))) ; else loop
)

(define (printHourly emp)
	(display (hourlyEmp-type emp)) ;; TODO left off here
)

(define (print lst)
	(let loop
		((workList lst))
	(cond
		((null? workList) 'done)
		((eqv? (hourlyEmp? (first workList)) #t)
			(printHourly (first workList)) (loop (cdr workList)))
		(else (loop (cdr workList)))
	)


	)
)

;(define (min lst))

;(define (max lst))

;(define (total lst))

;(define (avg lst))

(define (compute . args)
	;; TODO get arg handling to work correctly
	#|(if (or (not (eqv? (length args) 2)) (not (eqv? (length args) 4))) (usage))|#
	;; contains list of strings, each string representing different employee
	(define _readList (call-with-input-file (first args) readFile))
	;; (display (hourlyEmp-fName (first _hourlyList)))
	;; contains list of employee structures
	(define _empList (parseEmps _readList))
	(display _empList)
	(newline)
	(display (hourlyEmp? (second _empList)))
	(newline)
	(printHourly (second _empList))
	;(print _empList)

	'done
)
