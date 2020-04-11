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
	(exit)
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
	(define _partOne "There are ")
	(define _partTwo " employees\n")
	(define _empCount (number->string (length readList)))
	(display (string-append _partOne _empCount _partTwo))
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
		((eqv? (substring? "hourly" (first workList)) #t) ; if employee is hourly
			(loop (cdr workList) (cons (makeHourly (first workList)) empList))) ; make hourly employee, add to list
		((eqv? (substring? "commission" (first workList)) #t) ; if employee is commission
			(loop (cdr workList) (cons (makeCommission (first workList)) empList))) ; make commission employee, add to list
		(else (loop (cdr workList) empList)))) ; else loop
)

(define (getType emp)
	(cond
		((eqv? (hourlyEmp? emp) #t) 'hourly)
		((eqv? (salariedEmp? emp) #t) 'sal)
		(else 'comm))
)

(define (getHourlyEarning emp)
	(cond
		((<= (hourlyEmp-hours emp) 40)
			(* (hourlyEmp-hours emp) (hourlyEmp-rate emp)))
		((and (> (hourlyEmp-hours emp) 40) (<= (hourlyEmp-hours emp) 50))
			;; ((hours - 40) * (rate * 1.5)) + (40 * rate)
			(+ (* (hourlyEmp-rate emp) 40)
				(* (- (hourlyEmp-hours emp) 40) (* (hourlyEmp-rate emp) 1.5))))
		((> (hourlyEmp-hours emp) 50)
			;; ((hours - 50) * rate * 2) + (10 * (rate * 1.5)) + (rate * 40)
			(+ (+ (* (- (hourlyEmp-hours emp) 50) (* (hourlyEmp-rate emp) 2))
				(* (* (hourlyEmp-rate emp) 1.5) 10))
				(* (hourlyEmp-rate emp) 40))))
)

(define (getCommEarning emp)
	(define _earnings
		(* (commEmp-sales emp) (commEmp-commRate emp)))
	;; if earnings < minSal, return minSal. else return earnings
	(if (< _earnings (commEmp-minSal emp)) (commEmp-minSal emp) _earnings)
)

(define (printHourly emp)
	(display (string-append
		"Hourly employee: " (hourlyEmp-fName emp) " " (hourlyEmp-lName emp)))
	(newline)
	(display (string-append
		"hours worked: " (number->string (hourlyEmp-hours emp))
		", hourly rate: " (number->string (hourlyEmp-rate emp))))
	(newline)
	(display (string-append "earned $" (number->string (getHourlyEarning emp)) "\n"))
	(newline)
)

(define (printSalaried emp)
	(display (string-append
		"Salaried employee: " (salariedEmp-fName emp) " " (salariedEmp-lName emp)))
	(newline)
	(display (string-append
		"weekly salary: " (number->string (salariedEmp-salary emp))))
	(newline)
	(display (string-append
		"earned $" (number->string (salariedEmp-salary emp)) "\n"))
	(newline)
)

(define (printCommission emp)
	(display (string-append
		"Commission employee: " (commEmp-fName emp) " " (commEmp-lName emp)))
	(newline)
	(display (string-append
		"minimum salary: " (number->string (commEmp-minSal emp))
		", sales amount: " (number->string (commEmp-sales emp))
		", commission rate: " (number->string (* (commEmp-commRate emp) 100)) "%"))
	(newline)
	(display (string-append "earned $" (number->string (getCommEarning emp)) "\n"))
	(newline)
)

(define (print lst)
	(let loop
		((workList lst))
	(cond
		((null? workList) 'done)
		((eqv? (getType (first workList)) 'hourly)
			(printHourly (first workList)) (loop (cdr workList)))
		((eqv? (getType (first workList)) 'sal)
			(printSalaried (first workList)) (loop (cdr workList)))
		((eqv? (getType (first workList)) 'comm)
			(printCommission(first workList)) (loop (cdr workList)))
		(else (loop (cdr workList)))))
)

#|(define (min lst)
	(let loop
		((workList lst)
		(minEarning -1)
		(currMin -1))
	(cond
		((null? workList) minEarning)
		((eqv? (hourlyEmp? (first workList)) #t)
			(if (< (getHourlyEarning emp) currMin))
	)
	)
))|#

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
	(print _empList)
	(display (getType (first _empList)))

	'done
)
