;; structures for each type of employee to hold necessary information
(define-structure (salariedEmp keyword-constructor)
	type fName lName salary)
(define-structure (hourlyEmp keyword-constructor)
	type fName lName hours rate)
(define-structure (commEmp keyword-constructor)
	type fName lName minListSal sales commRate)

(define (usage)
	(define _usage1
		"\nUsage: (compute employee_file action)")
	(define _usage2
		"Usage: (compute employee_file action operator threshold)")
	(define _validActions
		"Valid actions: count print min max total avg")
	(define _validOps
		"Valid operations: eq ne gt ge lt le\n\n")
	(display
		(string-append _usage1 "\nor\n" _usage2 "\n\n" _validActions "\n" _validOps))
	(quit)
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

;; splits a string by given delimiter
;; returns list of strings making up OG string

(define (str-split str ch)
  (let ((len (string-length str)))
    (letrec
      ((split
        (lambda (a b)
          (cond
            ((>= b len) (if (= a b) '() (cons (substring str a b) '())))
              ((char=? ch (string-ref str b)) (if (= a b)
                (split (+ 1 a) (+ 1 b))
                  (cons (substring str a b) (split b b))))
                (else (split a (+ 1 b)))))))
                  (split 0 0)))
)

(define (makeHourly hourlyString)
	(define _parsedHourly (str-split hourlyString #\space))
	(make-hourlyEmp
		'type (first _parsedHourly)
		'fName (second _parsedHourly)
		'lName (third _parsedHourly)
		'hours (string->number (fourth _parsedHourly))
		'rate (string->number (fifth _parsedHourly)))
)

(define (makeCommission commString)
	(define _parsedComm (str-split commString #\space))
	(make-commEmp
		'type (first _parsedComm)
		'fName (second _parsedComm)
		'lName (third _parsedComm)
		'minListSal (string->number (fourth _parsedComm))
		'sales (string->number (fifth _parsedComm))
		'commRate (string->number (sixth _parsedComm)))
)

(define (makeSalaried salString)
	(define _parsedSal (str-split salString #\space))
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
	;; if earnings < minListSal, return minListSal. else return earnings
	(if (< _earnings (commEmp-minListSal emp)) (commEmp-minListSal emp) _earnings)
)

(define (getSalEarning emp)
	(salariedEmp-salary emp)
)

(define (getEarning emp)
	(cond
		((eqv? (getType emp) 'hourly)
			(getHourlyEarning emp))
		((eqv? (getType emp) 'comm)
			(getCommEarning emp))
		((eqv? (getType emp) 'sal)
			(getSalEarning emp))
	)
)

;; helper functions for print() function

(define (printHourly emp)
	(display (string-append
		"Hourly employee: " (hourlyEmp-fName emp) " " (hourlyEmp-lName emp)))
	(newline)
	(display (string-append
		"hours worked: " (number->string (hourlyEmp-hours emp))
		", hourly rate: " (number->string (hourlyEmp-rate emp))))
	(newline)
	(display (string-append "earned $" (number->string (getEarning emp)) "\n"))
	(newline)
)

(define (printSalaried emp)
	(display (string-append
		"Salaried employee: " (salariedEmp-fName emp) " " (salariedEmp-lName emp)))
	(newline)
	(display (string-append
		"weekly salary: " (number->string (getEarning emp))))
	(newline)
	(display (string-append
		"earned $" (number->string (getEarning emp)) "\n"))
	(newline)
)

(define (printCommission emp)
	(display (string-append
		"Commission employee: " (commEmp-fName emp) " " (commEmp-lName emp)))
	(newline)
	(display (string-append
		"min salary: " (number->string (commEmp-minListSal emp))
		", sales amount: " (number->string (commEmp-sales emp))
		", commission rate: " (number->string (* (commEmp-commRate emp) 100)) "%"))
	(newline)
	(display (string-append "earned $" (number->string (getEarning emp)) "\n"))
	(newline)
)

;; the actual meat of the program
;; the actions the user requests
;; list passed has already been cut to include thresh emps only (if required)

(define (count readList)
	(define _partOne "There are ")
	(define _partTwo " employees\n\n")
	(define _empCount (number->string (length readList)))
	(display (string-append _partOne _empCount _partTwo))
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

(define (minList lst)
	(let loop
		((workList lst)
		(minListEarning (getEarning (first lst)))
		(minListEmp (first lst)))
	(cond
		((null? workList)
			(cond
				((eqv? (getType minListEmp) 'hourly) (printHourly minListEmp))
				((eqv? (getType minListEmp) 'sal) (printSalaried minListEmp))
				((eqv? (getType minListEmp) 'comm) (printCommission minListEmp))))
		((< (getEarning (first workList)) minListEarning)
			(loop (cdr workList) (getEarning (first workList)) (first workList)))
		(else
			(loop (cdr workList) minListEarning minListEmp))))
)

(define (maxList lst)
	(let loop
		((workList lst)
		(maxListEarning (getEarning (first lst)))
		(maxListEmp (first lst)))
	(cond
		((null? workList)
			(cond
				((eqv? (getType maxListEmp) 'hourly) (printHourly maxListEmp))
				((eqv? (getType maxListEmp) 'sal) (printSalaried maxListEmp))
				((eqv? (getType maxListEmp) 'comm) (printCommission maxListEmp))))
		((> (getEarning (first workList)) maxListEarning)
			(loop (cdr workList) (getEarning (first workList)) (first workList)))
		(else
			(loop (cdr workList) maxListEarning maxListEmp))))
)

(define (total lst)
	(let loop
		((workList lst)
		(total 0.0))
	(cond
		((null? workList) (display (string-append
			"Total payment is $" (number->string total) "\n" "\n")))
		(else (loop (cdr workList) (+ total (getEarning (first workList)))))))
)

(define (avg lst)
	(define _size (length lst))
	(let loop
		((workList lst)
		(total 0.0))
	(cond
		((null? workList) (display (string-append
			"Average payment per employee is $" (number->string (/ total _size)) "\n" "\n")))
		(else (loop (cdr workList) (+ total (getEarning (first workList)))))))
)

(define _threshVal) ;; global var to use in functions below

(define (_eq? emp) (= (getEarning emp) _threshVal))

(define (ne? emp) (not (= (getEarning emp) _threshVal)))

(define (gt? emp) (> (getEarning emp) _threshVal))

(define (ge? emp) (>= (getEarning emp) _threshVal))

(define (lt? emp) (< (getEarning emp) _threshVal))

(define (le? emp) (<= (getEarning emp) _threshVal))

(define (threshFilter lst op val)
	(set! _threshVal val)
	(cond
		((equal? op "eq") (filter _eq? lst))
		((equal? op "ne") (filter ne? lst))
		((equal? op "gt") (filter gt? lst))
		((equal? op "ge") (filter ge? lst))
		((equal? op "lt") (filter lt? lst))
		((equal? op "le") (filter le? lst))
	)
)

(define (doWork lst action)
	(cond
		((equal? action "count") (count lst))
		((equal? action "print") (print lst))
		((equal? action "min") (minList lst))
		((equal? action "max") (maxList lst))
		((equal? action "total") (total lst))
		((equal? action "avg") (avg lst)))
)

;; main function
(define (compute . args)
	(if (and (not (= (length args) 2)) (not (= (length args) 4))) (usage))
	;; _readList = list of strings, each string representing different employee
	(define _readList (call-with-input-file (first args) readFile))
	;; _empList =  list of employee structures
	(define _empList (parseEmps _readList))
	(if (= (length args) 2) (doWork _empList (second args)))
	(if (= (length args) 4)
		(doWork (threshFilter _empList (third args) (fourth args)) (second args)))
)
