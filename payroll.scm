;(define parseFile TODO complete parseFile)

;(define genList TODO complete genList)

; (define genSal TODO complete genSal)

; (define genHourly TODO complete genHourly)

; (define genComm TODO complete genComm)

(define (readFile inFile)
	(let loop ((lines '()) (next-line (read-line inFile)))
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

(define (usage)
	(define usage1
		"\nUsage: (compute employee_file action)")
	(define usage2
		"Usage: (compute employee_file action operator threshold)")
	(define validActions
		"Valid actions: count print min max total avg")
	(define validOps
		"Valid operations: eq ne gt ge lt le\n")
	(define return
		(string-append usage1 "\nor\n" usage2 "\n\n" validActions "\n" validOps))
	(display return)
	'done
)

(define (argChecking args)
	(cond
		((not (= (length args) 2))
		(usage))
		((not (= (length args) 4))
		(usage))
		(else 'okay)
	)
)

(define (compute . args)
	

	;(define empList
	;	(call-with-input-file (first args) readFile))
)
