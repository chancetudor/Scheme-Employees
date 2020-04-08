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

(define (count empList)
	(define _partOne
		"There are ")
	(define _partTwo
		" employees\n")
	(define _empCount
		(number->string (length empList)))
	(define _return
		(string-append _partOne _empCount _partTwo))

	(display _return)
)

(define (parse-string str)
	(let ((port (open-input-string str)))
		(parse-string-helper port)
		(close-input-port port)
		'done)
)

(define (parse-string-helper port)
	(let ((stuff (read-string char-set:whitespace port)))
		(if (eof-object? stuff)
		'done
	(begin (display stuff)
               (newline)
               (skip-whitespaces port)
			(parse-string-helper port))))
)

(define (skip-whitespaces port)
	(let ((ch (peek-char port)))
		(if (and (not (eof-object? ch)) (char-whitespace? ch))
			(begin (read-char port)
        		(skip-whitespaces port))))
)

;(define (print empList)

;)

; TODO implement
(define-record-type hourly
	(make-hourly fName lName hours wage)
	hourly?
	(fName fName)
	(lName lName)
	(hours hours)
	(wage wage)
)
; TODO implement
(define-record-type salaried
	(make-hourly fName lName wage)
	salaried?
	(fName fName)
	(lName lName)
	(wage wage)
)
; TODO implement
(define-record-type commission
	(make-hourly fName lName minSal sales commRate)
	hourly?
	(fName fName)
	(lName lName)
	(minSal minSal)
	(sales sales)
	(commRate commRate)
)

(define (readFile inFile)
	(let loop ((lines '())
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

(define (searchList lst type)
	(if (substring? type (first lst)) (first lst))
	(if (null? lst) 'empty
		(searchList (cdr lst) type))
)

(define (compute . args)
	; TODO get arg handling to work correctly
	#|(if (or (not (eqv? (length args) 2)) (not (eqv? (length args) 4)))
		(usage)
	)|#

	(define _empList
		(call-with-input-file (first args) readFile))

	(display _empList)
	(display (searchList _empList "salaried"))
	(newline)


	;(count _empList)
	;(print _empList)

	'done
)
