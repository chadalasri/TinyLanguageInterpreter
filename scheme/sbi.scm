#!/afs/cats.ucsc.edu/courses/cmps112-wm/usr/racket/bin/mzscheme -qr
;; $Id: sbi.scm,v 1.10 2019-01-15 14:10:54-08 - - $
;; ;;
;; ;; NAME
;; ;;    sbi.scm - silly basic interpreter
;; ;;
;; ;; SYNOPSIS
;; ;;    sbi.scm filename.sbir
;; ;;
;; ;; DESCRIPTION
;; ;;    The file mentioned in argv[1] is read and assumed to be an SBIR
;; ;;    program, which is the executed.  Currently it is only printed.
;; ;;

(define *stdin* (current-input-port))
(define *stdout* (current-output-port))
(define *stderr* (current-error-port))

;tables, setters and getters
(define *label-table* (make-hash))
(define *function-table* (make-hash))
(define (function-set! key value) (hash-set! *function-table* key value))
(define (function-get! key) (hash-ref *function-table* key))

(for-each 
	(lambda (pair) (function-set! (car pair) (cadr pair)))
		'(
			(/       ,(lambda (x y) (/ (+ x 0.0) (+ y 0.0))))
        		(+       ,+)
        		(-       ,-)
        		(*       ,*)

		)

)




; finds file and runs it from the directory on unix
(define *run-file*
    (let-values
        (((dirpath basepath root?)
            (split-path (find-system-path 'run-file))))
        (path->string basepath))
)

;kills whatever list is put in as the argument
(define (die list)
    (for-each (lambda (item) (display item *stderr*)) list)
    (newline *stderr*)
    (exit 1)
)

;prints correct usage of sbi.scm and kills program
(define (usage-exit)
    (die `("Usage: " ,*run-file* " filename"))
)

;reads the input file as list
(define (readlist-from-inputfile filename)
    (let ((inputfile (open-input-file filename)))
         (if (not (input-port? inputfile))
             (die `(,*run-file* ": " ,filename ": open failed"))
             (let ((program (read inputfile)))
                  (close-input-port inputfile)
                         program))))

;writes the program line by line
(define (set-label-table-call program)
    (for-each (lambda (line) (set-label-table line)) program))

;sets the label table is called
(define (set-label-table line)
	(cond   ((null? (cdr line)) (void))
		((pair? (cadr line)) (void))
		(else (hash-set! label-table (cadr line) (line)))
	)

)

;interpret print statement
;if nothing to print print void
;if only something to print and no expr print the thing
;if there is an expression print the first thing and evaluate the expression
(define (interpret-print printme)
	(cond
		((null? printme)  (void))
		((null? (cdr printme)) (printf "~s~n" (car printme)))
		((not(null? (cdr printme)))
			(printf "~s" (car printme)) 
			(printf "~s~n"(evaluate-expression (cadr printme)))
		)	
	)
)

;(define (binop-interpreter expression)
	;car is function sign
	;cadr is first function
	;caddr is second function
;	(printf "~s~n" (car expression))
;	(evaluate-expression (cadr expression))
;	(evaluate-expression (caddr expression))

;)

;this will evaluate the nested expressions with evaluate statements
;should they also be binop or unop expression
;will be called into the binop-unop separator
;should theoretcially work from the inside taking advantage of schemes inside out built in interpreter
;(define (unop-interpreter expression) 
;	(printf "~s~n" (car expression))
;	(cond
;		((pair? (cadr expression)) (evaluate-expression (cadr expression)))
;		(else (evaluate-expression (caddr expression)))
;	)

;)

;(define (binop-unop-separator expression)
;binop relop, sends expression to binop or unop helper functions 
;car expression holds function
;cadr expression holds the number in unop
;cadr expression holds first expression in binop
;caddr holds the second number in unop
;caddr holds the second expression binop
;must be binop or relop to enter here
	
;	(cond
;		((and (pair? (cadr expression)) (pair? (caddr expression))) (binop-interpreter expression))
;		(else (unop-interpreter expression))
;	)

	;(printf "~s~n~s~n" (cadr expression) (caddr expression))
	
;)
;
;(define (invoke-function expression)

;car holds function symbol
;;cadr holds first term
;;caddr holds second term
;	(let op (car expression))	 
;	(printf "number?: ~s~n" (apply op (cdr expression)))

;)
(define (evaluate-expression expression)
	(if (number? (car expression))
            expression
           (else (evaluate-expression (cdr expression)))
	)


)


;distinguishes between statements and sends them to the correct interpret function
(define (evaluate-statements statement)
	(cond 
		((eq? 'print (car statement)) (interpret-print (cdr statement)))

	)

)

;looks at line to determine if there are statements that need to be evaluated
(define (evaluate-line line)
	;;if cdr is null then skip
	(cond
		
		((null? (cdr line)) void)	;gives list that contains function calls
		((pair? (cadr line)) (evaluate-statements (cadr line)))
	)
)


;interpret program calls evaluate line on each line in the program, sequentially
(define (interpret-program filename program)
    (for-each (lambda (line) (evaluate-line line)) program)
) 

;main
(define (main arglist)
    (if (or (null? arglist) (not (null? (cdr arglist))))
        (usage-exit) ;if arglist is empty
        (let* ((sbprogfile (car arglist)) ;sbprogfile = filename
		;if not
                (program (readlist-from-inputfile sbprogfile)))
              	;start of function call;
		(set-label-table-call program)
		(interpret-program sbprogfile program)
		)))

(printf "terminal-port? *stdin* = ~s~n" (terminal-port? *stdin*))
(if (terminal-port? *stdin*)
    (main (vector->list (current-command-line-arguments)))
    (printf "sbi.scm: interactive mode~n"))

