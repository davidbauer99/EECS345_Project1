;EECS 345 Project 1
;David Bauer dmb172
;Ryan Nowacoski rmn36

;Load the parser
(load "simpleParser.scm")

(define interpret
  (lambda (f)
    (interpret_parsed (parser f) '(() ()))))

(define interpret_parsed
  (lambda (statements state)
    (cond
      ((null? statements) state)
      ((atom? state) state)
      (else (interpret_parsed (cdr statements) (M_state (car statements) state))))))

;;;;;;;;;;;;;;;;; M_value functions ;;;;;;;;;;;;;;;;;;
(define M_value
  (lambda (expression state)
    (cond
      ((null? expression) '())
      ((number? expression) expression)
      ((eq? 'true expression) #t)
      ((eq? 'false expression) #f)
      ((atom? expression) (lookup expression state)) 
      ((member (operator expression) '(+ - * / %)) (M_value-arith expression state))
      ((member (operator expression) '(&& || ! < > <= >= == !=)) (M_value-boolean expression state)))))

(define M_value-arith
  (lambda (expression state)
    (cond
      ((eq? '+ (operator expression)) (+ (M_value (operand1 expression) state) (M_value (operand2 expression) state)))
      ((eq? '- (operator expression)) (if (not (null? (cddr expression)))
                                            (- (M_value (operand1 expression) state) (M_value (operand2 expression) state))
                                            (- 0 (M_value (operand1 expression) state))))
      ((eq? '* (operator expression)) (* (M_value (operand1 expression) state) (M_value (operand2 expression) state)))
      ((eq? '/ (operator expression)) (quotient (M_value (operand1 expression) state) (M_value (operand2 expression) state)))
      ((eq? '% (operator expression)) (remainder (M_value (operand1 expression) state) (M_value (operand2 expression) state)))            
      (else (error 'unknown "unknown expression")))))

(define M_value-boolean
  (lambda (expression state)
      (cond
        ((eq? '&& (operator expression)) (and (M_value (operand1 expression) state) (M_value (operand2 expression) state)))
        ((eq? '|| (operator expression)) (or (M_value (operand1 expression) state) (M_value (operand2 expression) state)))
        ((eq? '! (operator expression)) (not (M_value (operand1 expression) state))) 
        ((eq? '< (operator expression)) (< (M_value (operand1 expression) state) (M_value (operand2 expression) state)))
        ((eq? '> (operator expression)) (> (M_value (operand1 expression) state) (M_value (operand2 expression) state)))
        ((eq? '<= (operator expression)) (<= (M_value (operand1 expression) state) (M_value (operand2 expression) state)))
        ((eq? '>= (operator expression)) (>= (M_value (operand1 expression) state) (M_value (operand2 expression) state)))
        ((eq? '== (operator expression)) (eq? (M_value (operand1 expression) state) (M_value (operand2 expression) state)))
        ((eq? '!= (operator expression)) (not (eq? (M_value (operand1 expression) state) (M_value (operand2 expression) state))))
        (else (error 'unknown "unknown expression")))))

(define operator car)

(define operand1 cadr)

(define operand2 caddr)

;M_value-return will take a return statement and return the statement right of "return"
(define M_value-return
  (lambda (expression state)
    (convert_value (M_value (cadr expression) state))))

; converts #t or #f to true and false respectively
(define convert_value
  (lambda (v)
    (cond
      ((null? v) '())
      ((eq? v #t) 'true)
      ((eq? v #f) 'false)
      (else v))))

;;;;;;;;;;;;;;;;;;;;;;; M_state section ;;;;;;;;;;;;;;;;;;

(define M_state
  (lambda (statement state)
    (cond
      ((null? (car statement)) (error 'error "Empty statement."))
      ((eq? 'var (car statement)) (M_state-declare statement state))
      ((eq? '= (car statement)) (M_state-assign statement state))
      ((eq? 'return (car statement)) (M_value-return statement state))
      ((eq? 'if (car statement)) (M_state-if statement state))
      ((eq? 'while (car statement)) (M_state-while statement state))
      (else (error 'error "Unrecognized statement type.")))))

(define M_state-while
  (lambda (statement state)
    (if (M_value-boolean (GetCondition statement) state)
        (M_state-while statement (M_state (GetStatement statement) state))
        state)))

(define GetStatement caddr)

(define M_state-if
  (lambda (statement state)
    (cond
      ((M_value (GetCondition statement) state) (M_state (GetThenStatement statement) state))
      ((not (null? (GetOptElse statement))) (M_state (GetOptElse statement) state))
      (else state))))

;Helper for if statement to get the first condition
(define GetCondition cadr)

;Helper for if statment to get the "then" statement
(define GetThenStatement caddr)

;Helper for if statement to get the optional else statement
(define GetOptElse
  (lambda (l)
    (if (null? (cdddr l))
        '()
        (cadddr l))))

;M_declare_statement will take a list starting with 'var followed by an atom with an optional value
(define M_state-declare
  (lambda (statement state)
    (cond
      ((not (eq? 'var (car statement))) (error 'illegal "Declaration statment does not start with 'var'"))
      ((null? (declare-value-list statement)) (declare_var (declare-var-name statement) state))
      (else (update_state (declare-var-name statement) (M_value (declare-val statement) state) (declare_var (declare-var-name statement) state))))))

(define declare-value-list cddr)

(define declare-val caddr)

(define declare-var-name cadr)

; M_assign takes a statement and state and updates the state with the desired variable assignment.
(define M_state-assign
  (lambda (statement state)
    (cond
      ((not (eq? '= (car statement))) (error 'illegal "Assignment statement does not start with '='"))
      (else (update_state (assign-var statement) (M_value (assign-expression statement) state) state)))))

(define assign-var cadr)

(define assign-expression caddr)

;;;;;;;;;;;;;;; State manipulation and management ;;;;;;;;;;;;;;;;;;;;;;;

(define lookup
  (lambda (name state)
    (cond
      ((null? state) 'undefined)
      ((null? (car state)) (error 'error "Using a variable before declaring it."))
      ((and (eq? name (first_variable state)) (eq? (first_value state) 'undefined)) (error 'error "Using variable before it is assigned."))
      ((eq? name (first_variable state)) (first_value state))
      (else (lookup name (cons (remaining_variables state) (list (remaining_values state))))))))

(define variables car)

(define first_variable caar)

(define remaining_variables cdar)

(define state_values cadr)

(define first_value caadr)

(define remaining_values cdadr)

(define atom?
  (lambda (expression)
    (not (or (pair? expression) (null? expression)))))

; declare_var adds a new variable to the state with 'undefined as its initial value
(define declare_var
  (lambda (name state)
    (cond
      ((null? state) 'undefined)
      ((contains? name (car state)) (error 'error "Duplicate delcaration of variable."))
      (else (cons (cons name (variables state)) (list (cons 'undefined (state_values state))))))))

(define contains?
  (lambda (v l)
    (cond
      ((null? l) #f)
      ((eq? v (car l)) #t)
      (else (contains? v (cdr l))))))
        
    

; update_state takes a name and a value and updates that name with the value if it exists in the state
(define update_state
  (lambda (name value state)
    (cond
      ((null? state) 'undefined)
      ((null? (car state)) (error 'undefined "Attempting to assign an undeclared variable."))
      ((eq? name (first_variable state)) (cons (variables state) (list (cons value (remaining_values state)))))
      (else ((lambda (newState)
               (cons (cons (first_variable state) (variables newState)) (list (cons (first_value state) (state_values newState)))))
             (update_state name value (cons (remaining_variables state) (list (remaining_values state)))))))))