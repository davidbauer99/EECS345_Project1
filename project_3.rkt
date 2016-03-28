;EECS 345 Project 2
;David Bauer dmb172
;Ryan Nowacoski rmn36 - Boxes, interpret
; M_state-function

;Load the parser
;(load "simpleParser.scm")
(load "functionParser.scm")

(define interpret
  (lambda (f)
    (interpret_parsed (parser f) '((()) (())))))

(define interpret_parsed
  (lambda (statements state)
    (cond
      ((null? statements) state)
      ((atom? state) state)
      (else (M_state (firstStatement statements) state
                     (lambda (s) (interpret_parsed (remaining statements) s))
                     baseBreak baseContinue baseThrow baseReturn)))))

(define baseBreak (lambda (s) (error 'error "Break outside of block")))

(define baseContinue (lambda (s) (error 'error "Continue outside of block")))

(define baseThrow (lambda (v s) (error 'error "Throw outside of try block")))

(define baseReturn (lambda (v s) v))

(define remaining cdr)

(define firstStatement car)

;;;;;;;;;;;;;;;;; M_value functions ;;;;;;;;;;;;;;;;;;
(define M_value
  (lambda (expression state return throw)
    (cond
      ((null? expression) (return '() state))
      ((number? expression) (return expression state))
      ((eq? 'true expression) (return #t state))
      ((eq? 'false expression) (return #f state))
      ((atom? expression) (return (lookup expression state) state)) 
      ((member (operator expression) '(+ - * / %)) (M_value-arith expression state return throw))
      ((member (operator expression) '(&& || ! < > <= >= == !=)) (M_value-boolean expression state return throw))
      ((eq? (operator expression) 'funcall) (M_value-funcall expression state return throw)))))

(define M_value-funcall
  (lambda (expression state return throw)
    (cond
      ((not (eq? (operator expression) 'funcall)) (error "Error: M_value-funcall called without function call."))
      (else (M_state-funcall expression state
                             (lambda (s) (error "Error: non-returning function used as a value."))
                             baseBreak
                             baseContinue
                             throw
                             return)))))

(define M_value-arith
  (lambda (expression state return throw)
    (cond
      ((eq? '+ (operator expression)) (M_value (operand1 expression) state
                                               (lambda (v1 s) (M_value (operand2 expression) s (lambda (v2 s2) (return (+ v1 v2) s2)) throw))
                                               throw))
      ((eq? '- (operator expression)) (if (not (null? (cddr expression)))
                                          (M_value (operand1 expression) state (lambda (v1 s) (M_value (operand2 expression) s
                                                                                                      (lambda (v2 s2) (return (- v1 v2) s2))
                                                                                                      throw))
                                                   throw)
                                          (M_value (operand1 expression) state (lambda (v s) (return (- 0 v) s)) throw)))
      ((eq? '* (operator expression)) (M_value (operand1 expression) state
                                               (lambda (v1 s) (M_value (operand2 expression) s (lambda (v2 s2) (return (* v1 v2) s2)) throw))
                                               throw))
      ((eq? '/ (operator expression)) (M_value (operand1 expression) state
                                               (lambda (v1 s) (M_value (operand2 expression) s (lambda (v2 s2) (return (/ v1 v2) s2)) throw))
                                               throw))
      ((eq? '% (operator expression)) (M_value (operand1 expression) state
                                               (lambda (v1 s) (M_value (operand2 expression) s (lambda (v2 s2) (return (remainder v1 v2) s2)) throw))
                                               throw))
      (else (error 'unknown "unknown expression")))))

(define M_value-boolean
  (lambda (expression state return throw)
      (cond
        ((eq? 'true expression) (return #t state))
        ((eq? 'false expression) (return #f state))
        ;TODO
        ((eq? '&& (operator expression)) (M_value (operand1 expression) state
                                                  (lambda (v s) (M_value (operand2 expression) s
                                                                         (lambda (v2 s2) (return (and v v2) s2)) throw))
                                                  throw))
        ((eq? '|| (operator expression)) (M_value (operand1 expression) state
                                                  (lambda (v s) (M_value (operand2 expression) s
                                                                         (lambda (v2 s2) (return (or v v2) s2)) throw))
                                                  throw))
        ((eq? '! (operator expression)) (M_value (operand1 expression) state (lambda (v s) (return (not v) s)) throw))
        ((eq? '< (operator expression)) (M_value (operand1 expression) state
                                                  (lambda (v s) (M_value (operand2 expression) s
                                                                         (lambda (v2 s2) (return (< v v2) s2)) throw))
                                                  throw))
        ((eq? '> (operator expression)) (M_value (operand1 expression) state
                                                  (lambda (v s) (M_value (operand2 expression) s
                                                                         (lambda (v2 s2) (return (> v v2) s2)) throw))
                                                  throw))
        ((eq? '<= (operator expression)) (M_value (operand1 expression) state
                                                  (lambda (v s) (M_value (operand2 expression) s
                                                                         (lambda (v2 s2) (return (<= v v2) s2)) throw))
                                                  throw))
        ((eq? '>= (operator expression)) (M_value (operand1 expression) state
                                                  (lambda (v s) (M_value (operand2 expression) s
                                                                         (lambda (v2 s2) (return (>= v v2) s2)) throw))
                                                  throw))
        ((eq? '== (operator expression)) (M_value (operand1 expression) state
                                                  (lambda (v s) (M_value (operand2 expression) s
                                                                         (lambda (v2 s2) (return (eq? v v2) s2)) throw))
                                                  throw))
        ((eq? '!= (operator expression)) (M_value (operand1 expression) state
                                                  (lambda (v s) (M_value (operand2 expression) s
                                                                         (lambda (v2 s2) (return (not (eq? v v2)) s2)) throw))
                                                  throw))
        (else (error 'unknown "unknown expression")))))

(define operator car)

(define operand1 cadr)

(define operand2 caddr)

;M_value-return will take a return statement and return the statement right of "return"
(define M_value-return
  (lambda (expression state return throw)
    (M_value (evaluatable expression) state (lambda (v s) (return (convert_value v) s)) throw)))

(define evaluatable cadr)

; converts #t or #f to true and false respectively
(define convert_value
  (lambda (v)
    (cond
      ((null? v) '())
      ((eq? v #t) 'true)
      ((eq? v #f) 'false)
      (else v))))

;;;;;;;;;;;;;;;;;;;;;;; M_state section ;;;;;;;;;;;;;;;;;;
; lamnda (statement state return break continue throw)
(define M_state
  (lambda (statement state next break continue throw return)
    (cond
      ((null? (operation statement)) (error 'error "Empty statement."))
      ((eq? 'var (operation statement)) (M_state-declare statement state next throw))
      ((eq? '= (operation statement)) (M_state-assign statement state next break continue throw return))
      ((eq? 'try (operation statement)) (M_state-try statement state next break continue throw return))
      ((eq? 'begin (operation statement)) (M_state-begin statement state next break continue throw return))
      ((eq? 'throw (operation statement)) (M_state-throw statement state throw))
      ((eq? 'break (operation statement)) (M_state-break statement state break))
      ((eq? 'continue (operation statement)) (M_state-continue statement state continue))
      ((eq? 'return (operation statement)) (M_value-return statement state return throw))
      ((eq? 'if (operation statement)) (M_state-if statement state next break continue throw return))
      ((eq? 'while (operation statement)) (M_state-while statement state next break continue throw return))
      ((eq? 'funcall (operation statement)) (M_state-funcall statement state next baseBreak baseContinue throw (lambda (v s) (next s))))
      (else (error 'error "Unrecognized statement type.")))))

(define M_state-add-frame
  (lambda (state next)
    (next (cons (cons '() (variableList state)) (list (cons '() (valueList state)))))))

(define M_state-pop-frame
  (lambda (state next)
    (next (cons (restOfVariableFrames state) (list (restOfValueFrames state))))))

(define M_state-begin
  (lambda (statement state next break continue throw return)
    (cond
      ((not (eq? (car statement) 'begin)) (error 'error "Begin block without 'begin' at the beginning."))
      (else (M_state-add-frame state (lambda (st) (M_state-block (cdr statement) st
                                                                 (lambda (s) (M_state-pop-frame s (lambda (s2) (next s2))))
                                                                 (lambda (s) (M_state-pop-frame s (lambda (s2) (break s2))))
                                                                 (lambda (s) (M_state-pop-frame s (lambda (s2) (continue s2))))
                                                                 (lambda (v s) (M_state-pop-frame s (lambda (s2) (throw v s2))))
                                                                 (lambda (v s) (M_state-pop-frame s (lambda (s2) (return v s2)))))))))))

(define M_state-funcall
  (lambda (statement state next break continue throw return)
    (cond
      ((not (eq? (car statement) 'funcall)) (error 'error "Function evaluation without 'funcall' at the beginning."))
      (else (M_state-add-frame state (lambda (st) (M_state-declare-func-vars (funcInputs statement) (funcArgs (lookup (funcName statement))) st
                                                                             (lambda (st2) (M_state-block (funcStatements (lookup (funcName statement))) st2
                                                                                                        (lambda (s) (M_state-pop-frame s (lambda (s2) (next s2))))
                                                                                                        (lambda (s) (M_state-pop-frame s (lambda (s2) (break s2))))
                                                                                                        (lambda (s) (M_state-pop-frame s (lambda (s2) (continue s2))))
                                                                                                        (lambda (v s) (M_state-pop-frame s (lambda (s2) (throw v s2))))
                                                                                                        (lambda (v s) (M_state-pop-frame s (lambda (s2) (return v s2))))))
                                                                             throw)))))))

(define funcStatements cadr)

(define funcName cadr)

(define funcInputs cddr)

(define funcArgs car)

(define M_state-declare-func-vars
  (lambda (inputs args state next throw)
    (cond
      ((and (null? inputs) (null? args)) (next state))
      ((null? inputs) (error "Error: not enough inputs for function call."))
      ((null? args) (error "Error: more arguments than inputs for function call."))
      (else (M_state-declare (buildDeclare (firstArg args) (firstInput)) state
                             (lambda (s) (M_state-declare-func-vars (remainingInputs inputs) (remainingArgs args) s next throw)) throw)))))

(define firstArg car)

(define firstInput car)

(define remainingInputs cdr)

(define remainingArgs cdr)

(define M_state-block
  (lambda (statements state next break continue throw return)
    (cond
      ((null? statements) (next state))
      (else (M_state (firstStatement statements) state
                     (lambda (s) (M_state-block (remaining statements) s next break continue throw return))
                     break continue throw return)))))

(define M_state-throw
  (lambda (statement state throw)
    (throw (M_value (throwVar statement) state (lambda (v s) v) throw) state)))

(define M_state-break
  (lambda (statement state break)
    (break state)))

(define M_state-continue
  (lambda (statement state continue)
    (continue state)))

(define throwVar cadr)

(define M_state-try
  (lambda (statement state next break continue throw return)
    (cond
      ((null? (finallyBlock statement))
        (M_state-add-frame state (lambda (st) (M_state-try-catch (tryBlock statement) (catchBlock statement) st
                                                                 (lambda (s) (M_state-pop-frame s (lambda (s2) (next s2))))
                                                                 (lambda (s) (M_state-pop-frame s (lambda (s2) (break s2))))
                                                                 (lambda (s) (M_state-pop-frame s (lambda (s2) (continue s2))))
                                                                 (lambda (v s) (M_state-pop-frame s (lambda (s2) (throw v s2))))
                                                                 (lambda (v s) (M_state-pop-frame s (lambda (s2) (return v s2))))))))
      ((null? (catchList statement))
       (M_state-add-frame state (lambda (st) (M_state-try-finally (tryBlock statement) (finallyStatements statement) st
                                                                 (lambda (s) (M_state-pop-frame s (lambda (s2) (next s2))))
                                                                 (lambda (s) (M_state-pop-frame s (lambda (s2) (break s2))))
                                                                 (lambda (s) (M_state-pop-frame s (lambda (s2) (continue s2))))
                                                                 (lambda (v s) (M_state-pop-frame s (lambda (s2) (throw v s2))))
                                                                 (lambda (v s) (M_state-pop-frame s (lambda (s2) (return v s2)))))))) 
      (else
        (M_state-add-frame state (lambda (st) (M_state-try-catch-finally
                                               (tryBlock statement) (catchBlock statement) (finallyStatements statement) st
                                               (lambda (s) (M_state-pop-frame s (lambda (s2) (next s2))))
                                               (lambda (s) (M_state-pop-frame s (lambda (s2) (break s2))))
                                               (lambda (s) (M_state-pop-frame s (lambda (s2) (continue s2))))
                                               (lambda (v s) (M_state-pop-frame s (lambda (s2) (throw v s2))))
                                               (lambda (v s) (M_state-pop-frame s (lambda (s2) (return v s2)))))))))))

(define catchList caddr)

(define tryBlock cadr)

(define catchBlock cdaddr)

(define finallyStatements
  (lambda (statement)
    (cadr (cadddr statement))))

(define finallyBlock cadddr)

(define M_state-try-catch
  (lambda (try catch state next break continue throw return)
    (M_state-block try state next break continue
                   (lambda (v s) (M_state-catch catch v s next break continue throw return)) return)))

(define M_state-catch
  (lambda (catch value state next break continue throw return)
    (cond
      ((null? value) (error 'error "Null value was thrown."))
      (else (M_state-pop-frame state
                               (lambda (st) (M_state-add-frame st
                                                               (lambda (s) (M_state-declare (buildDeclare (varName catch) value) s
                                                                                            (lambda (s2) (M_state-block (catchStatements catch) s2 next break continue throw return)) throw)))))))))

(define catchStatements cadr)
      

(define buildDeclare
  (lambda (name value)
    (cons 'var (cons name (list value)))))

(define varName caar)

(define M_state-try-catch-finally
  (lambda (try catch finally state next break continue throw return)
    (M_state-block try state
                   (lambda (s) (finally-next-continuation finally s next break continue throw return))
                   (lambda (s) (finally-break-continuation finally s next break continue throw return))
                   (lambda (s) (finally-continue-continuation finally s next break continue throw return))
                   (lambda (v s) (finally-catch-throw-continuation catch v finally s next break continue throw return))
                   (lambda (v s) (finally-return-continuation finally s v next break continue throw return)))))

(define M_state-try-finally
  (lambda (try finally state next break continue throw return)
    (M_state-block try state
                   (lambda (s) (finally-next-continuation finally s next break continue throw return))
                   (lambda (s) (finally-break-continuation finally s next break continue throw return))
                   (lambda (s) (finally-continue-continuation finally s next break continue throw return))
                   (lambda (v s) (finally-throw-continuation finally s v next break continue throw return))
                   (lambda (v s) (finally-return-continuation finally s v next break continue throw return)))))

(define M_state-finally
  (lambda (statements state next break continue throw return)
    (M_state-pop-frame state
                       (lambda (st) (M_state-add-frame st
                                                       (lambda (s) (M_state-block statements s next break continue throw return)))))))

(define finally-next-continuation
  (lambda (finally state next break continue throw return)
    (M_state-finally finally state (lambda (s) (next s)) break continue throw return)))
               
(define finally-break-continuation
  (lambda (finally state next break continue throw return)
    (M_state-finally finally state (lambda (s) (break s)) break continue throw return)))

(define finally-continue-continuation
  (lambda (finally state next break continue throw return)
    (M_state-finally finally state (lambda (s) (continue s)) break continue throw return)))

(define finally-throw-continuation
  (lambda (finally state value next break continue throw return)
    (M_state-finally finally state (lambda (s) (throw value s)) break continue throw return)))

(define finally-catch-throw-continuation
  (lambda (catch value finally state next break continue throw return)
    (M_state-catch catch value state
                   (lambda (s) (finally-next-continuation finally s next break continue throw return))
                   (lambda (s) (finally-break-continuation finally s next break continue throw return))
                   (lambda (s) (finally-continue-continuation finally s next break continue throw return))
                   (lambda (v s) (M_state-finally finally s (lambda (s2) (throw v s2)) break continue throw return))
                   (lambda (v s) (finally-return-continuation finally s v next break continue throw return)))))

(define finally-return-continuation
  (lambda (finally state value next break continue throw return)
    (M_state-finally finally state (lambda (s) (return value s)) break continue throw return)))

(define M_state-while
  (lambda (statement state next break continue throw return)
    (if (M_value (GetCondition statement) state (lambda (v s) v) throw)
        (M_state (GetStatement statement) state
                 (lambda (s) (M_state-while statement s next break continue throw return))
                 (lambda (s) (next s))
                 (lambda (s) (M_state-while statement s next break continue throw return))
                 throw
                 return)
        (next state))))

(define GetStatement caddr)

(define M_state-if
  (lambda (statement state next break continue throw return)
    (cond
      ((M_value (GetCondition statement) state (lambda (v s) v) throw) (M_state (GetThenStatement statement) state next break continue throw return))
      ((not (null? (GetOptElse statement))) (M_state (GetOptElse statement) state next break continue throw return))
      (else (next state)))))

;Helper for if statement to get the first condition
(define GetCondition cadr)

;Helper for if statment to get the "then" statement
(define GetThenStatement caddr)

;Helper for if statement to get the optional else statement
(define GetOptElse
  (lambda (l)
    (if (null? (afterIfBlock l))
        '()
        (elseStatements l))))

(define afterIfBlock cdddr)

(define elseStatements cadddr)

;M_declare_statement will take a list starting with 'var followed by an atom with an optional value
(define M_state-declare
  (lambda (statement state next throw)
    (cond
      ((not (eq? 'var (operation statement))) (error 'illegal "Declaration statment does not start with 'var'"))
      ((null? (declare-value-list statement)) (next (declare_var (declare-var-name statement) state)))
      (else (M_value (declare-val statement) (declare_var (declare-var-name statement) state)
                     (lambda (v s) (update_state (declare-var-name statement) v s (lambda (s2) (next s2)))) throw)))))

(define declare-value-list cddr)

(define declare-val caddr)

(define declare-var-name cadr)

; M_assign takes a statement and state and updates the state with the desired variable assignment.
(define M_state-assign
  (lambda (statement state next break continue throw return)
    (cond
      ((not (eq? '= (operation statement))) (error 'illegal "Assignment statement does not start with '='"))
      (else (M_value (assign-expression statement) state
                     (lambda (v s) (update_state (assign-var statement) v s (lambda (s2) (next s2)))) throw)))))

(define operation car)

(define assign-var cadr)

(define assign-expression caddr)

;;;;;;;;;;;;;;; State manipulation and management ;;;;;;;;;;;;;;;;;;;;;;;
(define lookup
  (lambda (name state)
    (cond
      ((null? state) 'undefined)
      ((empty?  (variables state)) (error 'error "Using variable before it is assigned."))
      ((empty?  (firstVariableFrame state)) (lookup name (cons (remaining_variables state) (list (remaining_values state)))))
      ((and (eq? name (first_variable state)) (eq? 'undefined (first_value state))) (error 'error "Using variable before it is assigned."))
      ((eq? name (first_variable state)) (first_value state))
      (else (lookup name (cons (cons (rest_of_first_varframe state) (restOfVariableFrames state)) (list (cons (rest_of_first_valframe state) (restOfValueFrames state)))))))))
              
(define variables car)

(define first_variable caaar)

(define remaining_variables cdar)

(define state_values cadr)

(define first_value caaadr)

(define remaining_values cdadr)

(define rest_of_first_varframe cdaar)

(define rest_of_first_valframe cdaadr)

(define atom?
  (lambda (expression)
    (not (or (pair? expression) (null? expression)))))

; declare_var adds a new variable to the state with 'undefined as its initial value
(define declare_var
  (lambda (name state)
    (cond
      ((null? state) 'undefined)
      ((contains? name (variableList state)) (error 'error "Duplicate delcaration of variable."))
      (else (cons (cons (cons name (firstVariableFrame state)) (restOfVariableFrames state)) (list (cons (cons 'undefined (firstValueFrame state)) (restOfValueFrames state))))))))

(define contains?
  (lambda (v l)
    (cond
      ((null? l) #f)
      ((eq? v (frontOfList l)) #t)
      ((list? (frontOfList l)) (or (contains? v (frontOfList l)) (contains? v (restOfList l))))
      (else (contains? v (restOfList l))))))

(define frontOfList car)

(define restOfList cdr)

(define variableList car)

(define valueList cadr)

(define firstVariableFrame caar)

(define restOfVariableFrames cdar)

(define firstValueFrame caadr)

(define restOfValueFrames cdadr)

; update_state takes a name and a value and updates that name with the value if it exists in the state
(define update_state_frame
  (lambda (name value state)
    (cond
      ((null? state) 'undefined)
      ((null? (variableList state)) (error 'undefined "Attempting to assign an undeclared variable."))
      ((eq? name (caar state)) (cons (variables state) (list (cons value (remaining_values state)))))
      (else ((lambda (newState)
               (cons (cons (caar state) (variables newState)) (list (cons (caadr state) (state_values newState)))))
             (update_state_frame name value (cons (remaining_variables state) (list (remaining_values state)))))))))

(define update_state
  (lambda (name value state return)
    (cond
      ((null? state) (error 'undefined "Attempting to assign an undeclared variable."))
      ((null? (variableList state)) (error 'undefined "Attempting to assign an undeclared variable."))
      ((contains? name (firstVariableFrame state)) (return (cons (variableList state)
                                                           (list (cons (valueList (update_state_frame name value (firstFrameState state))) (restOfValueFrames state))))))
      (else (update_state name value (cons (restOfVariableFrames state) (list (restOfValueFrames state))) (lambda (s) (return (cons (cons (firstVariableFrame state) (variableList s)) (list (cons (firstValueFrame state) (valueList s)))))))))))

(define firstFrameState
  (lambda (state)
    (cons (firstVariableFrame state) (list (firstValueFrame state)))))