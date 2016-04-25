;EECS 345 Project 2
;David Bauer dmb172
;Ryan Nowacoski rmn36

;Class defs in base level of state
;Class declaration ({super/()} (((varnames)) ((varvals))) (((funcnames)) ((funcvals))))
;Instance format   (class (vars in reverse order))

;For looking up fields, use the number of elements from the end to find the var in the instance
; for example Class A has fields (a b) with initial values (1 2) they're stored in an instance as
; (2 1) so b is 0 from the end so it is the element at the 0 index in the instance... Will probably need helper functions

;Load the parser
(load "classParser.scm")

(define interpret
  (lambda (f c)
    (interpret_parsed (parser f) '((()) (())) c)))

(define interpret_parsed
  (lambda (statements state c)
    (cond
      ((null? statements) (M_state-add-frame state (lambda (v) (M_state-block (getMainStatements state c) v (lambda (v2) (error 'error "No return in main.")) baseBreak baseContinue baseThrow baseReturn () ()))))
      ((atom? state) state)
      (else (M_state (firstStatement statements) state
                     (lambda (s) (interpret_parsed (remaining statements) s c))
                     baseBreak baseContinue baseThrow baseReturn () ())))))

(define baseBreak (lambda (s) (error 'error "Break outside of block")))

(define baseContinue (lambda (s) (error 'error "Continue outside of block")))

(define baseThrow (lambda (v) (error 'error "Throw outside of try block")))

(define baseReturn (lambda (v) v))

(define remaining cdr)
 
(define firstStatement car)

(define getMainStatements
  (lambda (state class)
    (if (eq? (lookup class state) 'undefined)
        (error 'error "Specified class is not defined.")
        (cadr (lookup 'main (caddr (lookup class state)))))))

;;;;;;;;;;;;;;;;; M_value functions ;;;;;;;;;;;;;;;;;;
(define M_value
  (lambda (expression state return throw class this)
    (cond
      ((null? expression) (return '()))
      ((number? expression) (return expression))
      ((boolean? expression) (return expression))
      ((eq? 'true expression) (return #t))
      ((eq? 'false expression) (return #f))
      ((atom? expression) (M_lookup expression state class this return))
      ((member (operator expression) '(+ - * / %)) (M_value-arith expression state return throw class this))
      ((member (operator expression) '(&& || ! < > <= >= == !=)) (M_value-boolean expression state return throw class this))
      ((eq? (operator expression) 'funcall) (M_value-funcall expression state return throw class this))
      ((eq? (operator expression) 'new) (M_value-new expression state return class this))
      ((eq? (operator expression) 'dot) (M_object expression state (lambda (oc)
                                                                     (M_field-lookup (caddr expression) state (car oc) (cdr oc) return))
                                                  throw class this))
      ((obj? expression) (return expression)))))

(define M_lookup
  (lambda (exp state class this return)
    (cond
      ((null? (lookup exp state)) (M_field-lookup exp state class this return))
      (else (return (lookup exp state))))))

(define M_field-lookup
  (lambda (f state class this return)
    (return (GetIndex f (caaadr (lookup class state)) (lambda (v) (GetVal (cadr this) v))))))

(define GetIndex
  (lambda (f vars return)
    (cond
      ((null? vars) (error 'error "Variable not found"))
      ((eq? (car vars) f) (return (len (cdr vars))))
      (else (GetIndex f (cdr vars) return)))))

(define len
  (lambda (l)
    (if (null? l)
        0
        (+ 1 (length (cdr l))))))

(define GetVal
  (lambda (vals i)
    (cond
      ((null? vals) (error 'error "Variable not found."))
      ((zero? i) (unbox (car vals)))
      (else (GetVal (cdr vals) (- i 1))))))

(define M_value-funcall
  (lambda (expression state return throw class this)
    (cond
      ((not (eq? (operator expression) 'funcall)) (error "Error: M_value-funcall called without function call."))
      (else (M_state-funcall expression state
                             (lambda (s) (error "Error: non-returning function used as a value."))
                             baseBreak
                             baseContinue
                             throw
                             return class this)))))

(define M_value-arith
  (lambda (expression state return throw class this)
    (cond
      ((eq? '+ (operator expression)) (M_value (operand1 expression) state
                                               (lambda (v1) (M_value (operand2 expression) state (lambda (v2) (return (+ v1 v2))) throw class this))
                                               throw class this))
      ((eq? '- (operator expression)) (if (not (null? (cddr expression)))
                                          (M_value (operand1 expression) state (lambda (v1) (M_value (operand2 expression) state
                                                                                                      (lambda (v2) (return (- v1 v2)))
                                                                                                      throw class this))
                                                   throw class this)
                                          (M_value (operand1 expression) state (lambda (v) (return (- 0 v))) throw class this)))
      ((eq? '* (operator expression)) (M_value (operand1 expression) state
                                               (lambda (v1) (M_value (operand2 expression) state (lambda (v2) (return (* v1 v2))) throw class this))
                                               throw class this))
      ((eq? '/ (operator expression)) (M_value (operand1 expression) state
                                               (lambda (v1) (M_value (operand2 expression) state (lambda (v2) (return (/ v1 v2))) throw class this))
                                               throw class this))
      ((eq? '% (operator expression)) (M_value (operand1 expression) state
                                               (lambda (v1) (M_value (operand2 expression) state (lambda (v2) (return (remainder v1 v2))) throw class this))
                                               throw class this))
      (else (error 'unknown "unknown expression")))))

(define M_value-boolean
  (lambda (expression state return throw class this)
      (cond
        ((eq? 'true expression) (return #t state))
        ((eq? 'false expression) (return #f state))
        ((eq? '&& (operator expression)) (M_value (operand1 expression) state
                                                  (lambda (v) (M_value (operand2 expression) state
                                                                         (lambda (v2) (return (and v v2))) throw class this))
                                                  throw class this))
        ((eq? '|| (operator expression)) (M_value (operand1 expression) state
                                                  (lambda (v) (M_value (operand2 expression) state
                                                                         (lambda (v2) (return (or v v2))) throw class this))
                                                  throw class this))
        ((eq? '! (operator expression)) (M_value (operand1 expression) state (lambda (v) (return (not v))) throw class this))
        ((eq? '< (operator expression)) (M_value (operand1 expression) state
                                                  (lambda (v) (M_value (operand2 expression) state
                                                                         (lambda (v2) (return (< v v2))) throw class this))
                                                  throw class this))
        ((eq? '> (operator expression)) (M_value (operand1 expression) state
                                                  (lambda (v) (M_value (operand2 expression) state
                                                                         (lambda (v2) (return (> v v2))) throw class this))
                                                  throw class this))
        ((eq? '<= (operator expression)) (M_value (operand1 expression) state
                                                  (lambda (v) (M_value (operand2 expression) state
                                                                         (lambda (v2) (return (<= v v2))) throw class this))
                                                  throw class this))
        ((eq? '>= (operator expression)) (M_value (operand1 expression) state
                                                  (lambda (v) (M_value (operand2 expression) state
                                                                         (lambda (v2) (return (>= v v2))) throw class this))
                                                  throw class this))
        ((eq? '== (operator expression)) (M_value (operand1 expression) state
                                                  (lambda (v) (M_value (operand2 expression) state
                                                                         (lambda (v2) (return (eq? v v2))) throw class this))
                                                  throw class this))
        ((eq? '!= (operator expression)) (M_value (operand1 expression) state
                                                  (lambda (v) (M_value (operand2 expression) state
                                                                         (lambda (v2) (return (not (eq? v v2)))) throw class this))
                                                  throw class this))
        (else (error 'unknown "unknown expression")))))

(define M_value-new
  (lambda (expression state return class this)
    (cond
      ((not (eq? (operator expression) 'new)) (error 'error "No new operator."))
      (else (copyFields (lookup (className expression) state) (lambda (v) (return (cons (className expression) (list v)))))))))

(define className cadr)

(define copyFields
  (lambda (class return)
    (cond
      ((null? class) (error 'error "No class def found."))
      (else (copyOf (classFieldList class) (lambda (v) (return v)))))))

(define classFieldList
  (lambda (l)
    (car (cadadr l))))
  

(define superType car)

(define copyOf
  (lambda (fields return)
    (cond
      ((null? fields) (return '()))
      (else (copyOf (cdr fields) (lambda (v) (return (append v (list (box (unbox (car fields))))))))))))
             
(define operator car)

(define operand1 cadr)

(define operand2 caddr)

;M_value-return will take a return statement and return the statement right of "return"
(define M_value-return
  (lambda (expression state return throw class this)
    (M_value (evaluatable expression) state (lambda (v) (return (convert_value v))) throw class this)))

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
  (lambda (statement state next break continue throw return class this)
    (cond
      ((null? (operation statement)) (error 'error "Empty statement."))
      ((eq? 'var (operation statement)) (M_state-declare statement state next throw class this))
      ((eq? '= (operation statement)) (M_state-assign statement state next break continue throw return class this))
      ((eq? 'try (operation statement)) (M_state-try statement state next break continue throw return class this))
      ((eq? 'begin (operation statement)) (M_state-begin statement state next break continue throw return class this))
      ((eq? 'throw (operation statement)) (M_state-throw statement state throw class this))
      ((eq? 'break (operation statement)) (M_state-break statement state break class this))
      ((eq? 'continue (operation statement)) (M_state-continue statement state continue class this))
      ((eq? 'return (operation statement)) (M_value-return statement state return throw class this))
      ((eq? 'if (operation statement)) (M_state-if statement state next break continue throw return class this))
      ((eq? 'while (operation statement)) (M_state-while statement state next break continue throw return class this))
      ((eq? 'funcall (operation statement)) (M_state-funcall statement state next baseBreak baseContinue throw (lambda (v) (next state)) class this))
      ((eq? 'function (operation statement)) (M_state-function statement state next class this))
      ((eq? 'class (operation statement)) (M_state-class statement state next class this))
      (else (error 'error "Unrecognized statement type.")))))

(define M_state-class
  (lambda (statement state next class this)
    (cond
      ((not (eq? (car statement) 'class)) (error 'error "Class definition without 'class' at beginning."))
      (else (declare_var (class-name statement) state (lambda (s) (buildClassBody (body statement) (lambda (s2) (AddSuperFields (class-name statement)
                                                                                                                               s2 state
                                                                                                                               (lambda (s3) (update_state (class-name statement) s3 s (lambda (s4) (next s4)))))))))))))

(define AddSuperFields
  (lambda (cname class state return)
    (cond
      ((null? (car class)) (return class))
      (else (return (AppendFields (lookup (car class) state) class))))))

(define AppendFields
  (lambda (super class)
    (cons (car class) (cons (mergefields (cadr super) (cadr class)) (list (caddr class))))))

(define mergefields
  (lambda (sf cf)
    (cond
      ((null? (caar sf)) cf)
      ((null? (caar cf)) sf)
      (else (cons (list (append (caar cf) (caar sf))) (list (list (append (caadr cf) (caadr sf)))))))))

(define body cddr)

(define buildClassBody
  (lambda (body return)
    (cond
      ((null? body) (return '(((()) (())) ((()) (())))))
      ((eq? (firstElem body) '()) (buildClassBody (elemAfterSuper body) (lambda (v) (return (cons '() v)))))
      ((eq? (firstOperation body) 'extends) (buildClassBody (elemAfterSuper body) (lambda (v) (return (cons (superName body) v)))))
      ((and (eq? (firstOperation body) 'var) (not (null? (cddar body)))) (buildClassBody (remainingBody body) (lambda (v) (declare_var (fieldDecName body) (classVars v) (lambda (s) (update_state (fieldDecName body) (fieldDecVal body) s (lambda (s2) (return (cons s2 (funcVals v))))))))))
      ((eq? (firstOperation body) 'var) (buildClassBody (remainingBody body) (lambda (v) (declare_var (fieldDecName body) (classVars v) (lambda (s) (return (cons s (funcVals v))))))))
      ((eq? (firstOperation body) 'function) (buildClassBody (remainingBody body) (lambda (v) (M_state-function (firstElem body) (fieldVals v) (lambda (s) (return (cons (classVars v) (list s)))) () ()))))
      ((eq? (firstOperation body) 'static-function) (buildClassBody (remainingBody body) (lambda (v) (M_state-function (firstElem body) (fieldVals v) (lambda (s) (return (cons (classVars v) (list s)))) () ())))))))

(define classVars car)

(define funcVals cdr)

(define fieldVals cadr)

(define fieldDecName cadar)

(define fieldDecVal caddar)

(define firstElem car)

(define elemAfterSuper cadr)

(define firstOperation caar)

(define remainingBody cdr)

(define superName cadar)

(define class-name cadr)

(define M_state-add-frame
  (lambda (state next)
    (next (cons (cons '() (variableList state)) (list (cons '() (valueList state)))))))

(define M_state-pop-frame
  (lambda (state next)
    (next (cons (restOfVariableFrames state) (list (restOfValueFrames state))))))

(define M_state-begin
  (lambda (statement state next break continue throw return class this)
    (cond
      ((not (eq? (car statement) 'begin)) (error 'error "Begin block without 'begin' at the beginning."))
      (else (M_state-add-frame state (lambda (st) (M_state-block (cdr statement) st
                                                                 (lambda (s) (M_state-pop-frame s (lambda (s2) (next s2))))
                                                                 (lambda (s) (M_state-pop-frame s (lambda (s2) (break s2))))
                                                                 (lambda (s) (M_state-pop-frame s (lambda (s2) (continue s2))))
                                                                 throw
                                                                 return class this)))))))

(define M_state-function
  (lambda (statement state next class this)
    (declare_var (var-name statement) state (lambda (s) (update_state (var-name statement) (var-value statement) s (lambda (s2) (next s2)))))))

(define var-name cadr)

(define var-value cddr)

(define M_state-funcall
  (lambda (statement state next break continue throw return class this)
    (cond
      ((not (eq? (car statement) 'funcall)) (error 'error "Function evaluation without 'funcall' at the beginning."))
      (else (M_object (objexp statement) state (lambda (oc) (M_value-arg-list (funcInputs statement) state throw
                              (lambda (v) (GenEnvForFunc (funcName (objexp statement)) state
                                                         (lambda (s) (M_state-add-frame s (lambda (s2) (M_state-declare-func-vars v (funcArgs (lookup (funcName (objexp statement)) (classFuncs (GetFuncClass (funcName (objexp statement)) (getClass oc) s2)))) s2
                                                                                                (lambda (st) (M_state-block (funcStatements (lookup (funcName (objexp statement))
                                                                                                                                                    (classFuncs (GetFuncClass (funcName (objexp statement)) (getClass oc) st)))) st
                                                                                                                            (lambda (s4) (next state))
                                                                                                                            baseBreak
                                                                                                                            baseContinue
                                                                                                                            throw
                                                                                                                            return (GetFuncClassName (funcName (objexp statement)) (getClass oc) state) (getInst oc)))
                                                                                                throw class this)))) class this)) class this)) throw class this)))))

(define objexp cadr)

(define classFuncs caddr)

(define getClass car)

(define getInst cdr)

(define GenEnvForFunc
  (lambda (name state next class this)
    (cond
      ((null? (nextFrame state)) (next state))
      ((contains? name (firstVariableFrame state)) (next state))
      (else (GenEnvForFunc name (cons (restOfVariableFrames state) (list (restOfValueFrames state))) next class this)))))

(define nextFrame cdar)

(define M_value-arg-list
  (lambda (inputs state throw cps class this)
    (if (null? inputs)
        (cps '())
        (M_value (firstInput inputs) state (lambda (v) (M_value-arg-list (remainingInputs inputs) state throw (lambda (v2) (cps (cons v v2))) class this)) throw class this))))

(define funcStatements cadr)

(define funcName
  (lambda (func)
    (cond
      ((atom? func) func)
      (else (nameElem func)))))

(define nameElem caddr)

(define funcInputs cddr)

(define funcArgs car)

(define M_state-declare-func-vars
  (lambda (inputs args state next throw class this)
    (cond
      ((and (null? inputs) (null? args)) (next state))
      ((null? inputs) (error "Error: not enough inputs for function call."))
      ((null? args) (error "Error: more arguments than inputs for function call."))
      (else (M_state-declare (buildDeclare (firstArg args) (firstInput inputs)) state
                             (lambda (s) (M_state-declare-func-vars (remainingInputs inputs) (remainingArgs args) s next throw class this)) throw class this)))))

(define firstArg car)

(define firstInput car)

(define remainingInputs cdr)

(define remainingArgs cdr)

(define M_state-block
  (lambda (statements state next break continue throw return class this)
    (cond
      ((null? statements) (next state))
      (else (M_state (firstStatement statements) state
                     (lambda (s) (M_state-block (remaining statements) s next break continue throw return class this))
                     break continue throw return class this)))))

(define M_state-throw
  (lambda (statement state throw class this)
    (M_value (throwVar statement) state (lambda (v) (throw v)) throw class this)))

(define M_state-break
  (lambda (statement state break class this)
    (break state)))

(define M_state-continue
  (lambda (statement state continue class this)
    (continue state)))

(define throwVar cadr)

(define M_state-try
  (lambda (statement state next break continue throw return class this)
    (cond
      ((null? (finallyBlock statement))
        (M_state-add-frame state (lambda (st) (M_state-try-catch (tryBlock statement) (catchBlock statement) st
                                                                 (lambda (s) (M_state-pop-frame s (lambda (s2) (next s2))))
                                                                 (lambda (s) (M_state-pop-frame s (lambda (s2) (break s2))))
                                                                 (lambda (s) (M_state-pop-frame s (lambda (s2) (continue s2))))
                                                                 throw
                                                                 return class this))))
      ((null? (catchList statement))
       (M_state-add-frame state (lambda (st) (M_state-try-finally (tryBlock statement) (finallyStatements statement) st
                                                                 (lambda (s) (M_state-pop-frame s (lambda (s2) (next s2))))
                                                                 (lambda (s) (M_state-pop-frame s (lambda (s2) (break s2))))
                                                                 (lambda (s) (M_state-pop-frame s (lambda (s2) (continue s2))))
                                                                 throw
                                                                 return class this)))) 
      (else
        (M_state-add-frame state (lambda (st) (M_state-try-catch-finally
                                               (tryBlock statement) (catchBlock statement) (finallyStatements statement) st
                                               (lambda (s) (M_state-pop-frame s (lambda (s2) (next s2))))
                                               (lambda (s) (M_state-pop-frame s (lambda (s2) (break s2))))
                                               (lambda (s) (M_state-pop-frame s (lambda (s2) (continue s2))))
                                               throw
                                               return class this)))))))

(define catchList caddr)

(define tryBlock cadr)

(define catchBlock cdaddr)

(define finallyStatements
  (lambda (statement)
    (cadr (cadddr statement))))

(define finallyBlock cadddr)

(define M_state-try-catch
  (lambda (try catch state next break continue throw return class this)
    (M_state-block try state next break continue
                   (lambda (v) (M_state-catch catch v state next break continue throw return class this)) return class this)))

(define M_state-catch
  (lambda (catch value state next break continue throw return class this)
    (cond
      ((null? value) (error 'error "Null value was thrown."))
      (else (M_state-pop-frame state
                               (lambda (st) (M_state-add-frame st
                                                               (lambda (s) (M_state-declare (buildDeclare (varName catch) value) s
                                                                                            (lambda (s2) (M_state-block (catchStatements catch) s2 next break continue throw return class this)) throw class this)))))))))

(define catchStatements cadr)
      

(define buildDeclare
  (lambda (name value)
    (cons 'var (cons name (list value)))))

(define varName caar)

(define M_state-try-catch-finally
  (lambda (try catch finally state next break continue throw return class this)
    (M_state-block try state
                   (lambda (s) (finally-next-continuation finally s next break continue throw return class this))
                   (lambda (s) (finally-break-continuation finally s next break continue throw return class this))
                   (lambda (s) (finally-continue-continuation finally s next break continue throw return class this))
                   (lambda (v) (finally-catch-throw-continuation catch v finally state next break continue throw return class this))
                   return class this)))

(define M_state-try-finally
  (lambda (try finally state next break continue throw return class this)
    (M_state-block try state
                   (lambda (s) (finally-next-continuation finally s next break continue throw return class this))
                   (lambda (s) (finally-break-continuation finally s next break continue throw return class this))
                   (lambda (s) (finally-continue-continuation finally s next break continue throw return class this))
                   (lambda (v) (finally-throw-continuation finally state v next break continue throw return class this))
                   return class this)))

(define M_state-finally
  (lambda (statements state next break continue throw return class this)
    (M_state-pop-frame state
                       (lambda (st) (M_state-add-frame st
                                                       (lambda (s) (M_state-block statements s next break continue throw return class this)))))))

(define finally-next-continuation
  (lambda (finally state next break continue throw return class this)
    (M_state-finally finally state (lambda (s) (next s)) break continue throw return class this)))
               
(define finally-break-continuation
  (lambda (finally state next break continue throw return class this)
    (M_state-finally finally state (lambda (s) (break s)) break continue throw return class this)))

(define finally-continue-continuation
  (lambda (finally state next break continue throw return class this)
    (M_state-finally finally state (lambda (s) (continue s)) break continue throw return class this)))

(define finally-throw-continuation
  (lambda (finally state value next break continue throw return class this)
    (M_state-finally finally state (lambda (s) (throw value)) break continue throw return class this)))

(define finally-catch-throw-continuation
  (lambda (catch value finally state next break continue throw return class this)
    (M_state-catch catch value state
                   (lambda (s) (finally-next-continuation finally s next break continue throw return class this))
                   (lambda (s) (finally-break-continuation finally s next break continue throw return class this))
                   (lambda (s) (finally-continue-continuation finally s next break continue throw return class this))
                   (lambda (v) (M_state-finally finally state (lambda (s2) (throw v)) break continue throw return class this))
                   (lambda (v) (finally-return-continuation finally state v next break continue throw return class this))
                    class this)))

(define finally-return-continuation
  (lambda (finally state value next break continue throw return class this)
    (M_state-finally finally state (lambda (s) (return value s)) break continue throw return class this)))

(define M_state-while
  (lambda (statement state next break continue throw return class this)
    (M_value (GetCondition statement) state (lambda (v) (if v
                                                            (M_state (GetStatement statement) state
                                                                     (lambda (s) (M_state-while statement s next break continue throw return class this))
                                                                     (lambda (s) (next s))
                                                                     (lambda (s) (M_state-while statement s next break continue throw return class this))
                                                                     throw
                                                                     return class this)
                                                            (next state))) throw class this)))

(define GetStatement caddr)

(define M_state-if
  (lambda (statement state next break continue throw return class this)
    (M_value (GetCondition statement) state (lambda (v) (if (eq? v 'true)
                                                            (M_state (GetThenStatement statement) state next break continue throw return class this)
                                                            (M_state-else statement state next break continue throw return class this))) throw class this)))

(define M_state-else
  (lambda (statement state next break continue throw return class this)  
    (cond
      ((not (null? (GetOptElse statement))) (M_state (GetOptElse statement) state next break continue throw return class this))
      (else (next state)))))

(define M_object
  (lambda (objexp state return throw class this)
    (cond
      ((not (pair? objexp)) (return (cons (car this) this)))
      ((eq? (obj objexp) 'super) (M_object-super objexp state return throw class this))
      ((eq? (obj objexp) 'this) (return (cons (classFromInst this) this)))
      (else (M_value (obj objexp) state (lambda (v) (if (obj? v)
                                                  (return (cons (classFromInst v) v))
                                                  (error 'error "Function call on non-object.")))
                     throw class this)))))

(define M_object-super
  (lambda (objexp state return throw class this)
    (if (null? (car (lookup class state)))
        (error 'error "Class has no super.")
        (return (cons (car (lookup class state)) this)))))

(define obj cadr)

(define classFromInst car)

(define obj?
  (lambda (ob)
    (cond
      ((not (pair? ob)) #f)
      (else #t))))

(define GetFuncClass
  (lambda (f class state)
    (cond
      ((null? class) (error 'error "Function not defined."))
      ((contains? f (classFuncFrame (classFuncState (lookup class state)))) (lookup class state))
      (else (GetFuncClass f (superTypeName (lookup class state)) state)))))

(define classFuncState caaddr)

(define classFuncFrame car)

(define superTypeName car)

(define GetFuncClassName
  (lambda (f class state)
    (cond
      ((null? class) (error 'error "Function not defined."))
      ((contains? f (classFuncFrame (classFuncState (lookup class state)))) class)
      (else (GetFuncClassName f (superTypeName (lookup class state)) state)))))

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
  (lambda (statement state next throw class this)
    (cond
      ((not (eq? 'var (operation statement))) (error 'illegal "Declaration statment does not start with 'var'"))
      ((null? (declare-value-list statement)) (declare_var (declare-var-name statement) state (lambda (s) (next s))))
      (else (declare_var (declare-var-name statement) state (lambda (s) (M_value (declare-val statement) s
                                                                                 (lambda (v) (update_state (declare-var-name statement) v s (lambda (s2) (next s2)))) throw class this)))))))

(define declare-value-list cddr)

(define declare-val caddr)

(define declare-var-name cadr)

; M_assign takes a statement and state and updates the state with the desired variable assignment.
(define M_state-assign
  (lambda (statement state next break continue throw return class this)
    (cond
      ((not (eq? '= (operation statement))) (error 'illegal "Assignment statement does not start with '='"))
      ((null? (lookup (assign-var statement) state)) (M_state-field-assign statement state next break continue throw return class this)) 
      (else (M_value (assign-expression statement) state
                     (lambda (v) (update_state (assign-var statement) v state (lambda (s2) (next s2)))) throw class this)))))

(define M_state-field-assign
  (lambda (statement state next break continue throw return class this)
    (if (list? (assign-var statement))
        (M_object (assign-var statement) state (lambda (oc) (M_value (assign-expression statement) state
                                                                     (lambda (v) (M_field-assign (caddr (assign-var statement))
                                                                                                 v
                                                                                                 state next break continue
                                                                                                 throw return (car oc) (cdr oc)))
                                                                     throw class this))
                  throw class this)
        (M_object (assign-var statement) state (lambda (oc) (M_value (assign-expression statement) state
                                                                     (lambda (v) (M_field-assign (assign-var statement)
                                                                                                 v
                                                                                                 state next break continue
                                                                                                 throw return (car oc) (cdr oc)))
                                                                     throw class this))
                  throw class this))))
  

(define M_field-assign
  (lambda (f val state next break continue throw return class this)
    (begin (GetIndex f (caaadr (lookup class state)) (lambda (v) (UpdateVal val (cadr this) v))) (next state))))

(define UpdateVal
  (lambda (v vars i)
    (cond
      ((null? vars) (error 'error "Variable not found."))
      ((zero? i) (set-box! (car vars) v))
      (else (UpdateVal v (cdr vars) (- i 1))))))

;(GetIndex f (caaadr (lookup class state)) (lambda (v) (GetVal (cadr this) v))

(define operation car)

(define assign-var cadr)

(define assign-expression caddr)

;;;;;;;;;;;;;;; State manipulation and management ;;;;;;;;;;;;;;;;;;;;;;;
(define lookup
  (lambda (name state)
    (cond
      ((null? state) 'undefined)
      ((empty?  (variables state)) ())
      ((empty?  (firstVariableFrame state)) (lookup name (cons (remaining_variables state) (list (remaining_values state)))))
      ((and (eq? name (first_variable state)) (eq? 'undefined (unbox (first_value state)))) (error 'error "Using variable before it is assigned."))
      ((eq? name (first_variable state)) (unbox (first_value state)))
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
  (lambda (name state cps)
    (cond
      ((null? state) (cps 'undefined))
      ((contains? name (variableList state)) (error 'error "Duplicate delcaration of variable."))
      (else (cps (cons (cons (cons name (firstVariableFrame state)) (restOfVariableFrames state)) (list (cons (cons (box 'undefined) (firstValueFrame state)) (restOfValueFrames state)))))))))

(define contains?
  (lambda (v l)
    (cond
      ((null? l) #f)
      ((eq? v (frontOfList l)) #t)
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
      ((eq? name (firstNameFromFrame state)) (cons (variables state) (list (cons (begin (set-box! (firstValFromFrame state) value) (firstValFromFrame state)) (remaining_values state)))))
      (else ((lambda (newState)
               (cons (cons (firstNameFromFrame state) (variables newState)) (list (cons (firstValFromFrame state) (state_values newState)))))
             (update_state_frame name value (cons (remaining_variables state) (list (remaining_values state)))))))))

(define firstValFromFrame caadr)

(define firstNameFromFrame caar)

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