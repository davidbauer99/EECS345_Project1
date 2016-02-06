(define M_value
        (lambda (expression)
          (cond
            ((number? expression) expression)
            ((eq? '+ (operator expression)) (+ (M_value (operand1 expression)) (M_value (operand2 expression))))
            ((eq? '- (operator expression)) (- (M_value (operand1 expression)) (M_value (operand2 expression))))
            ((eq? '* (operator expression)) (* (M_value (operand1 expression)) (M_value (operand2 expression))))
            ((eq? '/ (operator expression)) (quotient (M_value (operand1 expression)) (M_value (operand2 expression))))
            ((eq? '% (operator expression)) (remainder (M_value (operand1 expression)) (M_value (operand2 expression))))            
            (else (error 'unknown "unknown expression")))))

(define operator car)

(define operand1 cadr)

(define operand2 caddr)

(define M_state-while
  (lambda (condition statement state)
    (if (M_boolean condition state)
        (M_state-while condition statement (M_state statement state))
        state)))

(define lookup
  (lambda (name state)
    (cond
      ((null? state) 'undefined)
      ((eq? name (first_variable state)) (first_value state))
      (else (lookup name (cons (remaining_variables state) (list (remaining_values state))))))))

(define variables car)

(define first_variable caar)

(define remaining_variables cdar)

(define state_values cadr)

(define first_value caadr)

(define remaining_values cdadr)

(define declare
  (lambda (name state)
    (cond
      ((null? state) 'undefined)
      (else (cons (cons name (variables state)) (list (cons 'undefined (state_values state))))))))

(define update_state
  (lambda (name value state)
    (cond
      ((null? state) 'undefined)
      ((null? (car state)) (error 'undefined "Attempting to assign an undefined variable."))
      ((eq? name (first_variable state)) (cons (variables state) (list (cons value (remaining_values state)))))
      (else ((lambda (newState)
              (cons (cons (first_variable state) (variables newState)) (list (cons (first_value state) (state_values newState)))))
             (update_state name value (cons (remaining_variables state) (list (remaining_values state)))))))))