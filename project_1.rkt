; Mvalue for +, -, *, /, %
; ex Mvalue '(3 + 5)
; (value '(3 + 5)) -> 8

(define value
  (lambda (expression)
    (cond
      ((number? expression) expression)
      ((null? (cdr expression)) (value (car expression)))
      ((eq? (cadr expression) '+) (+ (value (car expression)) (value (cddr expression))))
      ((eq? (cadr expression) '-) (- (value (car expression)) (value (cddr expression))))
      ((eq? (cadr expression) '*) (* (value (car expression)) (value (cddr expression))))
      ((eq? (cadr expression) '/) (quotient (value (car expression)) (value (cddr expression))))
      ((eq? (cadr expression) '%) (remainder (value (car expression)) (value (cddr expression)))))))

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
      ((eq? name (caar state)) (caadr state))
      (else (lookup name (cons (cdar state) (list (cdadr state))))))))

(define declare
  (lambda (name state)
    (cond
      ((null? state) 'undefined)
      (else (cons (cons name (car state)) (list (cons 'undefined (cadr state))))))))

(define update_state
  (lambda (name value state)
    (cond
      ((null? state) 'undefined)
      ((null? (car state)) (error 'undefined "Attempting to assign an undefined variable."))
      ((eq? name (caar state)) (cons (car state) (list (cons value (cdadr state)))))
      (else ((lambda (newState)
              (cons (cons (caar state) (car newState)) (list (cons (caadr state) (cadr newState)))))
             (update_state name value (cons (cdar state) (list (cdadr state)))))))))