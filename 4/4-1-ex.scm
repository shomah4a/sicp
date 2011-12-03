

(define test '((a . 1)
               (b . 2)
               (c . 3)))


(define global-env `((hello . 1)
                     (+ . ,+)))


(define (lookup-variable-value val env)
  (cdr (assoc val env)))


(define (eval sexp env)
  (cond
   ((or (string? sexp) (number? sexp)) sexp)
   ((symbol? sexp)
    (lookup-variable-value sexp env))
   ((pair? sexp)
    (let ((operator (eval (car sexp) env))
          (operands (map (cut eval <> env) (cdr sexp))))
      (apply operator operands)))
   (else (error "unknown type"))))
