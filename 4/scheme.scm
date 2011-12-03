;; -*- coding:utf-8 -*-


(define test '((a . 1)
               (b . 2)
               (c . 3)))


(define global-env `((hello . 1)
                     (+ . ,+)))


(define (lookup-variable-value val env)
  (cdr (assoc val env)))


(define (self-evaluating? sexp)
  (or (string? sexp) (number? sexp)))
      


(define (eval sexp env)
  (cond
   ((self-evaluating? sexp)
    sexp)
   ((symbol? sexp)
    (lookup-variable-value sexp env))
   ((pair? sexp)
    (let ((operator (eval (car sexp) env))
          (operands (map (cut eval <> env) (cdr sexp))))
      (apply operator operands)))
   (else (error "unknown type"))))
