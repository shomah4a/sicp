(use slib)
(require 'trace)

(define true #t)
(define false #f)

(define (dprint obj)
  (print "debugging... " obj)
  obj)

(define original-apply apply)

(define (apply-in-underlying-scheme proc args)
  (original-apply proc args))


(define (list-of-arg-values exps env)
  (if (no-operands? exps)
      '()
      (cons (actual-value (first-operand exps) env)
            (list-of-arg-values (rest-operands exps)
                                env))))
(define (list-of-delayed-args exps env)
  (if (no-operands? exps)
      '()
      (cons (delay-it (first-operand exps) env)
            (list-of-delayed-args (rest-operands exps)
                                  env))))

;; 関数適用
(define (apply procedure arguments env)
  (cond ((primitive-procedure? procedure)
         (apply-primitive-procedure
          procedure
          (list-of-arg-values arguments env)))  ; changed
        ((compound-procedure? procedure)
         (eval-sequence
          (procedure-body procedure)
          (extend-environment
           (procedure-parameters procedure)
           (list-of-delayed-args arguments env) ; changed
           (procedure-environment procedure))))
        (else
         (error
          "Unknown procedure type -- APPLY" procedure))))

;; リスト評価
(define (list-of-values exps env)
  (if (no-operands? exps)
      '()
      (cons (eval (first-operand exps) env)
            (list-of-values (rest-operands exps) env))))


;; if 式
(define (eval-if exp env)
  (if (true? (actual-value (if-predicate exp) env))
      (eval (if-consequent exp) env)
      (eval (if-alternative exp) env)))


;; 列の列を評価
(define (eval-sequence exps env)
  (cond ((last-exp? exps) (eval (first-exp exps) env))
        (else (actual-value (first-exp exps) env)
              (eval-sequence (rest-exps exps) env))))


;; 値の設定
(define (eval-assignment exp env)
  (set-variable-value! (assignment-variable exp)
                       (eval (assignment-value exp) env)
                       env)
  'ok)


;; 関数定義
(define (eval-definition exp env)
  (define-variable! (definition-variable exp)
    (eval (definition-value exp) env)
    env)
  'ok)


;; 自身が評価値そのものであるかどうか
(define (self-evaluating? exp)
  (cond ((number? exp) true)
        ((string? exp) true)
        ((null? exp) true)
        ((boolean? exp) true)
        (else false)))


;; 変数かどうか。要はシンボル
(define (variable? exp) (symbol? exp))


;; クオートされているリストかどうか
(define (quoted? exp)
  (tagged-list? exp 'quote))

;; クオートされたテキスト (quote
(define (text-of-quotation exp) (cadr exp))


;; タグ付けされたリストかどうか
(define (tagged-list? exp tag)
  (if (pair? exp)
      (eq? (car exp) tag)
      false))


;; アサインする
(define (assignment? exp)
  (tagged-list? exp 'set!))
(define (assignment-variable exp) (cadr exp))
(define (assignment-value exp) (caddr exp))


;; 定義?
(define (definition? exp)
  (tagged-list? exp 'define))


;; 変数定義
(define (definition-variable exp)
  (if (symbol? (cadr exp))
      (cadr exp)
      (caadr exp)))


;; 定数定義
(define (definition-value exp)
  (if (symbol? (cadr exp))
      (caddr exp)
      (make-lambda (cdadr exp)   ; formal parameters
                   (cddr exp)))) ; body


;; 無名関数ってやつ
(define (lambda? exp) (tagged-list? exp 'lambda))
(define (lambda-parameters exp) (cadr exp))
(define (lambda-body exp) (cddr exp))

;; 無名関数を作りましょう
(define (make-lambda parameters body)
  (cons 'lambda (cons parameters body)))

;; if 式を定義しましょう
(define (if? exp) (tagged-list? exp 'if))
(define (if-predicate exp) (cadr exp))
(define (if-consequent exp) (caddr exp))
(define (if-alternative exp)
  (if (not (null? (cdddr exp)))
      (cadddr exp)
      'false))

;; if を作りましょう
(define (make-if predicate consequent alternative)
  (list 'if predicate consequent alternative))

;; begin 式
(define (begin? exp) (tagged-list? exp 'begin))
(define (begin-actions exp) (cdr exp))
(define (last-exp? seq) (null? (cdr seq)))
(define (first-exp seq) (car seq))
(define (rest-exps seq) (cdr seq))

;; let 式
;; (let ((x 10)
;;       (y 20))
;;      (+ x y))
;; ((lambda (x y)
;;          (+ x y)) 10 20)
(define (let? exp)
  (tagged-list? exp 'let))
(define (list-let-params exp)
  (map car (cadr exp)))
(define (list-let-args exp)
  (map cadr (cadr exp)))
(define (list-let-sequence exp)
  (cddr exp))
(define (make-lambda-for-let args seqs)
  (list 'lambda args seqs))
(define (eval-let exp env)
  (let ((params (list-let-params exp))
        (args (list-let-args exp))
        (seqs (list-let-sequence exp)))
    (apply (make-procedure params seqs env) args env)))


(define (let-test)
  (define x
    '(let ((x 10)
           (y 20))
       (+ x y)))
  (print (list-let-params x))
  (print (list-let-args x))
  )

;; 式の列から式へ
(define (sequence->exp seq)
  (cond ((null? seq) seq)
        ((last-exp? seq) (first-exp seq))
        (else (make-begin seq))))
(define (make-begin seq) (cons 'begin seq))

;; 適用可能な式?
(define (application? exp) (pair? exp))
(define (operator exp) (car exp))
(define (operands exp) (cdr exp))
(define (no-operands? ops) (null? ops))
(define (first-operand ops) (car ops))
(define (rest-operands ops) (cdr ops))


;; cond 式
(define (cond? exp) (tagged-list? exp 'cond))
(define (cond-clauses exp) (cdr exp))
(define (cond-else-clause? clause)
  (eq? (cond-predicate clause) 'else))
(define (cond-predicate clause) (car clause))
(define (cond-actions clause) (cdr clause))
(define (cond->if exp)
  (expand-clauses (cond-clauses exp)))

;; 条件式
(define (expand-clauses clauses)
  (if (null? clauses)
      'false                          ; no else clause
      (let ((first (car clauses))
            (rest (cdr clauses)))
        (if (cond-else-clause? first)
            (if (null? rest)
                (sequence->exp (cond-actions first))
                (error "ELSE clause isn't last -- COND->IF"
                       clauses))
            (make-if (cond-predicate first)
                     (sequence->exp (cond-actions first))
                     (expand-clauses rest))))))


;; 条件チェック
(define (true? x)
  (not (eq? x false)))
(define (false? x)
  (eq? x false))


;; procedure
(define (make-procedure parameters body env)
  (list 'procedure parameters body env))
(define (compound-procedure? p)
  (tagged-list? p 'procedure))
(define (procedure-parameters p) (cadr p))
(define (procedure-body p) (caddr p))
(define (procedure-environment p) (cadddr p))


;; 環境
(define (enclosing-environment env) (cdr env))
(define (first-frame env) (car env))
(define the-empty-environment '())


;; スタックフレーム
(define (make-frame variables values)
  (cons variables values))
(define (frame-variables frame) (car frame))
(define (frame-values frame) (cdr frame))
(define (add-binding-to-frame! var val frame)
  (set-car! frame (cons var (car frame)))
  (set-cdr! frame (cons val (cdr frame))))

;; 環境追加
(define (extend-environment vars vals base-env)
  (if (= (length vars) (length vals))
      (cons (make-frame vars vals) base-env)
      (if (< (length vars) (length vals))
          (error "Too many arguments supplied" vars vals)
          (error "Too few arguments supplied" vars vals))))


;; 変数名解決
(define (lookup-variable-value var env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond ((null? vars)
             (env-loop (enclosing-environment env)))
            ((eq? var (car vars))
             (car vals))
            (else (scan (cdr vars) (cdr vals)))))
    (if (eq? env the-empty-environment)
        (error "Unbound variable" var)
        (let ((frame (first-frame env)))
          (scan (frame-variables frame)
                (frame-values frame)))))
  (env-loop env))


;; 値を設定
(define (set-variable-value! var val env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond ((null? vars)
             (env-loop (enclosing-environment env)))
            ((eq? var (car vars))
             (set-car! vals val))
            (else (scan (cdr vars) (cdr vals)))))
    (if (eq? env the-empty-environment)
        (error "Unbound variable -- SET!" var)
        (let ((frame (first-frame env)))
          (scan (frame-variables frame)
                (frame-values frame)))))
  (env-loop env))


;; 変数定義
(define (define-variable! var val env)
  (let ((frame (first-frame env)))
    (define (scan vars vals)
      (cond ((null? vars)
             (add-binding-to-frame! var val frame))
            ((eq? var (car vars))
             (set-car! vals val))
            (else (scan (cdr vars) (cdr vals)))))
    (scan (frame-variables frame)
          (frame-values frame))))


;; 原始的な手続き
(define (primitive-procedure? proc)
  (if (pair? proc)
      (procedure? (cadr proc))
      false))


(define (primitive-procedure-names)
  (map car
       primitive-procedures))

(define (primitive-procedure-objects)
  (map (lambda (proc) (list 'primitive (cadr proc)))
       primitive-procedures))

(define (apply-primitive-procedure proc args)
  (apply-in-underlying-scheme
   (primitive-implementation proc) args))


(define (evaluated-thunk? obj)
  (tagged-list? obj 'evaluated-thunk))

(define (thunk-value evaluated-thunk) (cadr evaluated-thunk))
(define (force-it obj)
  (cond ((thunk? obj)
         (let ((result (actual-value
                        (thunk-exp obj)
                        (thunk-env obj))))
           (set-car! obj 'evaluated-thunk)
           (set-car! (cdr obj) result)  ; replace exp with its value
           (set-cdr! (cdr obj) '())     ; forget unneeded env
           result))
        ((evaluated-thunk? obj)
         (thunk-value obj))
        (else obj)))

(define (delay-it exp env)
  (list 'thunk exp env))

(define (thunk? obj)
  (tagged-list? obj 'thunk))

(define (thunk-exp thunk) (cadr thunk))

(define (thunk-env thunk) (caddr thunk))

(define (force-it obj)
  (if (thunk? obj)
      (actual-value (thunk-exp obj) (thunk-env obj))
      obj))

(define (actual-value exp env)
  (force-it (eval exp env)))


;; 評価
(define (eval exp env)
  (cond ((self-evaluating? exp) exp)
        ((variable? exp) (lookup-variable-value exp env))
        ((quoted? exp) (text-of-quotation exp))
        ((assignment? exp) (eval-assignment exp env))
        ((definition? exp) (eval-definition exp env))
        ((if? exp) (eval-if exp env))
        ((let? exp) (eval-let exp env))
        ((lambda? exp)
         (make-procedure (lambda-parameters exp)
                         (lambda-body exp)
                         env))
        ((begin? exp) 
         (eval-sequence (begin-actions exp) env))
        ((cond? exp) (eval (cond->if exp) env))
        ((application? exp)
         (apply (actual-value (operator exp) env)
                (operands exp)
                env))
        (else
         (error "Unknown expression type -- EVAL" exp))))




;; 4-3 amb

(define (require? exp) (tagged-list? exp 'require))
(define (require-predicate exp) (cadr exp))

(define (an-integer-starting-from n)
  (amb n (an-integer-starting-from (+ n 1))))

(define (amb? exp) (tagged-list? exp 'amb))
(define (amb-choices exp) (cdr exp))

(define (ambeval exp env succeed fail)
  ((analyze exp) env succeed fail))


(define (analyze-self-evaluating exp)
  (lambda (env succeed fail)
    (succeed exp fail)))
(define (analyze-quoted exp)
  (let ((qval (text-of-quotation exp)))
    (lambda (env succeed fail)
      (succeed qval fail))))
(define (analyze-variable exp)
  (lambda (env succeed fail)
    (succeed (lookup-variable-value exp env)
             fail)))
(define (analyze-lambda exp)
  (let ((vars (lambda-parameters exp))
        (bproc (analyze-sequence (lambda-body exp))))
    (lambda (env succeed fail)
      (succeed (make-procedure vars bproc env)
               fail))))


(define (analyze-if exp)
  (let ((pproc (analyze (if-predicate exp)))
        (cproc (analyze (if-consequent exp)))
        (aproc (analyze (if-alternative exp))))
    (lambda (env succeed fail)
      (pproc env
             ;; success continuation for evaluating the predicate
             ;; to obtain pred-value
             (lambda (pred-value fail2)
               (if (true? pred-value)
                   (cproc env succeed fail2)
                   (aproc env succeed fail2)))
             ;; failure continuation for evaluating the predicate
             fail))))


(define (analyze-let exp)
  (let ((params (list-let-params exp))
        (args (list-let-args exp))
        (seqs (list-let-sequence exp)))
    (analyze-application (cons (append `(lambda ,params) seqs) args))))


(define (analyze-sequence exps)
  (define (sequentially a b)
    (lambda (env succeed fail)
      (a env
         ;; success continuation for calling a
         (lambda (a-value fail2)
           (b env succeed fail2))
         ;; failure continuation for calling a
         fail)))
  (define (loop first-proc rest-procs)
    (if (null? rest-procs)
        first-proc
        (loop (sequentially first-proc (car rest-procs))
              (cdr rest-procs))))
  (let ((procs (map analyze exps)))
    (if (null? procs)
        (error "Empty sequence -- ANALYZE"))
    (loop (car procs) (cdr procs))))


(define (analyze-definition exp)
  (let ((var (definition-variable exp))
        (vproc (analyze (definition-value exp))))
    (lambda (env succeed fail)
      (vproc env                        
             (lambda (val fail2)
               (define-variable! var val env)
               (succeed 'ok fail2))
             fail))))


(define (analyze-quire exp)
  (let ((pproc (analyze (require-predicate exp))))
    (lambda (env succeed fail)
      (pproc env
             (lambda (pred-value fail2)
               (if (not (= (eval exp env) pred-value))
                   (succeed 'fail fail)
                   (succeed 'ok fail2)))
             fail))))

(define (analyze-assignment exp)
  (let ((var (assignment-variable exp))
        (vproc (analyze (assignment-value exp))))
    (lambda (env succeed fail)
      (vproc env
             (lambda (val fail2)        ; *1*
               (let ((old-value
                      (lookup-variable-value var env))) 
                 (set-variable-value! var val env)
                 (succeed 'ok
                          (lambda ()    ; *2*
                            (set-variable-value! var
                                                 old-value
                                                 env)
                            (fail2)))))
             fail))))

(define (analyze-application exp)
  (let ((fproc (analyze (operator exp)))
        (aprocs (map analyze (operands exp))))
    (lambda (env succeed fail)
      (fproc env
             (lambda (proc fail2)
               (get-args aprocs
                         env
                         (lambda (args fail3)
                           (execute-application
                            proc args succeed fail3))
                         fail2))
             fail))))


(define (get-args aprocs env succeed fail)
  (if (null? aprocs)
      (succeed '() fail)
      ((car aprocs) env
       ;; success continuation for this aproc
       (lambda (arg fail2)
         (get-args (cdr aprocs)
                   env
                   ;; success continuation for recursive
                   ;; call to get-args
                   (lambda (args fail3)
                     (succeed (cons arg args)
                              fail3))
                   fail2))
       fail)))


(define (execute-application proc args succeed fail)
  (cond ((primitive-procedure? proc)
         (succeed (apply-primitive-procedure proc args)
                  fail))
        ((compound-procedure? proc)
         ((procedure-body proc)
          (extend-environment (procedure-parameters proc)
                              args
                              (procedure-environment proc))
          succeed
          fail))
        (else
         (error
          "Unknown procedure type -- EXECUTE-APPLICATION"
          proc))))


(define (analyze-amb exp)
  (let ((cprocs (map analyze (amb-choices exp))))
    (lambda (env succeed fail)
      (define (try-next choices)
        (if (null? choices)
            (fail)
            ((car choices) env succeed
             (lambda ()
               (try-next (cdr choices))))))
      (try-next cprocs))))


(define (analyze exp)
  (cond ((self-evaluating? exp) (analyze-self-evaluating exp))
        ((variable? exp) (analyze-variable exp))
        ((quoted? exp) (analyze-quoted exp))
        ((assignment? exp) (analyze-assignment exp))
        ((definition? exp) (analyze-definition exp))
        ((if? exp) (analyze-if exp))
        ((cond? exp) (analyze-if (cond->if exp)))
        ((let? exp) (analyze-let exp))
        ((amb? exp) (analyze-amb exp))
        ((lambda? exp) (analyze-lambda exp))
        ((begin? exp) (analyze-sequence (cdr exp)))
        ((application? exp) (analyze-application exp))
        (else
         (error "Unknown expression type -- ANALYZE" exp)))
  )


;; 環境作る

(define (primitive-implementation proc) (cadr proc))


(define primitive-procedures
  `((car ,car)
    (cdr ,cdr)
    (cons ,cons)
    (list ,list)
    (null? ,null?)
    (pair? ,pair?)
    (+ ,+)
    (- ,-)
    (* ,*)
    (/ ,/)
    (= ,=)
    (> ,>)
    (< ,<)
    (remainder ,remainder)
    (modulo ,modulo)
    (eq? ,eq?)
    (not ,not)
    (equal? ,equal?)
    (print ,print)
    (map ,map)
    (debug-print ,dprint)
    (dprint ,dprint)
    ))


(define (setup-environment)
  (let ((initial-env
         (extend-environment (primitive-procedure-names)
                             (primitive-procedure-objects)
                             the-empty-environment)))
    (define-variable! 'true true initial-env)
    (define-variable! 'false false initial-env)
    initial-env))

;; グローバル環境
(define the-global-environment (setup-environment))

(define default-environ the-global-environment)



;; test
(define (test)
  (define (insert-print exp)
    (list 'print exp))
  (define (ev exp) (eval (insert-print exp) default-environ))
  
  (ev '10)
  (ev '(define (aaaa a b)
         (+ a b)))

  (ev '(begin (define (aaaa a b)
                (+ a b))
              (print "bbbbb " (aaaa 10 20))
              ))
  (ev '(begin (define (factorial n)
                (define (fact n m)
                  (if (= n 0)
                      m
                      (fact (- n 1) (* n m))))
                (fact n 1))
              (print "bbbbbbb " (factorial 5))))

  (ev '(begin (define (factorial n t)
                (define (fact n m)
                  (if (= n 0)
                      m
                      (fact (- n 1) (* n m))))
                (fact n 1))
              (print "bbbbbbb " (factorial 5 (/ 1 0)))))
  (ev '(let ((x 10) (y 20)) (+ x y)))
  )


(define (test2)

  (define (insert-print exp)
    (list 'print exp))
  (define (ev exp)
    (ambeval (insert-print exp)
             default-environ
             (lambda (value fail) value)
             (lambda () 'failed)
             ))

  (ev '(print 10))

  (ev '(begin
         (define (test a b)
           (if (null? a)
               b
               (+ a b)))
         (print (test () 20))))
  
  (ev '(begin

(define (square x)
  (* x x))

(define (smallest-divisor n)
  (find-divisor n 2))


(define (find-divisor n test)
  (cond ((> (square test) n) n)
        ((divides? test n) test)
        (else (find-divisor n (+ test 1)))))

(define (divides? x y)
  (= (remainder y x) 0))


(define (runtime)
  (- (time->seconds (current-time)) 1136041200))

(define (prime? n)
  (if (= (smallest-divisor n) n)
      #t
      #f))

         (define (odd? n)
           (= (modulo n 2) 0))
         
         (define (require p)
           (if (not p) (amb)))
         
         (define (an-element-of items)
           (require (not (null? items)))
           (amb (car items) (an-element-of (cdr items))))

         (define (pair a b)
           (require (prime? (+ a b)))
           (list a b))
         
         (define (prime-sum-pair list1 list2)
           (let ((a (an-element-of list1))
                 (b (an-element-of list2)))
           (pair a b)))
         
         (print (prime-sum-pair (list 1 3 5 8) (list 20 35 110)))
         ))
  )

(test2)


