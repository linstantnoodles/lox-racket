#! /usr/bin/env racket
#lang racket

(require "parser.rkt")
(require "scanner.rkt")
(provide interpret)

(define DEBUG #f)
(define (debug msg)
  (if DEBUG (println msg) '()))

(define (interpret src) 

  ; allowing control to be passed 
  ; upwards
  (struct non-empty-return (value))
  (struct empty-return ())
  (define (raise-non-empty-return value) (raise (non-empty-return value)))
  (define (raise-empty-return) (raise (empty-return '())))

  ; compose a new function to store closures
  ; this needs to be defined in the interpreter because
  ; that's where environments are introduced
  (define (lexical-statement-function statement-function env)
    (list
      (car statement-function)
      (cadr statement-function)
      (caddr statement-function)
      (cadddr statement-function)
      env
      ))

  ; envs have parents
  (define (make-env [parent '()])
    (list (make-hash) parent))

  (define (env-set! env key value)
    (let ([hash (car env)])
      (hash-set! hash key value)))

  (define (env-get env key)
    (debug "===env-get===")
    (debug key)
    (if (not (pair? env))
      (raise (format "undefined variable ~a" key))
      (let (
        [hash (car env)]
        [parent-env (cadr env)]
        )
        (if (hash-has-key? hash key)
          (hash-ref hash key)
          (env-get parent-env key)))))

  (define (env-has-key? env key)
    (let ([hash (car env)])
      (hash-has-key? hash key)))

  (define (evaluate-literal exp env) (cadr exp))

  (define (evaluate-unary exp env)
    (let ([operator-type (caadr exp)]
          [unary-exp (caddr exp)]
         )
      (cond 
        [(equal? operator-type 'MINUS) (- (evaluate unary-exp env))]
        [(equal? operator-type 'BANG) (not (evaluate unary-exp env))]
        [else (raise ("Unrecognized unary operator"))]))
  )

  (define (evaluate-statement-if exp env)
    (let (
      [condition (cadr exp)]
      [if-branch (caddr exp)]
      [else-branch (cadddr exp)] 
    )
      (if (evaluate condition env)
        (evaluate if-branch env)
        (if (not (empty? else-branch)) 
          (evaluate else-branch env) 
          '()))
    ))

  (define (evaluate-statement-while exp env)
    (let (
        [condition (cadr exp)]
        [body (caddr exp)]
      )
      (if (evaluate condition env)
        (begin 
          (evaluate body env)
          (evaluate-statement-while exp env)
        )
        '())
    ))

  (define (evaluate-statement-block exp env)
    (define (recur statement-list env) 
      (if (> (length statement-list) 0)
        (let* (
          [statement (car statement-list)]
          [type (car statement)]
          )
          (if (equal? type 'STATEMENT_VAR)
            (let ([new-env (make-env env)])
               (begin
                  (evaluate statement new-env)
                  (recur (cdr statement-list) new-env)
                ))
            (begin
              (evaluate statement env)
              (recur (cdr statement-list) env)
            )))
         '()))
    (recur (cadr exp) (make-env env)))

  (define (evaluate-statement-print exp env)
    (let ([value (evaluate (cadr exp) env)])
      (println value)))
      
  (define (evaluate-statement-exp exp env)
    (let ([value (evaluate (cadr exp) env)])
      (void)))

  (define (evaluate-variable exp env) (env-get env (cadr exp)))

  (define (evaluate-function-call exp env)
    (define (accumulate-arg-values values args)
      (if (empty? args)
        values 
        (accumulate-arg-values (append values (list (evaluate (car args) env))) (cdr args))))

    (define (bind-params-to-args-in-env params args env)
      (if (empty? params) env
        (let (
          [first-param-name (cadr (car params))]
          [first-arg (car args)]
          )
        (begin
          (env-set! env first-param-name first-arg)
          (bind-params-to-args-in-env (cdr params) (cdr args) env)))))
    (debug "evaluate-function-call==")
    (debug (list-ref (evaluate (cadr exp) env) 4))
    (let* (
      [callee (evaluate (cadr exp) env)]
      [body (cadddr callee)]
      [params  (caddr callee)]
      [closure  (list-ref callee 4)]
      [arg-values (accumulate-arg-values '() (caddr exp))]
      [local-env (bind-params-to-args-in-env params arg-values (make-env closure))]
      )
        (with-handlers (
          [empty-return? (lambda (exn) '())]
          [non-empty-return? (lambda (return) (non-empty-return-value return))]
        )
          (evaluate body local-env))))

  (define (evaluate-statement-var exp env)
    (let ([name (get-token-lexeme (cadr exp))]
          [initializer (caddr exp)])
      (if (empty? initializer)
        (env-set! env name '())
        (env-set! env name (evaluate initializer env)))))

  (define (evaluate-statement-return exp env)
    (let ([value (cadr exp)])
      (if (empty? value)
        (raise-empty-return)
        (raise-non-empty-return (evaluate value env)))))

(define (evaluate-assignment exp env)
  (let ([name (cadr exp)]
        [value (caddr exp)])
        (if (env-has-key? env name)
          (env-set! env name (evaluate value env))
          (if (not (empty? (cadr env)))
            (evaluate-assignment exp (cadr env))
            (raise (cons "Cannot assign to undeclared variable" (list name)))
          ))))
          
  ; exp is the call expression which is name, params, and block/body
  (define (evaluate-statement-fun-declaration exp env)
    (let (
      [name (cadr exp)]
      )
      (env-set! env name (lexical-statement-function exp env))))

  (define (evaluate-binary exp env)
    (let (
          [binary-value-left (evaluate (cadr exp) env)]
          [operator-type (caaddr exp)]
          [binary-value-right (evaluate (cadddr exp) env)]
         )
      (cond 
        [(equal? operator-type 'PLUS)
         (if (and (string? binary-value-left) (string? binary-value-right))
            (string-append binary-value-left binary-value-right)
            (+ binary-value-left binary-value-right))]
        [(equal? operator-type 'MINUS) (- binary-value-left binary-value-right)]
        [(equal? operator-type 'STAR) (* binary-value-left binary-value-right)]
        [(equal? operator-type 'SLASH) (/ binary-value-left binary-value-right)]
        [(equal? operator-type 'LESS) (< binary-value-left binary-value-right)]
        [(equal? operator-type 'LESS_EQUAL) (<= binary-value-left binary-value-right)]
        [(equal? operator-type 'GREATER) (> binary-value-left binary-value-right)]
        [(equal? operator-type 'GREATER_EQUAL) (>= binary-value-left binary-value-right)]
        [else (raise (cons "Unrecognized binary operator " (list operator-type)))])))
  
  (define (evaluate exp env)
      (debug "===evaluation===")
      (debug exp)
      (let ([type (car exp)])
        (cond
          [(equal? type 'STATEMENT_FUN) (evaluate-statement-fun-declaration exp env)]
          [(equal? type 'STATEMENT_IF) (evaluate-statement-if exp env)]
          [(equal? type 'STATEMENT_WHILE) (evaluate-statement-while exp env)]
          [(equal? type 'STATEMENT_BLOCK) (evaluate-statement-block exp env)] 
          [(equal? type 'STATEMENT_PRINT) (evaluate-statement-print exp env)]
          [(equal? type 'STATEMENT_RETURN) (evaluate-statement-return exp env)]
          [(equal? type 'STATEMENT_EXP) (evaluate-statement-exp exp env)]
          [(equal? type 'STATEMENT_VAR) (evaluate-statement-var exp env)]
          [(equal? type 'ASSIGNMENT_EXP) (evaluate-assignment exp env)]
          [(equal? type 'VARIABLE_EXP) (evaluate-variable exp env)]
          [(equal? type 'CALL_EXP) (evaluate-function-call exp env)]
          [(equal? type 'LITERAL_EXP) (evaluate-literal exp env)]
          [(equal? type 'GROUP_EXP) (evaluate (cadr exp) env)]
          [(equal? type 'UNARY_EXP) (evaluate-unary exp env)]
          [(equal? type 'BINARY_EXP) (evaluate-binary exp env)]
          [else (raise (cons "Unrecognized expression " (list type)) #t)]
        )))

  (define (exec-statements statements env)
    (if (empty? statements)
      (void)
      (let* (
        [statement (car statements)]
        [type (car statement)]
      )
      (begin
        (if (equal? type 'STATEMENT_VAR)
          (let ([new-env (make-env env)])
            (begin
              (evaluate statement new-env)
              (exec-statements (cdr statements) new-env)))
          (begin
              (evaluate statement env)
              (exec-statements (cdr statements) env)))
      )
    )))

  (exec-statements (parse src) (make-env))
  #| (println "printing end hash") |#
  #| (println envr) |#
)

;(interpret "(-(1+1))")
;(interpret "(-(2+1))"):w
;(interpret "(-(2*5))")
;(interpret "(1 + 1)")
;(interpret "(\"wow\" + \"hey\")")
; (interpret "var k = 5 + 1; k = 2; print k;")