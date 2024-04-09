#! /usr/bin/env racket
#lang racket

(require "parser.rkt")
(require "scanner.rkt")
(provide interpret)

(define (interpret src) 
  (define (make-env [parent '()])
    (list (make-hash) parent))

  (define (env-set! env key value)
    (let ([hash (car env)])
      (hash-set! hash key value)))

  (define (env-get env key)
    (let (
      [hash (car env)]
      [parent-env (cadr env)]
      )
      (if (hash-has-key? hash key)
        (hash-ref hash key)
        (env-get parent-env key))))

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
        (begin
          (evaluate (car statement-list) env)
          (recur (cdr statement-list) env)
        ) '()))
    (recur (cadr exp) (make-env env)))

  (define (evaluate-statement-print exp env)
    (let ([value (evaluate (cadr exp) env)])
      (println value)))
      
  (define (evaluate-statement-exp exp env)
    (let ([value (evaluate (cadr exp) env)])
      (void)))

  (define (evaluate-variable exp env) (env-get env (cadr exp)))

  (define (evaluate-call exp env)
    (define (accumulate-arg-values values args)
      (if (empty? args)
        values 
        (accumulate-arg-values (append values (list (evaluate (car args)))) (cdr args))))
    (let* (
      [callee (evaluate (cadr exp) env)]
      [body (cadddr callee)]
      [arg-values (accumulate-arg-values '() (caddr exp))]
      )
      (evaluate body env)))

  (define (evaluate-statement-var exp env)
    (let ([name (get-token-lexeme (cadr exp))]
          [initializer (caddr exp)])
      (if (empty? initializer)
        (env-set! env name '())
        (env-set! env name (evaluate initializer env)))))

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
      (env-set! env name exp)))

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
      (let ([type (car exp)])
        (cond
          [(equal? type 'STATEMENT_FUN) (evaluate-statement-fun-declaration exp env)]
          [(equal? type 'STATEMENT_IF) (evaluate-statement-if exp env)]
          [(equal? type 'STATEMENT_WHILE) (evaluate-statement-while exp env)]
          [(equal? type 'STATEMENT_BLOCK) (evaluate-statement-block exp env)] 
          [(equal? type 'STATEMENT_PRINT) (evaluate-statement-print exp env)]
          [(equal? type 'STATEMENT_EXP) (evaluate-statement-exp exp env)]
          [(equal? type 'STATEMENT_VAR) (evaluate-statement-var exp env)]
          [(equal? type 'ASSIGNMENT_EXP) (evaluate-assignment exp env)]
          [(equal? type 'VARIABLE_EXP) (evaluate-variable exp env)]
          [(equal? type 'CALL_EXP) (evaluate-call exp env)]
          [(equal? type 'LITERAL_EXP) (evaluate-literal exp env)]
          [(equal? type 'GROUP_EXP) (evaluate (cadr exp) env)]
          [(equal? type 'UNARY_EXP) (evaluate-unary exp env)]
          [(equal? type 'BINARY_EXP) (evaluate-binary exp env)]
          [else (raise (cons "Unrecognized expression " (list type)) #t)]
        )))

  (define (exec-statements statements env)
    (if (empty? statements)
      (void)
      (begin
          (evaluate (car statements) env)
          (exec-statements (cdr statements) env)
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