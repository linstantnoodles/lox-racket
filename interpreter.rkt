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
    (let ([hash (car env)])
      (hash-ref hash key)))
  (define envr (make-env))

  (define (evaluate-literal exp) (cadr exp))

  (define (evaluate-unary exp)
    (let ([operator-type (caadr exp)]
          [unary-exp (caddr exp)]
         )
      (cond 
        [(equal? operator-type 'MINUS) (- (evaluate unary-exp))]
        [(equal? operator-type 'BANG) (not (evaluate unary-exp))]
        [else (raise ("Unrecognized unary operator"))]))
  )

  (define (evaluate-statement-block exp)
    (define (recur statement-list) 
      (if (> (length statement-list) 0)
        (begin
          (evaluate (car statement-list))
          (recur (cdr statement-list))
        ) '()))
    (recur (cadr exp)))

  (define (evaluate-statement-print exp)
    (let ([value (evaluate (cadr exp))])
      (println value)))
      
  (define (evaluate-statement-exp exp)
    (let ([value (evaluate (cadr exp))])
      (void)))

  (define (evaluate-variable exp) (env-get envr (cadr exp)))

  (define (evaluate-statement-var exp)
    (let ([name (get-token-lexeme (cadr exp))]
          [initializer (caddr exp)])
      (if (empty? initializer)

        (env-set! envr name '())
        (env-set! envr name (evaluate initializer)))))

(define (evaluate-assignment exp)
  (let ([name (cadr exp)]
        [value (caddr exp)])
      (env-set! envr name (evaluate value))))

  (define (evaluate-binary exp)
    (let (
          [binary-value-left (evaluate (cadr exp))]
          [operator-type (caaddr exp)]
          [binary-value-right (evaluate (cadddr exp))]
         )
      (cond 
        [(equal? operator-type 'PLUS)
         (if (and (string? binary-value-left) (string? binary-value-right))
            (string-append binary-value-left binary-value-right)
            (+ binary-value-left binary-value-right))]
        [(equal? operator-type 'MINUS) (- binary-value-left binary-value-right)]
        [(equal? operator-type 'STAR) (* binary-value-left binary-value-right)]
        [(equal? operator-type 'SLASH) (/ binary-value-left binary-value-right)]
        [else (raise ("Unrecognized binary operator"))])))
  
  (define (evaluate exp)
      (let ([type (car exp)])
        (cond
          [(equal? type 'STATEMENT_BLOCK) (evaluate-statement-block exp)] 
          [(equal? type 'STATEMENT_PRINT) (evaluate-statement-print exp)]
          [(equal? type 'STATEMENT_EXP) (evaluate-statement-exp exp)]
          [(equal? type 'STATEMENT_VAR) (evaluate-statement-var exp)]
          [(equal? type 'ASSIGNMENT_EXP) (evaluate-assignment exp)]
          [(equal? type 'VARIABLE_EXP) (evaluate-variable exp)]
          [(equal? type 'LITERAL_EXP) (evaluate-literal exp)]
          [(equal? type 'GROUP_EXP) (evaluate (cadr exp))]
          [(equal? type 'UNARY_EXP) (evaluate-unary exp)]
          [(equal? type 'BINARY_EXP) (evaluate-binary exp)]
          [else (raise (cons "Unrecognized expression " (list type)) #t)]
        )))

  (define (exec-statements statements)
    (if (empty? statements)
      (void)
      (begin
          (evaluate (car statements))
          (exec-statements (cdr statements))
    )))

  (exec-statements (parse src))
  #| (println "printing end hash") |#
  #| (println envr) |#
)

;(interpret "(-(1+1))")
;(interpret "(-(2+1))"):w
;(interpret "(-(2*5))")
;(interpret "(1 + 1)")
;(interpret "(\"wow\" + \"hey\")")
; (interpret "var k = 5 + 1; k = 2; print k;")
