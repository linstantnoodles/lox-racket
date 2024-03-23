#! /usr/bin/env racket
#lang racket

(require "scanner.rkt")
(provide parse)
(provide block)

(define (binary-exp exp-left operator exp-right)
  (list 'BINARY_EXP exp-left operator exp-right)
)
(define (group-exp exp)
  (list 'GROUP_EXP exp))

(define (unary-exp token exp)
  (list 'UNARY_EXP token exp))

(define (literal-exp exp)
  (list 'LITERAL_EXP exp))

(define (variable-exp exp)
  (list 'VARIABLE_EXP exp))

(define (assignment-exp name value)
  (list 'ASSIGNMENT_EXP name value))

(define (statement-print exp)
  (list 'STATEMENT_PRINT exp))

(define (statement-block exp)
  (list 'STATEMENT_BLOCK exp))

(define (statement-exp exp)
  (list 'STATEMENT_EXP exp))
  

(define (statement-var name initializer) 
  (list 'STATEMENT_VAR name initializer))

(define value (list 
    (make-token 'NUMBER 7 null 1)
    (make-token 'EQUAL_EQUAL "==" null 1)
    (make-token 'NUMBER 7 null 1)
))

; how does lambda call itself?
; (define (seq-op-creator token-types)
;     (lambda (term token-list)
;         (let-values ([(expr rest-token-list) (factor token-list)])
;             (define (recur token-list curr-expr)
;                 (if (not (match token-list token-types))
;                   (values curr-expr token-list)
;                   (let-values (
;                         [(operator) (car token-list)]
;                         [(right rest-token-list) (factor (cdr token-list))]
;                        )
;                        (recur rest-token-list (binary-exp curr-expr operator right)))))
;             (recur rest-token-list expr))))

(define DEBUG #f)
(define (debug msg)
  (if DEBUG (println msg) '()))

(define (parse src)
    (define (recur token-list)
      (if (empty? token-list) 
        '()
        (let-values ([
            (declaration-value rest-token-list) (declaration token-list)
        ])
            (cons declaration-value (recur rest-token-list))
            )))
    (let ([token-list (scan src)]) (recur token-list)))

(define (declaration token-list)
  (if (match token-list (list 'VAR))
      (var-declaration (cdr token-list))
      (statement token-list)))

(define (var-declaration token-list)
    (let (
          [rest-token-list (consume 'IDENTIFIER token-list "expect a variable name")]
          [name (car token-list)]
         )
        (if (match rest-token-list (list 'EQUAL))
            (let-values ([(initializer rest-token-list) (expression (cdr rest-token-list))])
                (let ([rest-token-list (consume 'SEMICOLON rest-token-list "expect ; after variable decl")])
                    (values (statement-var name initializer) rest-token-list)))
            (let ([rest-token-list (consume 'SEMICOLON rest-token-list "expect ; after variable initialization")])
              (values (statement-var name '()) rest-token-list))
        )))

(define (statement token-list)
    (if (match token-list (list 'PRINT))
      (print-statement (cdr token-list))
      (if (match token-list (list 'LEFT_BRACE))
        (block (cdr token-list))
        (expression-statement token-list))))

(define (block token-list)
  (define (at-end token-list) (<= (length token-list) 0))

  (define (recur statements token-list)
    (if (or (match token-list (list 'RIGHT_BRACE)) (at-end token-list))
      (values statements token-list)
      (let-values ([(expr rest-token-list) (declaration token-list)])
        (recur (append statements (list expr)) rest-token-list))))

  (let-values ([(statement-list rest-token-list) (recur '() token-list)])
      (let ([rest-token-list (consume 'RIGHT_BRACE rest-token-list "expectesd } after block")])
      (values (statement-block statement-list) rest-token-list))))

(define (print-statement token-list)
    (let-values ([(expr rest-token-list) (expression token-list)])
        (let ([rest-token-list (consume 'SEMICOLON rest-token-list "expect ; after expression")])
                                                        (values (statement-print expr) rest-token-list))))

(define (expression-statement token-list)
    (let-values ([(expr rest-token-list) (expression token-list)])
        (let ([rest-token-list (consume 'SEMICOLON rest-token-list "expect ; after expression")])
                                                        (values (statement-exp expr) rest-token-list))))

; expression     → equality ;
(define (expression token-list)
    (debug "expression")
    (let-values ([(expr rest-token-list) (assignment token-list)])
      (values expr rest-token-list)))

(define (assignment token-list)
    (let-values ([(expr rest-token-list) (equality token-list)])
      (if (match rest-token-list (list 'EQUAL))
        (let-values ([(value rest-token-list) (assignment (cdr rest-token-list))])
          (if (eq? (car expr) 'VARIABLE_EXP)
            (values (assignment-exp (cadr expr) value) rest-token-list)
            (raise "invalid assignment target.")))
        (values expr rest-token-list))))

; equality       → comparison ( ( "!=" | "==" ) comparison )* ;
(define (equality token-list)
    (let-values ([(expr rest-token-list) (comparison token-list)])
        (define (recur token-list curr-expr)
            (if (not (match token-list (list 'BANG_EQUAL 'EQUAL_EQUAL)))
              (values curr-expr token-list)
              (let-values (
                    [(operator) (car token-list)]
                    [(right rest-token-list) (comparison (cdr token-list))]
                   )
                   (recur rest-token-list (binary-exp curr-expr operator right)))))
        (recur rest-token-list expr)))

; comparison     → term ( ( ">" | ">=" | "<" | "<=" ) term )* ;
(define (comparison token-list)
    (debug "comparison")
    (debug token-list)
    (let-values ([(expr rest-token-list) (term token-list)])
        (define (recur token-list curr-expr)
            (if (not (match token-list (list 'LESS 'GREATER 'LESS_EQUAL 'GREATER_EQUAL)))
              (values curr-expr token-list)
              (let-values (
                    [(operator) (car token-list)]
                    [(right rest-token-list) (term (cdr token-list))]
                   )
                   (recur rest-token-list (binary-exp curr-expr operator right)))))
        (recur rest-token-list expr)))

; term           → factor ( ( "-" | "+" ) factor )* ;
(define (term token-list)
    (debug "-term")
    (debug token-list)
    (let-values ([(expr rest-token-list) (factor token-list)])
        (define (recur token-list curr-expr)
            (if (not (match token-list (list 'MINUS 'PLUS)))
              (values curr-expr token-list)
              (let-values (
                    [(operator) (car token-list)]
                    [(right rest-token-list) (factor (cdr token-list))]
                   )
                   (recur rest-token-list (binary-exp curr-expr operator right)))))
        (recur rest-token-list expr)))

; factor         → unary ( ( "/" | "*" ) unary )* ;
(define (factor token-list)
  (let-values ([(expr rest-token-list) (unary token-list)])
    (define (recur token-list curr-expr)
        (if (not (match token-list (list 'SLASH 'STAR)))
          (values curr-expr token-list)
          (let-values (
                [(operator) (car token-list)]
                [(right rest-token-list) (unary (cdr token-list))]
               )
               (recur rest-token-list (binary-exp curr-expr operator right)))))
    (recur rest-token-list expr)))

; unary          → ( "!" | "-" ) unary | primary ;
(define (unary token-list)
  (debug "unary")
  (debug token-list)
  (debug "---")
  (if (match token-list (list 'MINUS 'BANG))
    (let-values (
        [(operator) (car token-list)]
        [(right-expr rest-token-list) (unary (cdr token-list))]
      )
        (values (unary-exp operator right-expr) rest-token-list))
    (primary token-list)))

; reports the wrong line number... currently the line AFTER
; the bad token
(define (consume token-type token-list error-message)
  (if (empty? token-list)
    (raise (cons "no tokens to consumed" error-message))
    (let ([next-token (car token-list)])
      (if (equal? token-type (car next-token))
          (cdr token-list)
          (raise 
            (string-join
              (list
                "line"
                (number->string (get-token-line next-token)) ":" error-message)))))))

; primary        → NUMBER | STRING | "true" | "false" | "nil" | "(" expression ")" ;
(define (primary token-list)
  (debug "primary")
  (debug token-list)
  (debug "---")
  (cond 
    [(match token-list (list 'FALSE)) (values (literal-exp #f) (cdr token-list))]
    [(match token-list (list 'TRUE)) (values (literal-exp #t) (cdr token-list))]
    [(match token-list (list 'NIL)) (values (literal-exp '()) (cdr token-list))]
    [(match token-list (list 'NUMBER 'STRING)) (values (literal-exp (caddr (car token-list))) (cdr token-list))]
    [(match token-list (list 'IDENTIFIER)) (values (variable-exp (caddr (car token-list))) (cdr token-list))]
    [(match token-list (list 'LEFT_PAREN)) (let-values ([(expr rest-token-list) (expression (cdr token-list))])
                                                     (let ([rest-token-list (consume 'RIGHT_PAREN rest-token-list "expect ) after expression")])
                                                        (values (group-exp expr) rest-token-list)))]
    [else (raise "expected a valid expression")]
  )
)

(define (match token-list token-types)
    (if (empty? token-list)
      #false
      (if (member (caar token-list) token-types) #t #f)))


; match tests
; (match value (list 'EQUAL_EQUAL))
; (match value (list 'UAL_EQUAL))

; unary tests
; (unary (list 
;     (make-token 'MINUS '-' null 1)
;     (make-token 'NUMBER '5' 5 1)
;     ))

; primary tests
; (primary (list 
;   (make-token 'NUMBER '123' 123 1)
; ))

; factor
; (factor (list 
;     (make-token 'NUMBER '5' 5 1)
;     (make-token 'STAR '*' null 1)
;     (make-token 'NUMBER '5' 5 1)
;     ))

; comparison
; (comparison (list 
;     (make-token 'NUMBER '5' 5 1)
;     (make-token 'GREATER '>' null 1)
;     (make-token 'NUMBER '5' 5 1)
;     ))

; equality
; (equality (list 
;     (make-token 'NUMBER '5' 5 1)
;     (make-token 'EQUAL_EQUAL '== null 1)
;     (make-token 'NUMBER '5' 5 1)
;     ))

; expression
; bug - the issue is that our match doesn't work per spec
; match should only match values in SEQUENCE. instead we're doing a membership check
; (expression (list 
;     (make-token 'LEFT_PAREN "(" null 1)
;     (make-token 'NUMBER '5' 5 1)
;     (make-token 'MINUS '-' null 1)
;     (make-token 'NUMBER '5' 5 1)
;     (make-token 'RIGHT_PAREN ")" null 1)
;     ))

#| (expression (list (make-token 'STRING 'wowofsdf' 'wowowd' 1))) |#

;(statement (list 
;     (make-token 'PRINT 'print' 5 1)
;     (make-token 'NUMBER '5' 5 1)
;     (make-token 'SEMICOLON ";" 1 1)
;))

; (parse "x + x;")
