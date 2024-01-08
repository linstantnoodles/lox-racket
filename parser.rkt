#! /usr/bin/env racket
#lang racket

(require "scanner.rkt")

(define (binary-exp exp-left operator exp-right)
  (list 'BINARY_EXP exp-left operator exp-right)
)

(define (group-exp exp)
  (list 'GROUP_EXP exp))

(define (unary-exp token exp)
  (list 'UNARY_EXP token exp))

(define (literal-exp exp)
  (list 'LITERAL_EXP exp))

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

; expression     → equality ;
(define (expression token-list)
    (println "expresson")
    (let-values ([(expr rest-token-list) (equality token-list)])
      (values expr rest-token-list)))

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
    (println "comparison")
    (println token-list)
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
    (println "-term")
    (println token-list)
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
  (println "unary")
  (println token-list)
  (println "---")
  (if (match token-list (list 'MINUS 'BANG))
    (let-values (
        [(operator) (car token-list)]
        [(right-expr rest-token-list) (unary (cdr token-list))]
      )
        (values (unary-exp operator right-expr) rest-token-list))
    (primary token-list)))

(define (consume token-type token-list error-message)
  (if (empty? token-list)
    (raise (cons "no tokens to consumed" error-message))
    (if (equal? token-type (car (car token-list)))
        (cdr token-list)
        (raise error-message))))

; primary        → NUMBER | STRING | "true" | "false" | "nil" | "(" expression ")" ;
(define (primary token-list)
  (println "primary")
  (println token-list)
  (println "---")
  (cond 
    [(match token-list (list 'FALSE)) (values (literal-exp #f) (cdr token-list))]
    [(match token-list (list 'TRUE)) (values (literal-exp #t) (cdr token-list))]
    [(match token-list (list 'NIL)) (values (literal-exp '()) (cdr token-list))]
    [(match token-list (list 'NUMBER 'STRING)) (values (literal-exp (caddr (car token-list))) (cdr token-list))]
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

; since we're constructing a tree
; everything is returning both an
; expression TREE AND the new
; next position of the token list
; (define (parse token-list)
;   (define (at-end pos) (>= pos (length token-list)))
;   (define (get-token-type token)
;     (cadr token))
;   (define (recur curr-expr curr-pos)
;     (define (match expr token-types)
;       (if (empty? token-types)))

;     (define (check token-type) 
;         (equal? (get-token-type (list-ref token-list curr-pos)) token-type))
;     (define (expression) (equality))
;     (define (equality) 
;       (let ([expr (comparison)]) expr)
;     )
;     (define (comparison) (term))
;     (define (term) (factor))
;     (define (factor) (unary))
;     (define (unary)
;       (if (match (list 'MINUS 'PLUS))
;       (unary-exp "-" 5)
;     )

;     (if (at-end curr-pos) curr-expr (expression))
;   )

;   (recur '() 0)
; )

; (parse value)


; testing match token


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
(expression (list 
    (make-token 'LEFT_PAREN "(" null 1)
    (make-token 'NUMBER '5' 5 1)
    (make-token 'MINUS '-' null 1)
    (make-token 'NUMBER '5' 5 1)
    (make-token 'RIGHT_PAREN ")" null 1)
    ))

