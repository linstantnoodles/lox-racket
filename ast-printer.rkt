#! /usr/bin/env racket
#lang racket

(require "scanner.rkt")


; expression     → literal
;                | unary
;                | binary
;                | grouping ;

; literal        → NUMBER | STRING | "true" | "false" | "nil" ;
; grouping       → "(" expression ")" ;
; unary          → ( "-" | "!" ) expression ;
; binary         → expression operator expression ;
; operator       → "==" | "!=" | "<" | "<=" | ">" | ">="
;                | "+"  | "-"  | "*" | "/" ;

(define (binary-exp exp-left operator exp-right)
  (list 'BINARY_EXP exp-left operator exp-right)
)

(define (group-exp exp)
  (list 'GROUP_EXP exp))

(define (unary-exp token exp)
  (list 'UNARY_EXP token exp))

(define (literal-exp exp)
  (list 'LITERAL_EXP exp))

(define test-expr 
  (binary-exp
    (unary-exp
      (make-token 'MINUS "-" null 1)
      (literal-exp 123)
    )
    (make-token 'STAR "*" null 1)
    (group-exp (literal-exp 45.67))
  )
)

(define (ast-printer exp) 
  (define (recur exp)
      (let ([type (car exp)])
        (cond 
          [(equal? type 'LITERAL_EXP) (car (car exp))]
          [(equal? type 'GROUP_EXP) (recur (car (car exp)))]
          [(equal? type 'UNARY_EXP) 'wow]
          [(equal? type 'BINARY_EXP) 'wow]
          [else (raise (cons "Unrecognized expression " (list type)) #t)]
        )))
  (recur exp)
)

(println test-expr)






