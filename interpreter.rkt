#! /usr/bin/env racket
#lang racket

(require "parser.rkt")

(define (interpret src) 
  (define (evaluate-literal exp) (cadr exp))
  (define (evaluate-unary exp)
    (let ([operator-type (caadr exp)]
          [unary-exp (caddr exp)]
         )
      (cond 
        [(equal? operator-type 'MINUS) (- (recur unary-exp))]
        [(equal? operator-type 'BANG) (not (recur unary-exp))]
        [else (raise ("Unrecognized unary operator"))]))
  )
  (define (evaluate-binary exp)
    (let (
          [binary-exp-left (cadr exp)]
          [operator-type (caaddr exp)]
          [binary-exp-right (cadddr exp)]
         )
      (cond 
        [(equal? operator-type 'PLUS) (+ (recur binary-exp-left) (recur binary-exp-right))]
        [(equal? operator-type 'MINUS) (- (recur binary-exp-left) (recur binary-exp-right))]
        [(equal? operator-type 'STAR) (* (recur binary-exp-left) (recur binary-exp-right))]
        [(equal? operator-type 'SLASH) (/ (recur binary-exp-left) (recur binary-exp-right))]
        [else (raise ("Unrecognized binary operator"))]))
  )
  (define (recur exp)
      (let ([type (car exp)])
        (cond 
          [(equal? type 'LITERAL_EXP) (evaluate-literal exp)]
          [(equal? type 'GROUP_EXP) (recur (cadr exp))]
          [(equal? type 'UNARY_EXP) (evaluate-unary exp)]
          [(equal? type 'BINARY_EXP) (evaluate-binary exp)]
          [else (raise (cons "Unrecognized expression " (list type)) #t)]
        )))
  (recur (parse src))
)

(interpret "(-(1+1))")
(interpret "(-(2+1))")
(interpret "(-(2*5))")
