#! /usr/bin/env racket
#lang racket

(require "parser.rkt")

(define (interpret exp) 
  (define (recur exp)
      (let ([type (car exp)])
        (cond 
          [(equal? type 'LITERAL_EXP) (car (car exp))]
          [(equal? type 'GROUP_EXP) (recur (car (car exp)))]
          [(equal? type 'UNARY_EXP) 'wow]
          [(equal? type 'BINARY_EXP) 'wow]
          [else (raise (cons "Unrecognized expression " (list type)) #t)]
        )))
  (recur (parse exp))
)


(interpret "1")
