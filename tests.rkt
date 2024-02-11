#! /usr/bin/env racket
#lang racket 

(require rackunit) 
(require "parser.rkt")
(require "scanner.rkt")
(require "interpreter.rkt")

(define (test-parser)
  (begin 
    (println "testing parse")
    (println (parse "1;"))
    (println (parse "-1;"))
    (println (parse "1+1;"))
    (println (parse "1+1;"))
    (println (parse "1 < 2;"))
    (println (parse "1 > 2;"))
    (println (parse "1 <= 2;"))
    (println (parse "1 >= 2;"))
    (println (parse "var x = 1 + 1;"))
))

(test-parser)
