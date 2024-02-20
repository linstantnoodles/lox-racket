#! /usr/bin/env racket
#lang racket 

(require rackunit) 
(require "parser.rkt")
(require "scanner.rkt")
(require "interpreter.rkt")

(define (test-parser)
  (begin 
    (println "testing parses")
    (println (parse "1;"))
    (println (parse "-1;"))
    (println (parse "1+1;"))
    (println (parse "1+1;"))
    (println (parse "1 < 2;"))
    (println (parse "1 > 2;"))
    (println (parse "1 <= 2;"))
    (println (parse "1 >= 2;"))
    (println (parse "var x = 1 + 1;"))
    (println (parse "var x = 1; x = 2;"))
    (println (parse "{ var x = 1; }"))
    (println (parse "{ var x = 1; } { 1; } "))
    ; (let ([token-list (scan "{var x = 1;}")])
    ;   (begin
    ;     (println token-list)
    ;     (block (cdr token-list))  
    ;   ) 
    ;   )
))

(test-parser)
