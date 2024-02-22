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
    (println (parse "{1+1;} {var x = 7; print x;}"))
    ; (let ([token-list (scan "{var x = 1;}")])
    ;   (begin
    ;     (println token-list)
    ;     (block (cdr token-list))  
    ;   ) 
    ;   )
))
(define (test-interpreter)
  (begin
    (interpret "(-(1+1));")
    (interpret "(-(2+1));")
    (interpret "(-(2*5));")
    (interpret "(1 + 1);")
    (interpret "(\"wow\" + \"hey\");")
    (interpret "var k = 5 + 1; k = 2; print k;")
    (interpret "{1+1;} {var x = 7; print x;}")
  )
)


(test-parser)
(test-interpreter)
