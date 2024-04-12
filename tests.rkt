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
    (println (parse "if (true) { print(1); }"))
    (println (parse "while (a < 5) { print(3); a = 5; }"))
    (println (parse "fun count(n,x) { print(n); }"))
    (println (parse "foo(1+1, b);"))
    (println (parse "fun wow(a) { print(a); }"))
    ;(println (parse "foo(1)(2)(3);"))
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
    ; assignment should not be allowed to create a new variable
    ; (interpret "{x = 7; print x;}")
    (interpret "var a = \"global a\"; var b = \"global b\"; var c = \"global c\"; {var a = \"outer a\"; var b = \"outer b\"; {var a = \"inner a\"; print a; print b; print c; } print a; print b; print c; } print a; print b; print c;")
    (interpret "if (false) { print(3); } else { print(5); }")
    (interpret "var a = 0; while (a < 5) { print(3); a = a + 1;}")
    (interpret "fun wow() { print(1); }")
    (interpret "var a = \"global\"; { fun showA() {print a; } showA(); var a = \"block\"; showA(); }")
  )
)

(define (test-arg-accumulator)
  (begin
     (let ([token-list (scan "2,3,5)")]) (let-values ([(args rtokens) (consume-arg-list token-list)]) (println args)))))


; (test-arg-accumulator)
; (test-parser)
; (println (parse "fun wow() { print(1); } wow();"))
; (interpret "fun wow(a, b) { print(a); print(b); } wow(5, 1+1);")
(test-interpreter)
;(parse "var a = \"global\"; { fun showA() {print a; } showA(); var a = \"block\"; showA(); }")
(interpret "var a = \"global\"; { fun showA() {print a; } showA(); var a = \"block\"; showA(); }")
;(interpret "fun wow(a, b) { return a + b; } var a = wow(5, 1+1); print(a);")
; (interpret "fun makeCounter() {var i = 0; fun count() {i = i + 1; print i; } return count; } var counter = makeCounter(); counter(); counter();")
; (println (parse "var a = 0; while (a < 1) { print(3); a = 1;}"))
; (println (interpret "var a = 0; a = 1; print(a);"))
; (println (interpret "var a = 0; { a = 5; print(a); }"))
; next - bind arguments to params, create new environment for invocations, handle return statements

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
