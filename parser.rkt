#! /usr/bin/env racket
#lang racket

(require "scanner.rkt")
(provide parse)
(provide block)
(provide consume-arg-list)
(define DEBUG #f)
(define (debug msg)
  (if DEBUG (println msg) '()))

(define (binary-exp exp-left operator exp-right) (list 'BINARY_EXP exp-left operator exp-right))
(define (group-exp exp) (list 'GROUP_EXP exp))
(define (unary-exp token exp) (list 'UNARY_EXP token exp))
(define (literal-exp exp) (list 'LITERAL_EXP exp))
(define (variable-exp exp) (list 'VARIABLE_EXP exp))
(define (call-exp exp arguments) (list 'CALL_EXP exp arguments))
(define (get-exp exp name) (list 'GET_EXP exp name))
(define (assignment-exp name value) (list 'ASSIGNMENT_EXP name value))
(define (statement-print exp) (list 'STATEMENT_PRINT exp))
(define (statement-block exp) (list 'STATEMENT_BLOCK exp))
(define (statement-exp exp) (list 'STATEMENT_EXP exp))
(define (statement-var name initializer) (list 'STATEMENT_VAR name initializer))
(define (statement-return value) (list 'STATEMENT_RETURN value))
(define (statement-if condition then-branch else-branch) (list 'STATEMENT_IF condition then-branch else-branch))
(define (statement-function name param-list body) (list 'STATEMENT_FUN name param-list body))
(define (statement-class name fun-list) (list 'STATEMENT_CLASS name fun-list))
(define (statement-while condition body) (list 'STATEMENT_WHILE condition body))

(define (parse src)
  (define (recur token-list)
    (if (empty? token-list) 
      '()
      (let-values ([(declaration-value rtokens) (declaration token-list)])
        (cons declaration-value (recur rtokens)))))
  (let ([token-list (scan src)]) (recur token-list)))

(define (declaration token-list)
  (cond 
    [(match token-list (list 'VAR)) (var-declaration (cdr token-list))]
    [(match token-list (list 'FUN)) (fun-declaration (cdr token-list))]
    [(match token-list (list 'CLASS)) (class-declaration (cdr token-list))]
    [else (statement token-list)]))

(define (consume-parameter-list token-list)
  (define (accumulate-params params rtokens)
    (if (not (match rtokens (list 'COMMA)))
      (values params rtokens)
      (let ([rtokens (consume 'COMMA rtokens "expect , after function param")])
        (accumulate-params
          (append params (list (car rtokens)))
          (cdr rtokens)))))
  (if (match token-list (list 'RIGHT_PAREN))
    (values '() token-list)
    (accumulate-params (list (car token-list)) (cdr token-list))))

(define (consume-function-list token-list)
  (define (accumulate-functions fun-list rtokens)
    (if (match rtokens (list 'RIGHT_BRACE))
      (values fun-list rtokens)
      (let-values ([(fn rtokens) (fun-declaration rtokens)])
        (accumulate-functions (append fun-list (list fn)) rtokens))))
  (if (match token-list (list 'RIGHT_BRACE))
    (values '() token-list)
    (accumulate-functions '() token-list)))

(define (class-declaration tokens)
  (let (
      [rtokens (consume 'IDENTIFIER tokens "expect a class name")]
      [name (car tokens)]
    )
    (let ([rtokens (consume 'LEFT_BRACE rtokens "expect '{' before class body")])
      (let-values ([(fun-list rtokens) (consume-function-list rtokens)])
        (let ([rtokens (consume 'RIGHT_BRACE rtokens "expect '}' after class body")])
          (values (statement-class (cadr name) fun-list) rtokens))))))

(define (fun-declaration tokens)
  (let (
      [rtokens (consume 'IDENTIFIER tokens "expected a function name")]
      [name (car tokens)]
    )
    (let ([rtokens (consume 'LEFT_PAREN rtokens "expect '(' after function name")])
      (let-values ([(param-list rtokens) (consume-parameter-list rtokens)])
        (let ([rtokens (consume 'RIGHT_PAREN rtokens "expect ')' after function params")])
          (let ([rtokens (consume 'LEFT_BRACE rtokens "expect '{' before body")])
            (let-values ([
              (body rtokens) (block rtokens)
              ])
              (values (statement-function (cadr name) param-list body) rtokens))))))))

(define (var-declaration token-list)
    (let (
          [rtokens (consume 'IDENTIFIER token-list "expect a variable name")]
          [name (car token-list)]
         )
        (if (match rtokens (list 'EQUAL))
            (let-values ([(initializer rtokens) (expression (cdr rtokens))])
                (let ([rtokens (consume 'SEMICOLON rtokens "expect ; after variable decl")])
                    (values (statement-var name initializer) rtokens)))
            (let ([rtokens (consume 'SEMICOLON rtokens "expect ; after variable initialization")])
              (values (statement-var name '()) rtokens))
        )))

(define (statement token-list)
  (cond
    [(match token-list (list 'PRINT)) (print-statement (cdr token-list))]
    [(match token-list (list 'RETURN)) (return-statement (cdr token-list))]
    [(match token-list (list 'IF)) (if-statement (cdr token-list))]
    [(match token-list (list 'WHILE)) (while-statement (cdr token-list))]
    [(match token-list (list 'FOR)) (for-statement (cdr token-list))]
    [(match token-list (list 'LEFT_BRACE)) (block (cdr token-list))]
    [else (expression-statement token-list)]))

; forStmt → "for" "(" ( varDecl | exprStmt | ";" )
;             expression? ";"
;             expression? ")" statement ;
(define (for-statement token-list)
  (define (initializer token-list)
    (cond 
      [(match token-list (list 'SEMICOLON)) (values '() (cdr token-list))]
      [(match token-list (list 'VAR)) (var-declaration (cdr token-list))]
      [else (expression-statement token-list)]))  
  (define (condition token-list)
    (if (match token-list (list 'SEMICOLON))
      (values '() (cdr token-list))
      (let-values ([(expression-value rtokens) (expression token-list)])
         (let ([rtokens (consume 'SEMICOLON rtokens "expect ';' after condition expression")])
          (values expression-value rtokens)))))
  (define (increment token-list)
    (if (match token-list (list 'RIGHT_PAREN))
      (values '() (cdr token-list))
      (let-values ([(expression-value rtokens) (expression token-list)])
         (let ([rtokens (consume 'RIGHT_PAREN rtokens "expect ')' after increment expression")])
          (values expression-value rtokens)))))
  (let ([rtokens (consume 'LEFT_PAREN token-list "expect '(' after for")])
    (let*-values (
      [(initializer-value rtokens) (initializer rtokens)]
      [(condition-value rtokens) (condition rtokens)]
      [(increment-value rtokens) (increment rtokens)]
      [(body-value rtokens) (statement rtokens)])
        (values
          (statement-block
            (list
              initializer-value
              (statement-while 
                condition-value
                (statement-block
                  (list
                    body-value
                    (statement-exp increment-value))))))
          rtokens))))

; whileStmt      → "while" "(" expression ")" statement ;
(define (while-statement token-list)
  (let ([rtokens (consume 'LEFT_PAREN token-list "expect '(' after while")])
      (let-values ([(condition rtokens) (expression rtokens)])
        (let ([rtokens (consume 'RIGHT_PAREN rtokens "expect ')' after while")])
          (let-values ([(body rtokens) (statement rtokens)])
            (values (statement-while condition body) rtokens))))))

(define (if-statement token-list) 
  (let ([rtokens (consume 'LEFT_PAREN token-list "expect '(' after if")])
    (let-values ([(condition rtokens) (expression rtokens)])
      (let ([rtokens (consume 'RIGHT_PAREN rtokens "expect ')' after if")])
        (let-values ([(then-branch rtokens) (statement rtokens)])
          (if (match rtokens (list 'ELSE))
            (let-values ([(else-branch rtokens) (statement (cdr rtokens))])
              (values (statement-if condition then-branch else-branch) rtokens))
            (values (statement-if condition then-branch '()) rtokens)))))))
               
(define (return-statement token-list)
  (if (match token-list (list 'SEMICOLON))
    (values (statement-return '()) (cdr token-list))
    (let-values ([(value rtokens) (expression token-list)])
      (let ([rtokens (consume 'SEMICOLON rtokens "expect ; after return expression")])
        (values (statement-return value) rtokens)))))

(define (block token-list)
  (define (at-end token-list) (<= (length token-list) 0))

  (define (recur statements token-list)
    (if (or (match token-list (list 'RIGHT_BRACE)) (at-end token-list))
      (values statements token-list)
      (let-values ([(expr rtokens) (declaration token-list)])
        (recur (append statements (list expr)) rtokens))))

  (let-values ([(statement-list rtokens) (recur '() token-list)])
      (let ([rtokens (consume 'RIGHT_BRACE rtokens "expectesd } after block")])
      (values (statement-block statement-list) rtokens))))

(define (print-statement token-list)
    (let-values ([(expr rtokens) (expression token-list)])
        (let ([rtokens (consume 'SEMICOLON rtokens "expect ; after print expression")])
                                                        (values (statement-print expr) rtokens))))

(define (expression-statement token-list)
    (let-values ([(expr rtokens) (expression token-list)])
        (let ([rtokens (consume 'SEMICOLON rtokens "expect ; after statement expression")])
                                                        (values (statement-exp expr) rtokens))))

; expression     → equality ;
(define (expression token-list)
    (debug "expression")
    (debug token-list)
    (let-values ([(expr rtokens) (assignment token-list)])
      (values expr rtokens)))

(define (assignment token-list)
    (let-values ([(expr rtokens) (equality token-list)])
      (if (match rtokens (list 'EQUAL))
        (let-values ([(value rtokens) (assignment (cdr rtokens))])
          (if (eq? (car expr) 'VARIABLE_EXP)
            (values (assignment-exp (cadr expr) value) rtokens)
            (raise "invalid assignment target.")))
        (values expr rtokens))))

; equality       → comparison ( ( "!=" | "==" ) comparison )* ;
(define (equality token-list)
  (let-values ([(expr rtokens) (comparison token-list)])
    (define (recur token-list curr-expr)
      (if (not (match token-list (list 'BANG_EQUAL 'EQUAL_EQUAL)))
        (values curr-expr token-list)
        (let-values (
          [(operator) (car token-list)]
          [(right rtokens) (comparison (cdr token-list))]
          )
          (recur rtokens (binary-exp curr-expr operator right)))))
      (recur rtokens expr)))

; comparison     → term ( ( ">" | ">=" | "<" | "<=" ) term )* ;
(define (comparison token-list)
    (debug "comparison")
    (debug token-list)
    (let-values ([(expr rtokens) (term token-list)])
        (define (recur token-list curr-expr)
            (if (not (match token-list (list 'LESS 'GREATER 'LESS_EQUAL 'GREATER_EQUAL)))
              (values curr-expr token-list)
              (let-values (
                    [(operator) (car token-list)]
                    [(right rtokens) (term (cdr token-list))]
                   )
                   (recur rtokens (binary-exp curr-expr operator right)))))
        (recur rtokens expr)))

; term → factor ( ( "-" | "+" ) factor )* ;
(define (term token-list)
  (let-values ([(expr rtokens) (factor token-list)])
    (define (recur token-list curr-expr)
        (if (not (match token-list (list 'MINUS 'PLUS)))
          (values curr-expr token-list)
          (let-values (
              [(operator) (car token-list)]
              [(right rtokens) (factor (cdr token-list))]
              )
              (recur rtokens (binary-exp curr-expr operator right)))))
    (recur rtokens expr)))

; factor → unary ( ( "/" | "*" ) unary )* ;
(define (factor token-list)
  (let-values ([(expr rtokens) (unary token-list)])
    (define (recur token-list curr-expr)
      (if (not (match token-list (list 'SLASH 'STAR)))
        (values curr-expr token-list)
        (let-values (
          [(operator) (car token-list)]
          [(right rtokens) (unary (cdr token-list))]
          )
          (recur rtokens (binary-exp curr-expr operator right)))))
    (recur rtokens expr)))

; unary → ( "!" | "-" ) unary | call ;
(define (unary token-list)
  (if (match token-list (list 'MINUS 'BANG))
    (let-values (
      [(operator) (car token-list)]
      [(right-expr rtokens) (unary (cdr token-list))])
        (values (unary-exp operator right-expr) rtokens))
    (call token-list)))

(define (consume-arg-list token-list)
  (define (recur args rtokens)
    (if (match rtokens (list 'RIGHT_PAREN))
      (values args rtokens)
        (let-values ([(expr rtokens) (expression rtokens)])
          (if (match rtokens (list 'COMMA))
            (let ([rtokens (consume 'COMMA rtokens "expected , after arg")])
              (recur
                (append args (list expr))
                rtokens))
            (recur
                (append args (list expr))
                rtokens)))))
  (if (match token-list (list 'RIGHT_PAREN)) ; no arguments basically
    (values '() token-list)
    (recur '() token-list)))

; call → primary ( "(" arguments? ")" )* ;
(define (call token-list)
  (define (recur callee tokens)
    (if (not (match tokens (list 'LEFT_PAREN)))
      (values callee tokens)
      (let-values ([(arguments rtokens) (consume-arg-list (cdr tokens))])
        (let ([rtokens (consume 'RIGHT_PAREN rtokens "expect ) after function args")])
          (recur (call-exp callee arguments) rtokens)))))

  ; bootstrap the initial callee
  (let-values ([(expr rtokens) (primary token-list)])
    (cond 
      [
        (match rtokens (list 'LEFT_PAREN))
        (let-values ([(arguments rtokens) (consume-arg-list (cdr rtokens))])
          (let ([rtokens (consume 'RIGHT_PAREN rtokens "expect ) after function args")])
            (recur (call-exp expr arguments) rtokens)))
      ]
      [
        (match rtokens (list 'DOT))
        (let ([name (cadr rtokens)])
          (let ([rtokens (consume 'IDENTIFIER (cdr rtokens) "expect property name after .")])
            (recur (get-exp expr name) rtokens)))
      ]
      [else (values expr rtokens)])))

; reports the wrong line number... currently the line AFTER
; the bad toke
; just consumes it and return tokens
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
                (number->string (get-token-line next-token)) ":" error-message (format "--- encountered type ~a" (car next-token)))))))))

; primary → NUMBER | STRING | "true" | "false" | "nil" | "(" expression ")" ;
(define (primary token-list)
  (cond 
    [(match token-list (list 'FALSE)) (values (literal-exp #f) (cdr token-list))]
    [(match token-list (list 'TRUE)) (values (literal-exp #t) (cdr token-list))]
    [(match token-list (list 'NIL)) (values (literal-exp '()) (cdr token-list))]
    [(match token-list (list 'NUMBER 'STRING)) (values (literal-exp (caddr (car token-list))) (cdr token-list))]
    [(match token-list (list 'IDENTIFIER)) (values (variable-exp (caddr (car token-list))) (cdr token-list))]
    [(match token-list (list 'LEFT_PAREN)) (let-values ([(expr rtokens) (expression (cdr token-list))])
                                                     (let ([rtokens (consume 'RIGHT_PAREN rtokens "expect ) after expression")])
                                                        (values (group-exp expr) rtokens)))]
    [else (begin 
      (println (car token-list))
      (raise "expected a valid expression"))]
  )
)

(define (match token-list token-types)
    (if (empty? token-list)
      #false
      (if (member (caar token-list) token-types) #t #f)))