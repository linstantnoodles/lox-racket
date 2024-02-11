#! /usr/bin/env racket
#lang racket

(provide scan)
(provide make-token)
(provide get-token-type)
(provide get-token-lexeme)
(provide get-token-literal)
(provide get-token-line)

; (car token) -> type
; (cadr token) -> text
; (caddr token) -> literal
; (cadddr token) -> line
(define (make-token type text literal line)
  (list type text literal line)
)
(define (get-token-type token) (car token))
(define (get-token-lexeme token) (cadr token))
(define (get-token-literal token) (caddr token))
(define (get-token-line token) (cadddr))

(define (scan source)
  (define (at-end current) (>= current (string-length source)))
  (define keywords (make-hash))
  (begin
    (hash-set! keywords "and" 'AND)
    (hash-set! keywords "class" 'CLASS)
    (hash-set! keywords "else" 'ELSE)
    (hash-set! keywords "false" 'FALSE)
    (hash-set! keywords "for" 'FOR)
    (hash-set! keywords "fun" 'FUN)
    (hash-set! keywords "if" 'IF)
    (hash-set! keywords "nil" 'NIL)
    (hash-set! keywords "or" 'OR)
    (hash-set! keywords "print" 'PRINT)
    (hash-set! keywords "return" 'RETURN)
    (hash-set! keywords "super" 'SUPER)
    (hash-set! keywords "this" 'THIS)
    (hash-set! keywords "true" 'TRUE)
    (hash-set! keywords "var" 'VAR)
    (hash-set! keywords "while" 'WHILE)
  )
  (define (recur start current line tokens) 
    (define (add-token type shift) (recur start (+ shift current) line (cons (make-token type '() '() line) tokens)))
    (define (skip shift line) (recur start (+ shift current) line tokens))
    (define (add-string-token value shift) (recur start (+ shift current) line (cons (make-token 'STRING value value line) tokens)))
    (define (add-number-token value shift)
        (recur
          start
          (+ shift current)
          line
          (cons (make-token 'NUMBER value (string->number value) line) tokens)))
    (define (add-identifier-token value shift)
      (recur
        start
        (+ shift current)
        line
        (if 
            (hash-has-key? keywords value) 
            (cons (make-token (hash-ref keywords value) value '() line) tokens)
            (cons (make-token 'IDENTIFIER value value line) tokens)
        )
      )
    )
    (define (next-pos current) (if (at-end current) (+ 1 current) (next-pos (+ 1 current))))
    (define (match-next char) (if (at-end (+ 1 current)) #f (equal? char (string-ref source (+ 1 current)))))
    (define (add-string start)
       (define (iter-add-string end)
         (if (or (at-end end) (equal? (string-ref source end) #\")) end (iter-add-string (+ 1 end))))
       (let ([end (iter-add-string (+ 1 start))])
         (add-string-token (substring source start end) (+ 1 (- end start))))
    )
    (define (add-number start)
       (define (iter-add-number end)
         (define (next-char)
            (if (at-end (+ 1 end)) '() (string-ref source (+ 1 end))))
         (if 
           (or
             (at-end end)
             (and 
               (not (char-numeric? (string-ref source end)))
               (not (equal? (string-ref source end) #\.))
             )
           )
           end 
           (if (equal? (string-ref source end) #\.)
             (if (char-numeric? (next-char)) (iter-add-number (+ 1 end)) end)
             (iter-add-number (+ 1 end)))
         )
       )
       (let ([end (iter-add-number start)])
         (add-number-token (substring source start end) (- end start)))
    )
    (define (char-alpha? char) 
      (let (
            [curr (char->integer char)]
            [lower-a (char->integer #\a)]
            [upper-a (char->integer #\A)]
            [lower-z (char->integer #\z)]
            [upper-z (char->integer #\Z)]
            [underscore (char->integer #\_)]
            )
          (or 
            (and (>= curr lower-a) (<= curr lower-z))
            (and (>= curr upper-a) (<= curr upper-z))
            (equal? curr underscore))))
    (define (char-alphanum? char) (or (char-alpha? char) (char-numeric? char)))
    (define (add-identifier start)
       (define (iter-add-identifier end)
         (if 
           (or (at-end end) (not (char-alphanum? (string-ref source end))))
                end (iter-add-identifier (+ 1 end))
          ))
       (let ([end (iter-add-identifier start)])
         (add-identifier-token (substring source start end) (- end start)))
    )
    (if (at-end current)
        tokens
        (let ([c (string-ref source current)])
          (cond
            [(equal? c #\space) (skip 1 line)]
            [(equal? c #\newline) (skip 1 (+ 1 line))]
            [(equal? c #\() (add-token 'LEFT_PAREN 1)]
            [(equal? c #\)) (add-token 'RIGHT_PAREN 1)]
            [(equal? c #\{) (add-token 'LEFT_BRACE 1)]
            [(equal? c #\}) (add-token 'RIGHT_BRACE 1)]
            [(equal? c #\,) (add-token 'COMMA 1)]
            [(equal? c #\.) (add-token 'DOT 1)]
            [(equal? c #\-) (add-token 'MINUS 1)]
            [(equal? c #\+) (add-token 'PLUS 1)]
            [(equal? c #\;) (add-token 'SEMICOLON 1)]
            [(equal? c #\*) (add-token 'STAR 1)]
            [(equal? c #\!) (if (match-next #\=) (add-token 'BANG_EQUAL 2) (add-token 'BANG 1))]
            [(equal? c #\=) (if (match-next #\=) (add-token 'EQUAL_EQUAL 2) (add-token 'EQUAL 1))]
            [(equal? c #\<) (if (match-next #\=) (add-token 'LESS_EQUAL 2) (add-token 'LESS 1))]
            [(equal? c #\>) (if (match-next #\=) (add-token 'GREATER_EQUAL 2) (add-token 'GREATER 1))]
            [(equal? c #\/) (if (match-next #\/) (recur start (next-pos current) line tokens) (add-token 'SLASH 1))]
            [(equal? c #\") (add-string current)]
            [(char-numeric? c) (add-number current)]
            [(char-alpha? c) (add-identifier current)]
            [else (raise (cons "Unrecognized character: " (list c line)) #t)]
  ))))
  (reverse (recur 0 0 1 '()))
)
