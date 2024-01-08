#! /usr/bin/env racket
#lang racket

(require "scanner.rkt")

(define (file-contents filename) (port->string (open-input-file filename) #:close? #t))

(define (run source)
  (let ([chars (scan source)])
    (for ([char (in-list chars)])
      (displayln char)))
)

(define (report line where message)
  (displayln "@{line}, @{where}, @{message}"))

(define (error line message)
  (report line "" message))

(define (runfile name) (run (file-contents name)))

(define (loop) 
    (printf ">  ")
    (let ([curr-line (read-line)])
      (if
       (not (or (string=? curr-line "quit") (string=? curr-line "q")))
       (begin
         (run curr-line)
         (loop)
         )
       (printf "Exiting")
       )
     )
)

(let ([args (current-command-line-arguments)]
      [args-length (vector-length (current-command-line-arguments))])
  (cond
    [(> args-length 1) (print "what the fuck")]
    [(eq? args-length 1) (runfile (vector-ref args 0))]
    [else (loop)]
  )
 )
