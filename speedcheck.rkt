#lang racket
(require racket/tcp)
(require "speedcheck-lib.rkt")

(define (speedcheck-clent)
  (displayln (vector-ref (current-command-line-arguments) 0)))

(speedcheck)