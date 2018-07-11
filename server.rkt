#lang racket
 (require racket/tcp)

(define listener (tcp-listen 8080))

(define (loop)
  (serve)
  (loop))

(define (serve)
  (print "started")
  (let-values ([(in out) (tcp-accept listener)]) (print "serving")))
(loop)