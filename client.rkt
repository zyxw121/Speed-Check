#lang racket
 (require racket/tcp)

(let-values ([(in out) (tcp-connect "localhost" 8080)]) (print "client connected")) 