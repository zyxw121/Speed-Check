#lang racket
 
(provide
 speedcheck-server
 speedcheck-client)

(require racket/tcp)

;serve a client with the given channels
(define (serve in out)
  (define x (read-byte in))
  (displayln x)
  (match x
    [0
     (print "download req")
     (write-byte 10 out)               
               (serve in out)]
    [1 (handle-upload in out)
             (serve in out)]
    [_ null])
)
; have to handle eof somehow

(define (handle-download in out)
  (for/list ([i (in-range (read-byte in))])
    (write-bytes (make-bytes (* 1000 1000) 0) out)
    (flush-output)))

(define (handle-upload in out)
  (for/list ([i (in-range (read-byte in))])
    (read-bytes (* 1000 1000) in)))

(define (request-upload n in out)
;  (send-command 'Upload out)
  (for/list ([i (in-range n)])
    (write-bytes (make-bytes (* 1000 1000) 0) in)))

(define (request-download n in out)
;  (send-command 'Download out)
  (write-bytes (make-bytes 1 n) out)
  (flush-output out)
  (for/list ([i (in-range n)])
    (read-bytes (* 1000 1000) in)))

(define (speedcheck-server)
  (define listener (tcp-listen 8081))
  (define (loop) 
    (let-values ([(in out) (tcp-accept listener)])
      (print "client connected")
      (serve in out)
      ;(displayln (read-byte in))
      ;(write-string "Hello" out)
      ;(flush-output out)
      (close-input-port in)
      (close-output-port out)
      (print "client gone"))
    (loop))
  (print "started")
  (loop))

(define (with-time f)
  (define-values (a b c d) (time-apply f null))
  (print b)
  b)

(define (speedcheck-client)
  (define-values (in out) (tcp-connect "localhost" 8081))
  (displayln "connected")
  (with-time (lambda () (request-download 10 in out)))
  (close-input-port in)
  (close-output-port out))