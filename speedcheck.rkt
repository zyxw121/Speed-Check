#lang racket
 
(provide
 speedcheck-server
 speedcheck-client)

(require racket/tcp)

;serve a client with the given channels
(define (serve in out)
  (define cmd (read-byte in))
  (match cmd
    [0 (handle-download in out)               
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
  (for/list ([i (in-range n)])
    (write-bytes (make-bytes (* 1000 1000) 0) in)))

(define (request-download n in out)
  (write-bytes (make-bytes 1 0) out)
  (write-bytes (make-bytes 1 n) out)
  (flush-output out)
  (for/list ([i (in-range n)])
    (read-bytes (* 1000 1000) in)))

(define (speedcheck-server)
  (define listener (tcp-listen 8081))
  (define (loop) 
    (let-values ([(in out) (tcp-accept listener)])
      (serve in out)
      (close-input-port in)
      (close-output-port out))
    (loop))
  (loop))

(define (with-time f)
  (define-values (a b c d) (time-apply f null))
  b)

(define (speedcheck-client)
  (define-values (in out) (tcp-connect "localhost" 8081))
  (with-time (lambda () (request-download 100 in out)))
  (close-input-port in)
  (close-output-port out))