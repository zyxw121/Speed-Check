#lang racket
(require racket/tcp)

(define (handle-download in out)
  (define n (string->number (read-line in)))
    (write-bytes (make-bytes (* 1000 n) 0) out)
    (flush-output))

(define (handle-upload in out)
  (define n (string->number (read-line in)))
  (read-bytes (* 1000 n) in)
  void)

(define (serve listener)
  (let-values ([(in out) (tcp-accept listener)])
    (thread (lambda () 
    (match (read-byte in)
    [0 (handle-download in out)]
    [1 (handle-upload in out)]
    [_ void])
      (close-input-port in)
      (close-output-port out)))))

;serve a client with the given channels
(define (speedcheck-server)
  (define listener (tcp-listen 8080))
  (define (loop)  
    (serve listener) 
    (loop))
  (loop))

(speedcheck-server)