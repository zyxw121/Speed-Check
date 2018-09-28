#lang racket
(require racket/tcp)

(define (handle-download in out)
  (define n (string->number (read-line in)))
    (write-bytes (make-bytes (* 1000 n) 0) out)
    (flush-output))


(define (handle-upload in out)
  (define n (string->number (read-line in)))
  (read-bytes (* 1000 n) in))


(define (start-serve listener)
  (let-values ([(in out) (tcp-accept listener)])
      (serve in out)
      (close-input-port in)
      (close-output-port out)))

;serve a client with the given channels
(define (serve in out)
  (define cmd (read-byte in))
  (match cmd
    [0 (handle-download in out)
               (serve in out)]
    [1 (handle-upload in out)
             (serve in out)]
    [_ null]))


(define (speedcheck-server)
  (define listener (tcp-listen 8080))
  (define (my-thread) (start-serve listener))
  (define (loop)  
    (if (tcp-accept-ready? listener) (start-serve listener) null)
    (loop))
  (loop))


(speedcheck-server)
