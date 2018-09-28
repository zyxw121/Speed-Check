#lang racket
(require racket/tcp)

(define (handle-download in out)
 ; (display "Starting DL ")
  (define n (string->number (read-line in)))
 ; (display n)
  ;(displayln " kB")
    (write-bytes (make-bytes (* 1000 n) 0) out)
    (flush-output))
 ; (displayln "DL finished"))


(define (handle-upload in out)
  (display "Starting UL ")
  (define n (string->number (read-line in)))
  (display n)
  (displayln " kB")
  (read-bytes (* 1000 n) in)
  (displayln "UL finished"))


(define (send-kilo out)
  (write-bytes (make-bytes 1000 0) out)
  (display "sent kilo ")
  (flush-output))

(define (send-mega out)
  (write-bytes (make-bytes (* 1000 1000) 0) out)
  (display "sent mega ")
  (flush-output))

(define (start-serve listener)
  (let-values ([(in out) (tcp-accept listener)])
   ; (displayln "client connected")
      (serve in out)
  ;  (displayln "client disconnected")
      (close-input-port in)
      (close-output-port out)))

;serve a client with the given channels
(define (serve in out)
 ; (displayln "waiting for cmd")
  (define cmd (read-byte in))
  ;(display "got cmd: ")
 ; (displayln cmd)
  (match cmd
    [0 (handle-download in out)
               (serve in out)]
    [1 (handle-upload in out)
             (serve in out)]
    [2 null] ; proper exit
    [3 (send-kilo out)]
    [4 (send-mega out)]
    [_ null])
)
; have to handle eof somehow

(define (speedcheck-server)
  (define listener (tcp-listen 8080))
  (define (my-thread) (start-serve listener))
  (define (loop)  
    (if (tcp-accept-ready? listener) (start-serve listener) null)
    (loop))
  (loop))


(speedcheck-server)
