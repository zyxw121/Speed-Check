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

; 0 <= n < 256
(define (request-upload n in out)
  (write-bytes (make-bytes 1 1) out)
  (write-bytes (make-bytes 1 n) out)
  (for/list ([i (in-range n)])
    (write-bytes (make-bytes (* 1000 1000) 0) out)))

(define (request-download n in out)
  (write-bytes (make-bytes 1 0) out)
  (write-bytes (make-bytes 1 n) out)
  (flush-output out)
  (for/list ([i (in-range n)])
    (read-bytes (* 1000 1000) in)))

(define (speedcheck-server)
  (define listener (tcp-listen 8080))
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

(define (connect-loop name disconnect in out)
  (display name)
  (display ">")
  (define cmd (read-line))
  (match cmd
    [(? (lambda (x) (regexp-match-positions #rx"up .*" x))  (app (lambda (x) (string->number (substring x 3))) (? number? n) )) 
                       (define t (with-time (lambda () (request-upload n in out))))
                       (display t)
                       (display "ms   ")
                       (display (exact->inexact (/ (round (/ (* 100000 n) t)) 100))) ;horrible
                       (displayln "MBps")
                       (connect-loop name disconnect in out)]    
    [(? (lambda (x) (regexp-match-positions #rx"down .*" x))  (app (lambda (x) (string->number (substring x 5))) (? number? n) ))
                       (define t (with-time (lambda () (request-download n in out))))
                       (display t)
                       (display "ms   ")
                       (display (exact->inexact (/ (round (/ (* 100000 n) t)) 100))) ;horrible
                       (displayln "MBps")
                       (connect-loop name disconnect in out)]
    [":d" (displayln "Disconnected from server")
                  (close-input-port in)
                  (close-output-port out)
                  (disconnect)]
    [":q" (displayln "Quitting")]
    [":?" (displayln "help")
          (connect-loop in out)]
    [_ (displayln "Bad input. ;? for help")])
  )

(define (speedcheck-client)
  (define (loop)
      (display ">")
  (define cmd (read-line))
  (match cmd
    [(? (lambda (x) (regexp-match-positions #rx"connect .*" x))  _) (define-values (in out) (tcp-connect (substring cmd 8) 8080))
                       (displayln (string-append "Connected to server " (substring cmd 8) ))
                       (connect-loop (substring cmd 8) loop in out)]
    [":q" (displayln "Quitting")]
    [":?" (displayln "help")
          (loop)]
    [_ (displayln "Bad input. :? for help")
       (loop)]))
  (displayln "Speedcheck, version 0.0.1: https://github.com/zyxw121/Speed-Check   :? for help")
  (loop))
