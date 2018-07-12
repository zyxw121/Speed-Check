#lang racket

(provide
 speedcheck-server
 speedcheck-client)

(require racket/tcp)

;serve a client with the given channels
(define (serve in out)
  (match (read-command in)
    ['Download (handle-download in out)
               (serve in out)]
    ['Upload (handle-upload in out)
             (serve in out)]
    ['Quit null]))

(define (read-command in)
  (match (read-byte in)
    [0 'Download]
    [1 'Upload]
    [2 'Quit]))

(define (send-command cmd out)
  (write-byte
   (match cmd
    ['Download 0]
    ['Upload 1]
    ['Quit 2])
   out))

(define (handle-download in out)
  (for/list ([i (in-range (read-byte in))])
    (write-bytes (make-bytes (* 1000 1000) 0) out))
  (flush-output))

(define (handle-upload in out)
  (for/list ([i (in-range (read-byte in))])
    (read-bytes (* 1000 1000) in)))

(define (request-upload n in out)
  (send-command 'Upload out)
  (for/list ([i (in-range n)])
    (write-bytes (make-bytes (* 1000 1000) 0) in)))

(define (request-download n in out)
  (send-command 'Download out)
  (for/list ([i (in-range n)])
    (read-bytes (* 1000 1000) in)))

(define (speedcheck-server)
  (define listener (tcp-listen 8081))
  (print "server started")
  (let-values ([(in out) (tcp-accept listener)])
    (print "server connected")
    (serve in out)
    (close-input-port in)
    (close-output-port out))
  (speedcheck-server))

(define (with-time f)
  (define-values (a b c d) (time-apply f null))
  (print b)
  b)

(define (speedcheck-client)
  (define-values (in out) (tcp-connect "localhost" 8081))
  (displayln "connected")
  (map (lambda x (with-time (request-download 10 in out))) (sequence->list (in-range 10)))
  (close-input-port in)
  (close-output-port out))

