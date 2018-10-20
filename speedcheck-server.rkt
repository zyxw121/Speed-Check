#lang racket
(require racket/tcp)
(require racket/cmdline)

(define current-verbose-out (make-parameter (lambda (x) (void))))
(define current-port (make-parameter 8080))

(define-syntax-rule (fork e ...)
  (thread (lambda () (begin e ...))))

(define (download in out)
  (define n (string->number (read-line in)))
    (write-bytes (make-bytes (* 1000 n) 0) out)
    (flush-output))

(define (upload in out)
  (define n (string->number (read-line in)))
  (read-bytes (* 1000 n) in)
  void)

(define (write-console s)
  (displayln s)
  (display ">"))

(define (accept listener)
  (define cust (make-custodian))
  (parameterize ([current-custodian cust])
  (define-values (in out) (tcp-accept listener))
  (define (handle)
              (define cmd (read-byte in))
              (match cmd
                [0  (download in out)
                   (handle)]
                [1 (upload in out)
                   (handle)]
                [_ void]))
  (fork (with-handlers ([exn:fail? (lambda (e) (write-console "Error"))]) (handle)))
  (fork (sleep 120)
        (custodian-shutdown-all cust))))

(define (serve port)
  (define main-cust (make-custodian))
  (parameterize ([current-custodian main-cust])
    (define listener (tcp-listen port 5 #t))
    (define (loop)
      (accept listener)
      (loop))
    (thread loop))
  (lambda ()
    (custodian-shutdown-all main-cust)))

(command-line
   #:program "Speed-Check server"
   #:once-each
   [("-v" "--verbose") "Run with verbose messages"
                       (current-verbose-out displayln)]
   [("-p" "--port") port
                    "Connect on a specified port"
                    (current-port (string->number port))])

(define (speedcheck-server)
  (letrec ([stop (serve (current-port))]
           [loop (lambda () 
                   (display ">")
                   (let ([cmd (read-line)])
                     (cond [(equal? cmd "quit")
                            (displayln "Quitting Speed-Check")
                            (stop)]
                           [else (displayln "Unrecognized command. 'quit' to quit.")
                                 (loop)])))])
  (displayln "Speed-Check version 0.1.0")
  (display "Started server on port ")
  (displayln (current-port))
  (loop)))

(speedcheck-server)