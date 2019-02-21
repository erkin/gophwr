#lang racket/base
(provide dial-server)

(require "config.rkt")

(require (only-in racket/tcp tcp-connect))
(require (only-in openssl ssl-connect))


;;; \r\n is mandatory in Gopher
(define (write-line str out)
  (display (string-append str "\r\n") out))

;;; Read the menu line by line until we come across ".\r\n"
;;; or EOF, since not every server is compliant.
(define (read-loop in lines)
  (let ((line (read-line in 'return-linefeed)))
    (if (or (eof-object? line) (string=? line "."))
        ;; Close the port and return the list of lines
        (begin
          (close-input-port in)
          lines)
        (read-loop
         in (append lines (list line))))))

(define (connect-server connect host port path)
  (with-handlers
    ((exn:fail:network?
      ;; If SSL connection fails, try again as plaintext.
      (λ (ex)
        (if (eq? connect ssl-connect)
            (connect-server tcp-connect host port path)
            ;; If plaintext connection fails, return the exception.
            ex))))
    (let-values (((in out) (connect host port)))
      ;; Request the desired path.
      (write-line path out)
      (close-output-port out)
      ;; Read what server returns.
      (read-loop in '()))))

(define (dial-server host port path)
  (connect-server
   ;; Try to connect with SSL if it's enabled.
   (if *ssl-enabled?*
       ssl-connect
       tcp-connect)
   host port path))
