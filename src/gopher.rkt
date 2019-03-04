#lang racket
(provide dial-server)

(require "config.rkt")

(require racket/exn)
(require racket/tcp)
(require openssl)


;;; \r\n is mandatory in Gopher
(define (write-line str out)
  (display (string-append str "\r\n") out))

;;; TODO: Eschew the line-based approach and slurp the whole file in one
;;; go as binary, then parse it later.

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
            ;; If plaintext connection fails, return the exception message.
            (cons 'error (exn->string ex))))))
    (let-values (((in out) (connect host port)))
      ;; Request the desired path.
      (write-line path out)
      (close-output-port out)
      ;; Read what server returns.
      (read-loop in '()))))

;;; Right now, the window thread expects either a list of strings
;;; or ('error . "error message").
;;; Ideally, we should be using structs here. (TODO)
(define (dial-server host port path)
  (let ((results
         (connect-server
          ;; Try to connect with SSL if it's enabled.
          (if *tls-enabled?*
              ssl-connect
              tcp-connect)
          ;; Default to :70.
          host (if port port 70) path)))
    (if (empty? results)
        (cons 'error "Server returned nothing.")
        results)))
