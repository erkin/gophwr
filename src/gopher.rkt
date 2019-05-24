#lang racket
(provide fetch-file)

(require "config.rkt"
         "const.rkt")

(require racket/tcp)
(require openssl)


;;; \r\n is mandatory in Gopher.
(define (write-line str out)
  (display (string-append str "\r\n") out))

(define (fetch-file host port path #:type type)
  ;; Try to connect with SSL if it's enabled.
  ;; Doesn't work reliably.
  ;; TODO: Add back the fallback mechanism.
  (let-values (((in out)
                ((if (and *tls-enabled?* ssl-available?)
                     ssl-connect/enable-break
                     tcp-connect/enable-break)
                 host port)))
    ;; Request the desired file.
    (write-line path out)
    (flush-output out)
    ;; TODO: Cleanup, better error handling.
    (case type
      ((text) (let ((result (port->lines in #:line-mode 'return-linefeed)))
                (if (non-empty-string? (first result))
                    (begin
                      (close-output-port out)
                      result)
                    (raise-user-error "Server returned no lines."))))
      ((binary) (let ((result (port->bytes in)))
                  (if (bytes? result)
                      (begin
                        (close-output-port out)
                        result)
                      (raise-user-error "Server returned no bytes.")))))))
