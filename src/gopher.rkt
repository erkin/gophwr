#lang racket
(provide fetch-file)

(require (only-in racket/tcp tcp-connect/enable-break))
(require (only-in    openssl ssl-connect/enable-break))
(require "config.rkt"
         "const.rkt")


;;; \r\n is mandatory in Gopher.
(define (write-line str out)
  (display (string-append str "\r\n") out))

(define/contract (fetch-file host port path #:type type)
  (-> string? exact-positive-integer? string? #:type symbol?
      (or/c (listof string?) bytes?))
  ;; Try to connect with SSL if it's enabled.
  (let-values (((in out)
                ;; TODO: Fallback to plaintext if TLS handshake fails.
                ((if (tls-enabled?)
                     ssl-connect/enable-break
                     tcp-connect/enable-break)
                 host port)))
    ;; Request the desired file.
    (write-line path out)
    ;; Output port mustn't be closed before the input is completely
    ;; read. port->bytes and port->lines automatically close their
    ;; respective input ports.
    (flush-output out)
    (case type
      ((text) (let ((result (port->lines in)))
                (if result
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
