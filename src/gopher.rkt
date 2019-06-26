#lang racket/base
(provide fetch-file)

(require racket/contract/base racket/contract/region
         racket/port racket/tcp)
(require openssl)
(require "config.rkt")


;;; \r\n is mandatory in Gopher.
(define (write-line str out)
  (display (string-append str "\r\n") out))

(define/contract (fetch-file host port path #:type type)
  (-> string? exact-positive-integer? string? #:type symbol?
      (or/c (listof string?) bytes?))
  ;; Try to connect with TLS if it's enabled.
  (let-values (((in out)
                ;; TODO: Add an option to retry with clear connection if
                ;; TLS handshake fails.
                (cond
                  ;; https://github.com/erkin/gophwr/wiki/TLS#100k-convention
                  ((and ssl-available? (> port 100000))
                   (ssl-connect/enable-break host (- port 100000)))
                  ((tls-enabled?)
                   (ssl-connect/enable-break host port))
                  (else
                   (tcp-connect/enable-break host port)))))
    ;; Request the desired file.
    (write-line path out)
    ;; Output port mustn't be closed before the input is completely
    ;; read. port->bytes and port->lines automatically close their
    ;; respective input ports.
    (flush-output out)
    (case type
      ((text) (let ((result (port->lines in)))
                (if (pair? result)
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
