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
  ;; TODO: Add a fallback mechanism.
  (let-values (((in out)
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
    ;; TODO: Cleanup, better error handling.
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
