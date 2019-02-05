#lang racket/base
(provide dial-server)

(require (only-in racket/tcp tcp-connect))


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

(define (dial-server host port path)
  ;;; Surely there's a better way to do this.
  (call-with-escape-continuation
   (λ (escape)
     ;; Bail out with the 'error symbol if connection attempt fails.
     (define-values (in out)
       (with-handlers ((exn:fail:network? (λ _ (escape 'error))))
         ;; Assume port 70 if no port is given.
         (tcp-connect host (if port port 70))))
     (write-line path out)
     (close-output-port out)
     (read-loop in '()))))
