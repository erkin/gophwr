#lang racket/base
(provide dial-server)

(require (only-in racket/tcp tcp-connect))


;;; \r\n is mandatory in Gopher
(define crlf (string #\return #\newline))
(define (write-line str out)
  (display (string-append str crlf) out))

;;; Read the menu line by line until we come across ".\r\n"
;;; or EOF, since not every server is compliant.
(define (read-loop in lines)
  (define line (read-line in 'return-linefeed))
  (if (or (eof-object? line) (string=? line "."))
      lines
      (read-loop
       in (append lines (list line)))))

(define (dial-server host port path)
  (define-values (in out)
    ;; Assume port 70 if no port is given.
    (tcp-connect host (if port port 70)))
  (write-line path out)
  (close-output-port out)
  (read-loop in '()))
