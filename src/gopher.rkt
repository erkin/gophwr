#lang racket
(provide get-file)

(require "config.rkt")
(require "const.rkt")

(require racket/tcp)
(require openssl)


;; We're using a simple in-house parser because the official one is too
;; complicated for our simple but specific needs.
;; We won't be dealing with URL schemes or parameters, but we'll have to
;; deal with URIs that contain ';' and '	' as ordinary characters.
(define magic-regexp
  ;; Domain probably doesn't work with IPv6 right now.
  ;; Port and path are optional.
  ;; File type is mandatory if there's a path.
  (regexp
   (string-append
    "^"              ; Regexp begins here
      "([^/?#:]*)"   ; Domain can be anything, including IP addresses
      "(:[0-9]+)?"   ; Port
      "(/"           ; Path begins here
        "([^/])"     ; Single character file type
        "(/[^?#]*)*" ; Rest of the path
      ")?"           ; Path ends here
    "/?$"            ; Regexp ends here
    )))


;; Just a nitpick.
(define string->char
  (compose car string->list))

;;; \r\n is mandatory in Gopher.
(define (write-line str out)
  (display (string-append str "\r\n") out))

;; To let the user know the error comes from the client, not the server.
(define (error-string . strs)
  (cons 'error string))

;; Return a pair of file type and contents.
(define (read-input in type)
  (case type
    ((#\1)
     (cons 'menu (port->lines in #:line-mode 'return-linefeed)))
    ((#\0 #\7 #\m #\M #\p #\x)
     (cons 'text (port->lines in #:line-mode 'return-linefeed)))
    ((#\4 #\5 #\6 #\9 #\c #\d #\e #\s #\;)
     ;; TODO: Save the file.
     (cons 'binary (port->bytes in)))
    ((#\g)
     ;; TODO: Try to render the GIF.
     (cons 'gif (port->bytes in)))
    ((#\I)
     ;; TODO: Determine image type and try to render it.
     (cons 'image (port->bytes in)))
    (else
     (close-input-port in)
     (error-string "File type not recognised."))))

(define (connect-server host port type path)
  (define-values (in out)
    ;; Try to connect with SSL if it's enabled.
    ;; Doesn't work reliably.
    ;; TODO: Add back the fallback mechanism.
    ((if (and *tls-enabled?* ssl-available?)
         ssl-connect/enable-break
         tcp-connect/enable-break)
     host port))
  ;; Request the desired path.
  (write-line path out)
  (flush-output out)
  ;; Do something with the input port based on the type of the file
  ;; we're expecting.
  (let ((result (read-input in type)))
    (if (or (non-empty-string? result) (bytes? result))
        (begin
          (close-output-port out)
          result)
        (error-string "The server returned nothing."))))

(define (get-file address)
  ;; format: (address domain :port /type/path type /path)
  (let ((urn (regexp-match magic-regexp address)))
    ;; "foo.bar:69/0/baz/quux" becomes:
    ;;   '("foo.bar:69/0/baz/quux" "foo.bar" ":69"
    ;;     "/0/baz/quux" "0" "/baz/quux")
    (if (and urn (non-empty-string? (first urn)))
        (let ((host (second urn))
              (port (third  urn))
              (type (fifth  urn))
              (path (sixth  urn)))
          (connect-server
           host
           (if port
               ;; ":70" -> 70
               (string->number (substring port 1))
               ;; Fall back to 70 by default.
               70)
           ;; Fall back to an empty selector by default, which is the
           ;; top-level directory.
           (string->char (or type "1"))
           (or path "")))
        ;; Return an error if the address is not valid.
        (error-string (string-append "Invalid path: " address)))))
