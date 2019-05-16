#lang racket/base
(provide get-page)

(require "config.rkt")
(require "const.rkt")
(require "entry.rkt")

(require racket/list)
(require racket/string)
(require racket/tcp)
(require openssl)


;; Just a nitpick.
(define string->char
  (compose car string->list))

;;; \r\n is mandatory in Gopher
(define (write-line str out)
  (display (string-append str "\r\n") out))

(define (read-lines in)
  (define (read-loop in lines)
    (let ((line (read-line in 'return-linefeed)))
      (if (or (eof-object? line) (string=? line "."))
          (begin
           (close-input-port in)
           lines)
          (read-loop in (append lines (list line))))))
  (read-loop in '()))

;; To let the user know the error comes from the client, not the server.
(define (error-string . strs)
  (string-append* *project-name* " Error: " strs))

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
    "^"                ; Regexp begins here
      "([^/?#:]*)"     ; Domain can be anything, including IP addresses
      "(:[0-9]+)?"     ; Port
      "(/"             ; Path begins here
        "([^/])"       ; Single character file type
        "(/[^?#]*)*"   ; Rest of the path
      ")?/?"           ; Path ends here
    "$"                ; Regexp ends here
    )))

(define (get-page address)
  (let ((urn (regexp-match magic-regexp address)))
    ;; (address domain :port /type/path type /path)
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
           ;; Fall back to / by default, which is the main menu.
           (string->char (or type "1"))
           (or path "/")))
        ;; Return an error if the address is not valid.
        (error-string "Invalid path: " address))))

(define (connect-server host port type path)
  (define-values (in out)
    ;; Try to connect with SSL if it's enabled.
    ;; Doesn't work reliably.
    ;; TODO: Add back the fallback mechanism.
    ((if *tls-enabled?*
         ssl-connect
         tcp-connect)
     host port))
  ;; Request the desired path.
  (write-line path out)
  (flush-output out)
  (close-output-port out)
  ;; Read what the server returns.
  (let ((result
         (case type
           ((#\1)
            (generate-entries (read-lines in)))
           ((#\0 #\m #\M #\p #\x)
            (string-join (read-lines in) "\n"))
           ((#\g)
            ;; Try to render it.
            (port->bytes in))
           ((#\I)
            ;; Determine file type and try to render it.
            (port->bytes in))
           ((#\4 #\5 #\6 #\9 #\c #\d #\e #\s #\;)
            ;; Save the file.
            (port->bytes in))
           (else
            "File type unrecognised."))))
    (if (non-empty-string? result)
        result
        (error-string "The server returned nothing."))))
