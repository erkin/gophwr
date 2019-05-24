#lang racket
(provide parse-urn parse-selector)


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

(define selector-regexp
  (regexp
   (string-append
    "^(.)"   ; File type (mandatory)
    "(.*)\t" ; Descriptor text
    "(.*)\t" ; Path
    "(.*)\t" ; Address
    "(.*)$"  ; Port
    )))

;; Just a nitpick.
(define string->char
  (compose car string->list))

;; "foo.bar:69/0/baz/quux" becomes:
;;   '("foo.bar:69/0/baz/quux" "foo.bar" ":69"
;;     "/0/baz/quux" "0" "/baz/quux")
(define (parse-urn urn)
  ;; format: ("address" "domain" ":port" "/type/path" "type" "/path")
  (let ((parsed-urn (regexp-match magic-regexp urn)))
    (if parsed-urn
        (match-let-values (((address domain port _ type path)
                         (apply values parsed-urn)))
          ;; new format: ("address" "domain" port #\type "/path")
          (values address
                  domain
                  (if port
                      ;; ":70" -> 70
                      (string->number (substring port 1))
                      ;; Fall back to 70 by default.
                      70)
                  (if type
                      (string->char type)
                      #\1)
                  (or path "/")))
        (raise-user-error
         (string-append "Failed to parse address: " urn)))))

(define (parse-selector line)
  (let ((parsed-selector (regexp-match selector-regexp line)))
    (if parsed-selector
        (match-let-values (((_ type text path address port)
                            (apply values parsed-selector)))
          (values type
                  text
                  ;; A couple defaults, just in case.
                  (if (non-empty-string? path)
                      path "/")
                  (if (non-empty-string? address)
                      address "null.host")
                  (if (non-empty-string? port)
                      port "70")))
        (raise-user-error (string-append "Invalid selector: " line)))))
