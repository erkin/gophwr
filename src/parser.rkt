#lang racket/base
(provide parse-urn parse-selector)

(require racket/contract/base
         racket/contract/region
         racket/match
         racket/string)
(require "const.rkt")


#|
We're using a simple in-house URL parser because the official one is too
complicated for our simple but specific needs.
We won't be dealing with URL schemes or parameters, but we'll have to
deal with URIs that contain ';' and '	' as ordinary characters.

For instance:
(regexp-match magic-regexp "foo.bar:69/0/baz/quux")
  '("foo.bar:69/0/baz/quux" "foo.bar" ":69" "/0/baz/quux" "0" "/baz/quux")

(Note that domain field doesn't work with IPv6 right now.)
|#

(define magic-regexp
  ;; Port and path are optional.
  ;; File type is mandatory if there's a path.
  (regexp
   (string-append
    "^"            ; Regexp begins here
      "([^/?#:]*)" ; Domain can be anything, including IP addresses
      "(:[0-9]+)?" ; Port
      "(/"         ; Path begins here
        "([^/])"   ; Single character file type
        "(/.*)*"   ; Rest of the path
      ")?"         ; Path ends here
    "/?$"          ; Regexp ends here
    )))

(define selector-regexp
  (regexp
   (string-append
    "^([^\t])"   ; File type (mandatory)
    "([^\t]*)\t" ; Descriptor text
    "([^\t]*)\t" ; Path
    "([^\t]*)\t" ; Address
    "([^\t]*)"   ; Port
    "(\t\\+)?$"  ; Gopher+
    )))

;; Just a nitpick.
(define string->char
  (compose car string->list))

(define/contract (parse-urn urn)
  ;; ("domain:port/type/path" "domain" port "type" "/path")
  (-> string? (list/c string? string? exact-positive-integer? string? string?))
  (if-let (parsed-urn (regexp-match magic-regexp urn))
          (match-let (((list address domain port _ type path) parsed-urn))
            (list address
                  domain
                  (if port
                      ;; ":70" -> 70
                      (string->number (substring port 1))
                      ;; Fall back to 70 by default.
                      70)
                  (or type "1")
                  (or path "/")))
          (raise-user-error
           (string-append "Failed to parse address: " urn))))

(define/contract (parse-selector line)
  (-> string? (non-empty-listof string?))
  (if-let (parsed-selector (regexp-match selector-regexp line))
          (match-let (((list _ type text path address port plus)
                       parsed-selector))
            (list type
                  text
                  ;; A couple defaults, just in case.
                  (if (non-empty-string? path)
                      path "/")
                  (if (non-empty-string? address)
                      address "null.host")
                  (if (non-empty-string? port)
                      port "70")))
          ;; Some non-conformant pages omit all other fields for 'i' type.
          ;; We need to make an exception for them.
          (if (member (substring line 0 1) '("i" "3"))
              (list "i" (substring line 1) "/" "null.host" "70")
              ;; Otherwise, it's too broken to render.
              (raise-user-error
               (string-append "Invalid selector: " line)))))
