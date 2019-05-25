#lang racket/base

(require racket/cmdline)
(require openssl)

(require "const.rkt"
         "config.rkt"
         "window.rkt")


(define (display-version)
  (map displayln *version-message*)
  (exit null))

(module+ main
  (define addresses
    (command-line
     #:program *project-name*

     #:once-each
     (("--ssl" "--tls")
      "Enable TLS mode"
      (tls-enabled? #t))

     #:once-any
     (("--version" "-v")
      "Show version and licence information"
      (display-version))

     ;; Take and return a list of addresses if any argument is given,
     ;; return the homepage otherwise.
     ;; The reason we do it with a list is to be able to handle multiple
     ;; addresses as separate tabs in the future.
     #:args addresses
     (if (null? addresses)
         (list *homepage*)
         addresses)))

  (if (tls-enabled?)
      (writeln "TLS option enabled.")
      (unless ssl-available?
        (writeln "However, OpenSSL is not available.")
        (writeln ssl-load-fail-reason)
        (writeln "Aborting.")
        (exit 1)))

  (initialise-window)
  
  ;; Automatically navigate to the first argument at startup.
  (go-to (car addresses)))
