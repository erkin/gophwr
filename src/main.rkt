#lang racket/base

(require racket/cmdline)
(require (only-in openssl ssl-available? ssl-load-fail-reason))

(require "const.rkt"
         "config.rkt"
         "window.rkt")


(define (display-version)
  (for-each displayln *version-message*)
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

  (when (tls-enabled?)
    (unless ssl-available?
      (displayln "OpenSSL is not available."
                 ssl-load-fail-reason
                 "Aborting.")
      (exit 1)))

  (initialise-window)
  
  ;; Automatically navigate to the first argument at startup.
  (go-to (car addresses)))
