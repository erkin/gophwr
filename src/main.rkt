#lang racket/base

(require racket/cmdline)
(require (only-in openssl ssl-available? ssl-load-fail-reason))

(require "const.rkt"
         "config.rkt"
         "window.rkt")


(define (display-version)
  (for-each displayln version-message)
  (exit null))

(module+ main
  (define addresses
    (command-line
     #:program project-name
     #:once-each
     (("--ssl" "--tls")
      "Enable TLS mode"
      (tls-enabled? #t))
     (("--version" "-v")
      "Show version and licence information"
      (display-version))

     ;; List of arguments to navigate to at startup.
     ;; The reason we do it with a list is to be able to handle multiple
     ;; addresses as separate tabs in the future.
     #:args addresses
     addresses))

  (when (tls-enabled?)
    (unless ssl-available?
      (displayln "OpenSSL is not available."
                 ssl-load-fail-reason
                 "Aborting.")
      (exit 1)))

  (initialise-window)

  (if (null? addresses)
      ;; Go to homepage if no argument is given.
      (if homepage
          (go-to homepage #:history #t)
          ;; Blank page if homepage is #f.
          (clear-page))
      ;; Go to first address given at commandline.
      (go-to (car addresses) #:history #t)))
