#lang racket/base

(require racket/cmdline)
(require (only-in openssl
                  ssl-available? ssl-load-fail-reason))
(require "const.rkt"
         "config.rkt"
         "window.rkt")


(define addresses
  (command-line
   #:program project-name
   #:once-each
   (("--ssl" "--tls")
    "Start with TLS mode enabled"
    (tls-enabled? #t))
   (("--debug")
    "Enable debug menu"
    (debug-mode? #t))
   (("--version" "-v")
    "Show version and licence information"
    (displayln version-message)
    (exit))

   ;; List of arguments to navigate to at startup.
   ;; The reason we do it with a list is to be able to handle multiple
   ;; addresses as separate tabs in the future.
   #:args addresses
   addresses))

(module+ main
  (when (tls-enabled?)
    (unless ssl-available?
      (raise-user-error "TLS not available:"
                        ssl-load-fail-reason
                        "Aborting.")
      (exit 1)))

  (initialise-window)

  (if (null? addresses)
      ;; Go to homepage if no argument is given.
      (if homepage
          (go-to homepage #:history #t)
          ;; Blank page if homepage is #f.
          (clear-page page-text))
      ;; Go to first address given at commandline.
      (go-to (car addresses) #:history #t)))
