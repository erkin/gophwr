#lang racket
(require "const.rkt")
(require "config.rkt")
(require "window.rkt")

(define (display-version)
  (map displayln *version-message*)
  (exit null))

(module+ main
  (define initial-address
    (command-line
     #:program (string-downcase *project-name*)

     #:once-any
     (("--version" "-v")
      "Show version and licence information"
      (display-version))

     ;; Take a list of arguments and return the first if it exists,
     ;; return the homepage otherwise.
     ;; The reason we do it with a list is to be able to handle multiple
     ;; addresses as separate tabs in the future.
     #:args arguments
     (if (null? arguments)
         *homepage*
         (car arguments))))

  (initialise-window)
  
  ;; Automatically navigate to the first argument at startup.
  (navigate initial-address))
