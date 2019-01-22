#lang racket
(require "const.rkt")
(require "config.rkt")
(require "window.rkt")

(define (display-version)
  (map displayln *version-message*)
  (exit '()))

(module+ main
  (command-line
   #:once-any
   (("--version" "-v")
    "Show version and licence information"
    (display-version)))

  (initialise-window)
  
  ;; Automatically navigate to homepage at startup.
  (navigate *homepage*))
