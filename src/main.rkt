#lang racket/gui
(require "const.rkt")
(require "config.rkt")
(require "window.rkt")


(define (display-version)
  (map displayln *version-message*)
  (exit '()))

(define (initialise-window)
  (prepare-window)
  (populate-menu-bar)
  (populate-options)

  ;; Here we go!
  (send frame create-status-line)
  (send frame show #t))


(module+ main
  (command-line
   #:once-any
   (("--version" "-v")
    "Show version and licence information"
    (display-version)))

  (initialise-window)
  
  ;; Automatically navigate to homepage at startup.
  (navigate *homepage*))
