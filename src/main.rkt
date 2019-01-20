#lang racket/gui
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

  (populate-menu-bar)
  (populate-options)

  ;; Prepare the theme and apply it.
  ;; Values are defined in config.rkt
  (send* *theme*
    (set-face *font*)
    (set-delta-foreground *fg-colour*)
    (set-delta-background *bg-colour*))
  (send page-text change-style *theme*)
  (send* page-canvas
    (set-canvas-background *bg-colour*)
    (set-editor page-text))
  
  ;; Here we go.
  (send frame show #t)

  ;; Automatically navigate to homepage at startup.
  (navigate *homepage*))
