#lang racket/gui
(require "const.rkt")
(require "config.rkt")
(require "window.rkt")


(define (prepare-theme)
  (send* *theme*
    (set-face *font*)
    (set-delta-foreground *fg-colour*)
    (set-delta-background *bg-colour*))
  (send page-text change-style *theme*)
  (send* page-canvas
    (set-canvas-background *bg-colour*)
    (set-editor page-text)))

(define (display-version)
  (map displayln *version-message*)
  (exit '()))

(define (initialise-window)
  (populate-menu-bar)
  (populate-options)
  (prepare-theme)

  ;; Here we go.
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
