#lang racket/gui
(require "config.rkt")
(require "window.rkt")

(module+ main
  (command-line
   #:once-any
   (("--version" "-v")
    "Print version and licensing information"
    (begin
      (displayln "Gophwr v0.0.1")
      (displayln "Copyright (C) 2019 Erkin Batu Altunbaş")
      (newline)
      (display   "Each of this project's source code is subject ")
      (displayln "to the terms of the Mozilla Public Licence v2.0")
      (display   "If a copy of the MPL was not distributed with this ")
      (displayln "file, you can obtain one at https://mozilla.org/MPL/2.0/"))))

  (populate-menu-bar)
  (populate-options)

  (send *theme* set-face *font*)
  (send *theme* set-delta-foreground *fg-colour*)
  (send *theme* set-delta-background *bg-colour*)
  (send page-text change-style *theme*)

  (send page-canvas set-canvas-background *bg-colour*)
  (send page-canvas set-editor page-text)
  (send frame show #t)

  (navigate *homepage*))
