#lang racket/base
(provide (all-defined-out))

(require (only-in racket/draw make-color))

;;;; These will be adjustable in the options menu.

(define *homepage* "suika.erkin.party:70/1/gophwr")

(define *download-folder* #f)

(define *scrolls-per-page* 500)
(define *wheel-step* 1)

(define *initial-width* 1024)
(define *initial-height* 768)

(define tls-enabled? (make-parameter #f))
(define auto-wrap? (make-parameter #f))

;; A string containing the name of the font
;; #f for default monospace.
(define *font* #f)

(define *font-size* 11)
(define *title-size* 23)

;; Besides hexcodes, strings of colour names are accepted as well.
;; To see a list of colours, try:
;; (require racket/draw) (for-each displayln (send the-color-database get-names))

(define *fg-colour*
  (make-color #xEE #xEE #xEE))
(define *bg-colour*
  (make-color #x11 #x11 #x11))

(define *menu-colour*
  (make-color #xAA #xAA #xEE))
(define *link-colour*
  (make-color #xAA #xEE #xAA))
(define *error-colour*
  (make-color #xEE #xAA #xAA))
(define *document-colour*
  (make-color #xEE #xEE #xAA))
(define *download-colour*
  (make-color #xEE #xAA #xEE))
(define *clicked-colour*
  (make-color #xAA #xEE #xEE))
