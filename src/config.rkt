#lang racket/gui
(provide (all-defined-out))


;;;; These will be adjustable in the options menu.

(define *homepage* "suika.erkin.party:70/1/gophwr")

(define *download-folder* #f)

(define *scrolls-per-page* 500)
(define *wheel-step* 1)

(define *initial-width* 1024)
(define *initial-height* 768)

(define tls-enabled? (make-parameter #f))
(define auto-wrap? (make-parameter #f))

(define *font* "Unifont")

(define *fg-colour*
  (make-object color% #xEE #xEE #xEE))
(define *bg-colour*
  (make-object color% #x11 #x11 #x11))

(define *menu-colour*
  (make-object color% #xAA #xAA #xEE))
(define *link-colour*
  (make-object color% #xAA #xEE #xAA))
(define *error-colour*
  (make-object color% #xEE #xAA #xAA))
(define *document-colour*
  (make-object color% #xEE #xEE #xAA))
(define *download-colour*
  (make-object color% #xEE #xAA #xEE))
(define *clicked-colour*
  (make-object color% #xAA #xEE #xEE))
