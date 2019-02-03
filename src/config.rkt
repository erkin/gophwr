#lang racket/gui
(provide (all-defined-out))

;;;; These will be adjustable in the options menu.

(define *homepage* "gopher://suika.erkin.party:70/1/gophwr")
(define *wheel-step* 2)

(define *initial-width* 1024)
(define *initial-height* 768)

(define *theme*
  (new style-delta%))

(define *fg-colour*
  (make-object color% #xEE #xEE #xEE))
(define *bg-colour*
  (make-object color% #x11 #x11 #x11))

(define *font* "Unifont")
