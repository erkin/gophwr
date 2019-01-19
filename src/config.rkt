#lang racket/gui
(provide (all-defined-out))

(define *project-name* "Gophster")
(define *homepage* "gopher://suika.erkin.party:70")
(define *wheel-step* 1)

(define *theme*
  (make-object style-delta%))

(define *fg-colour*
  (make-object color% #xEE #xEE #xEE))
(define *bg-colour*
  (make-object color% #x11 #x11 #x11))

(define *font* "Unifont")
