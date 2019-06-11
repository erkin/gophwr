#lang racket/base
(provide (all-defined-out))
(require (only-in racket/draw make-color)
         (only-in racket/file get-preference))

(define config-file (build-path (find-system-path 'pref-dir) "gophwr.rktd"))

(define tls-enabled? (make-parameter #f))

;;; Preferences
(define-syntax-rule (define-preference symbol default)
  (define symbol
    (get-preference (quote symbol) (λ () default) 'timestamp config-file)))

(define-preference homepage "suika.erkin.party:70/1/gophwr")
(define-preference download-folder #f)

(define-preference scrolls-per-page 500)
(define-preference wheel-step 1)
(define-preference auto-wrap? #f)

(define-preference initial-width 1024)
(define-preference initial-height 768)

(define-preference font-name #f)
(define-preference font-size 11)
(define-preference title-size 23)

;;; Colours
(define-syntax-rule (define-colour colour rgb)
  (define colour
    (apply make-color (get-preference (quote symbol) (λ () rgb) 'timestamp config-file))))

(define-colour fg-colour '(#xEE #xEE #xEE))
(define-colour bg-colour '(#x11 #x11 #x11))

(define-colour menu-colour '(#xAA #xAA #xEE))
(define-colour link-colour '(#xAA #xEE #xAA))
(define-colour error-colour '(#xEE #xAA #xAA))
(define-colour document-colour '(#xEE #xEE #xAA))
(define-colour download-colour '(#xEE #xAA #xEE))
(define-colour clicked-colour '(#xAA #xEE #xEE))
