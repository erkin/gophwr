#lang info

(define collection "gophwr")
(define version "0.5")

(define pkg-desc "A graphical gopher client")
(define pkg-authors '("Erkin Batu Altunbaş"))

(define deps '("base" "gui-lib" "net-lib"))
(define build-deps '())

(define gracket-launcher-names '("gophwr"))
(define gracket-launcher-libraries '("main.rkt"))
