#lang racket/base
(provide (all-defined-out))


(define project-name "gophwr")
(define project-version "v0.5.8")

(define project-home "suika.erkin.party:70/1/gophwr")

(define version-message
  (format #<<version
~a ~a
Copyright (C) 2019 Erkin Batu Altunbaş

Each file of this project's source code is subject 
to the terms of the Mozilla Public Licence v2.0
https://mozilla.org/MPL/2.0/
version
          project-name project-version))

(define-syntax-rule (if-let (value condition) yes no)
  (let ((value condition))
    (if value yes no)))

(define-syntax-rule (when-let (value condition) . body)
  (let ((value condition))
    (when value . body)))

(struct gopher (urn domain port type path))
