#lang racket/base
(provide (all-defined-out))


(define project-name "gophwr")
(define project-version "v0.5.6")

(define version-message
  (format #<<version
~a ~a
Copyright (C) 2019 Erkin Batu Altunbaş

Each file of this project's source code is subject 
to the terms of the Mozilla Public Licence v2.0
If a copy of the MPL was not distributed with this 
file, you can obtain one at https://mozilla.org/MPL/2.0/
version
          project-name project-version))

(define-syntax-rule (if-let (value condition) yes no)
  (let ((value condition))
    (if value yes no)))

(define-syntax-rule (when-let (value condition) . body)
  (let ((value condition))
    (when value . body)))
