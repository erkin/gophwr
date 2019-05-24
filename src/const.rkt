#lang racket/base
(provide (all-defined-out))


(define *project-name* "gophwr")
(define *project-version* "v0.2.5")

(define *version-message*
  (list
   (string-append *project-name* " " *project-version*)
   "Copyright (C) 2019 Erkin Batu Altunbaş"
   ""
   "Each file of this project's source code is subject "
   "to the terms of the Mozilla Public Licence v2.0"
   "If a copy of the MPL was not distributed with this "
   "file, you can obtain one at https://mozilla.org/MPL/2.0/"))
