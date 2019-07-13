#lang racket/base
(provide small-icon large-icon huge-icon)

(require racket/class
         racket/draw
         racket/runtime-path
         (for-syntax racket/base))

(define-runtime-path icon-directory (build-path 'up "assets" "icons"))

(define (load-icon name)
  (make-object bitmap% (build-path icon-directory name) 'png/alpha))

(define small-icon
  (load-icon "16x16.png"))

(define large-icon
  (load-icon "32x32.png"))

(define huge-icon
  (load-icon "128x128.png"))
