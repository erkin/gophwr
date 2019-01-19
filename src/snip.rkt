#lang racket/gui
(provide parse-entry
         entry-snip% (rename-out (entry-snip-class snip-class)))
(require racket/snip)


;;; Work in progress
(define entry-snip%
  (class snip%
    (inherit set-snipclass
             get-flags set-flags
             get-admin)
    (super-new)
    (set-snipclass entry-snip-class)
    (send (get-the-snip-class-list) add entry-snip-class)
    (set-flags (cons 'handles-events (get-flags)))))

(define entry-snip-class%
  (class snip-class%
    (inherit set-classname)
    (super-new)
    (set-classname (~s '((lib "snip.rkt" "entry-snip")
                         (lib "window.rkt" "entry-snip"))))))

(define entry-snip-class (new entry-snip-class%))


(define (parse-entry line)
  (define new-snip
    (new entry-snip%))
  (let ((type (substring line 0 1))
        (entry (string-split (substring line 1) "\t" #:trim? #f)))
    (case type
      (("i" "3") ; message
       (if (string=? (cadr entry) "TITLE")
           (string-append "Page title: " (car entry) "\n")
           (string-append (car entry) "\n")))
      (("0" "m" "M" "p" "x") ; text
       (string-append "[txt] " (car entry) " | "
                      (caddr entry) (cadr entry) "\n"))
      (("1") ; directory
       (string-append "[dir] " (car entry) " | "
                      (caddr entry) (cadr entry) "\n"))
      (("h") ; web
       (string-append "[web] " (car entry)
                      " -> " (cadr entry) "\n"))
      (("g" "I") ; image
       (string-append "[img] " (car entry)
                      " | " (caddr entry) (cadr entry) "\n"))
      (("2" "4" "5" "6" "9" "c" "d" "e" "s" ";") ; binary
       (string-append "[bin] " (car entry)
                      " | " (caddr entry) (cadr entry) "\n"))
      (else ; todo: 7, +, T/8?
       (string-append "Unrecognised type: " type
                      " (" (car entry) ")\n")))))

