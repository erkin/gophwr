#lang racket/gui
(provide parse-entry
         entry-snip% (rename-out (entry-snip-class snip-class)))
(require racket/snip)

;;; Ideally, we should convert each line read from the connection into
;;; our custom clickable snips with separate styles by type


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

(define entry-snip-class
  (new entry-snip-class%))


(define (parse-entry line)
  (define new-snip
    (new entry-snip%))
  (let ((type (substring line 0 1))
        (entry (string-split (substring line 1) "\t" #:trim? #f)))
    (case type
      ;; Messages are displayed outright
      (("i")
       ;; Titles should be bold
       (if (string=? (cadr entry) "TITLE")
           (string-append "Page title: " (car entry) "\n")
           (string-append (car entry) "\n")))
      ;; Errors are like messages but should be displayed in red or something
      (("3")
       (string-append "Error: " (car entry) "\n"))
      ;; Text files should be rendered properly
      (("0" "m" "M" "p" "x")
       (string-append "[txt] " (car entry) " | "
                      (caddr entry) (cadr entry) "\n"))
      ;; Directories should be clickable and navigable
      (("1")
       (string-append "[dir] " (car entry) " | "
                      (caddr entry) (cadr entry) "\n"))
      ;; Web pages should be handled through xdg-open
      ;; to delegate to a web browser
      (("h") ; web
       (string-append "[web] " (car entry)
                      " -> " (cadr entry) "\n"))
      ;; I guess we need an image viewer.
      (("g" "I")
       (string-append "[img] " (car entry)
                      " | " (caddr entry) (cadr entry) "\n"))
      ;; Binary files shouldn't be rendered in the browser
      ;; but downloaded directly
      (("2" "4" "5" "6" "9" "c" "d" "e" "s" ";")
       (string-append "[bin] " (car entry)
                      " | " (caddr entry) (cadr entry) "\n"))
      ;; Input string for query
      (("7")
       (string-append "[input] " (car entry)
                      " | " (caddr entry) (cadr entry) "\n"))
      ;; Duplicate entries
      (("+")
       (string-append "[dup] " (car entry)
                      " | " (caddr entry) (cadr entry) "\n"))
      ;; todo: T/8?
      (else
       (string-append "Unrecognised type: " type
                      " (" (car entry) ")\n")))))

