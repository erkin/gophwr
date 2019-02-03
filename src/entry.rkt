#lang racket
(provide generate-entry)

;;;; Work in progress
;;; An "entry" is a selector within a gophermap that is stylised
;;; before being inserted into the editor.
;;; "1" entries should be clickable.

;;; TODO: Gopher+ support

(define (generate-entry line)
  (let ((entry (string-split (substring line 1) "\t" #:trim? #f))
        (type (substring line 0 1)))
    (case type
      ;; Messages are displayed outright.
      (("i")
       ;; Titles should be bold.
       (if (string=? (cadr entry) "TITLE")
           (string-append "Page title: " (car entry) "\n")
           (string-append (car entry) "\n")))
      ;; Errors are like messages but should be displayed
      ;; in red or something.
      (("3")
       (string-append "Error: " (car entry) "\n"))
      ;; Text files should be rendered properly.
      (("0" "m" "M" "p" "x")
       (string-append "[txt] " (car entry) " | "
                      (caddr entry) (cadr entry) "\n"))
      ;; Directories should be clickable and navigable.
      (("1")
       (string-append "[dir] " (car entry) " | "
                      (caddr entry) (cadr entry) "\n"))
      ;; Web pages should be handled through xdg-open
      ;; to delegate to a web browser.
      (("h")
       (string-append "[web] " (car entry)
                      " -> " (cadr entry) "\n"))
      ;; I guess we need an image viewer.
      (("g" "I")
       (string-append "[img] " (car entry)
                      " | " (caddr entry) (cadr entry) "\n"))
      ;; Binary files shouldn't be rendered in the browser
      ;; but downloaded directly.
      (("2" "4" "5" "6" "9" "c" "d" "e" "s" ";")
       (string-append "[bin] " (car entry)
                      " | " (caddr entry) (cadr entry) "\n"))
      ;; Input string for query.
      (("7")
       (string-append "[input] " (car entry)
                      " | " (caddr entry) (cadr entry) "\n"))
      ;; Duplicate entries.
      (("+")
       (string-append "[dup] " (car entry)
                      " | " (caddr entry) (cadr entry) "\n"))
      ;; I honestly have no idea how to handle telnet entries.
      (("t" "8")
       (string-append "[telnet] " (car entry)
                      " | " (caddr entry) (cadr entry) "\n"))
      (else
       (string-append "Unrecognised type: " type
                      " (" (car entry) ")\n")))))
