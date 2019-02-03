#lang racket
(provide generate-entry)

;;;; Work in progress
;;; An "entry" is a selector within a menu that is stylised before being
;;; inserted into the editor.

;;; Temporarily handled as ordinary strings

(define (generate-entry line)
  ;; Strings must be untrimmed to accommodate for blank i lines.
  (let* ((entry (string-split (substring line 1) "\t" #:trim? #f))
         (type (substring line 0 1))
         (text (car entry))
         (location ; address : port location
          (string-append ; Port string might contain a leftover newline.
           (caddr entry) ":" (string-trim (cadddr entry)) (cadr entry))))
    (case type
      ;; Messages are displayed outright.
      (("i")
       ;; Titles should be bold.
       (if (string=? (cadr entry) "TITLE")
           (string-append "Page title: " text "\n")
           (string-append text "\n")))
      ;; Errors are like messages but should be displayed
      ;; in red or something.
      (("3")
       (string-append "Error: " text "\n"))
      ;; Text files should be rendered properly.
      (("0")
       (string-append "[txt] " text " | " location "\n"))
      ;; Directories should be clickable and navigable.
      (("1")
       (string-append "[dir] " text " | "
                      location "\n"))
      ;; Web pages should be handled through xdg-open
      ;; to delegate to a web browser.
      (("h")
       (if (string=? "URL:" (substring (cadr entry) 0 4))
           (string-append "[web] " text " -> "
                          (substring (cadr entry) 4) "\n")
           (string-append "[html] " text " | " location "\n")))
      ;; I guess we need an image viewer.
      (("g" "I")
       (string-append "[img] " text " | " location "\n"))
      ;; Binary files shouldn't be rendered in the browser
      ;; but downloaded directly.
      (("4" "5" "6" "9" "c" "d" "e" "s" ";")
       (string-append "[bin] " text " | " location "\n"))
      ;; Input string for query.
      (("7")
       (string-append "[input] " text " | " location "\n"))
      ;; Duplicate entries.
      (("+")
       (string-append "[dup] " text " | " location "\n"))
      ;; I honestly have no idea how to handle telnet entries.
      (("T" "8")
       (string-append "[telnet] " text " | " location "\n"))
      ;; Nor CSO phonebook entries.
      (("2")
       (string-append "[pbx] " text " | " location "\n"))
      (else
       (string-append "Unrecognised type: " type " (" text ")\n")))))
