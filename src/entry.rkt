#lang racket
(provide generate-entry)

(require net/sendurl)

;;;; Work in progress
;;; An "entry" is a selector within a menu that is stylised before being
;;; inserted into the editor.


;;; Temporarily handled as ordinary strings
;;; TODO: Rewrite as snip%s.
(define (generate-entry line)
  ;; Don't bother preparing an entry if it's not a real menu item.
  ;; I sure hope this ordinary line doesn't contain a tab character.
  (if (string-contains? line "\t")
      (let* ((++ string-append)
             ;; Strings must be untrimmed to accommodate for blank i lines.
             (entry (string-split (substring line 1) "\t" #:trim? #f))
             (type (substring line 0 1))
             (text (first entry))
             (location ; address : port location
              (++
               (third entry) ":" (fourth entry) (second entry))))
        (case type
          ;; Messages are displayed outright.
          (("i")
           ;; Titles should be bold.
           (if (string=? (second entry) "TITLE")
               (++ "Page title: " text)
               text))
          ;; Errors are like messages but should be displayed
          ;; in red or something.
          (("3")
           (++ "Error: " text))
          ;; Text files should be rendered properly.
          (("0")
           (++ "[txt] " text " | " location))
          ;; Directories should be clickable and navigable.
          (("1")
           (++ "[dir] " text " | " location))
          ;; Web pages should be handled through send-url.
          (("h")
           (if (and (> (string-length (second entry)) 4)
                    (string=? "URL:" (substring (second entry) 0 4)))
               (++ "[web] " text " -> "
                   (substring (second entry) 4))
               (++ "[html] " text " | " location)))
          ;; I guess we need an image viewer.
          (("g" "I")
           (++ "[img] " text " | " location))
          ;; Binary files shouldn't be rendered in the browser
          ;; but downloaded directly.
          (("4" "5" "6" "9" "c" "d" "e" "s" ";")
           (++ "[bin] " text " | " location))
          ;; Input string for query.
          (("7")
           (++ "[input] " text " | " location))
          ;; Duplicate entries.
          (("+")
           (++ "[dup] " text " | " location))
          ;; I honestly have no idea how to handle telnet entries.
          (("T" "8")
           (++ "[telnet] " text " | " location))
          ;; Nor CSO phonebook entries.
          (("2")
           (++ "[pbx] " text " | " location))
          (else
           (++ "Unrecognised type: " type " (" text ")"))))
      ;; Return line on its own if it's an ordinary string.
      ;; For example, an error message.
      line))
