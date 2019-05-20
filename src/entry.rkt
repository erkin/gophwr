#lang racket/gui
(provide render-file)

(define (render-file page fetched-file)
  (let ((type    (car fetched-file))
        (content (cdr fetched-file)))
    (case type
      ((menu) (render-menu page content))
      ((text) (render-text page content))
      ((error) (render-error page content))
      ((binary) (download-binary content))
      ((image) (render-image page content))
      ((gif) (render-gif page content)))))

;;; An "entry" is a selector within a menu that is stylised before being
;;; inserted into the editor.

;; TODO: everything
(define (render-menu page content)
  (for-each (λ (line) (send page insert (generate-entry line)))))

(define (render-text page content)
  (for-each (λ (line) (send page insert line)) content))

(define (render-error page content)
  (send page insert content))

(define (download-binary content)
  (let ((file-path (put-file)))
    (when file-path
      (let ((output-file (open-output-file
                          file-path #:mode 'binary #:exists 'replace)))
        (write-bytes-avail/enable-break content output-file)
        (close-output-port output-file)))))

;; TODO
(define (render-image page content)
  (download-binary content))

;; TODO
(define (render-gif page content)
  (render-image page content))

(define (generate-entry line)
  ;; Don't bother preparing an entry if it's not a real menu item.
  ;; I sure hope this ordinary line doesn't contain a tab character.
  (if (string-contains? line "\t")
      (let* ((++ string-append)
             ;; Strings must be untrimmed to accommodate for blank i lines.
             (entry (string-split (substring line 1) "\t" #:trim? #f))
             (type (substring line 0 1))
             (text (first entry))
             (location ; address : port / type / location
              (++
               (third entry) ":" (fourth entry) "/"
               (if (non-empty-string? type) type "1")
               (second entry) "\n")))
        (case type
          ;; Messages are displayed outright.
          (("i")
           ;; Titles should be bold.
           (++ (if (string=? (second entry) "TITLE")
                   "TITLE "
                   ;; Prepend some space to align with menu items.
                   (make-string 6 #\space))
               text "\n"))
          ;; Errors are like messages but should be displayed
          ;; in red or something.
          (("3")
           (++ "ERROR " text "\n"))
          ;; Text files should be rendered properly.
          (("0")
           (++ "[txt] " text " | " location "\n"))
          ;; Directories should be clickable and navigable.
          (("1")
           (++ "[dir] " text " | " location "\n"))
          ;; Web pages should be handled through send-url.
          (("h")
           (if (and (> (string-length (second entry)) 4)
                    (string=? "URL:" (substring (second entry) 0 4)))
               (++ "[web] " text " → "
                   (substring (second entry) 4) "\n")
               (++ "[html] " text " | " location "\n")))
          ;; I guess we need an image viewer.
          (("g" "I")
           (++ "[img] " text " | " location "\n"))
          ;; Binary files shouldn't be rendered in the browser
          ;; but downloaded directly.
          (("4" "5" "6" "9" "c" "d" "e" "s" ";")
           (++ "[bin] " text " | " location "\n"))
          ;; Input string for query.
          (("7")
           (++ "[input] " text " | " location "\n"))
          ;; Duplicate entries.
          (("+")
           (++ "[dup] " text " | " location "\n"))
          ;; I honestly have no idea how to handle telnet entries.
          (("T" "8")
           (++ "[telnet] " text " | " location "\n"))
          ;; Nor CSO phonebook entries.
          (("2")
           (++ "[pbx] " text " | " location "\n"))
          (else
           (++ "Unrecognised type: " type " (" text ")\n"))))
      ;; Return line on its own if it's not an entry for some reason.
      (string-append line "\n")))
