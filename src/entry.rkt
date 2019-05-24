#lang racket/gui
(provide render-menu render-text
         save-binary)

(require "config.rkt")


(define (parse-selector line)
  (let ((selectors (regexp-match "^(.)(.*)\t(.*)\t(.*)\t(.*)$" line)))
    (if selectors
        (apply values (cdr selectors))
        (raise-user-error (string-append "Invalid selector: " line)))))

(define (render-text page content)
  (for-each (λ (line) (send page insert line)) content))

(define (save-binary content)
  (let ((file-path (put-file)))
    (when file-path
      (let ((output-file (open-output-file
                          file-path #:mode 'binary #:exists 'replace)))
        (write-bytes-avail/enable-break content output-file)
        (close-output-port output-file)))))

(define (render-menu page content)
  (let* ((++ string-append)
         (d-usual (make-object style-delta%))
         (d-menu (make-object style-delta%))
         (d-error (make-object style-delta%))
         (d-title (make-object style-delta%))
         (d-link (make-object style-delta%))
         (d-download (make-object style-delta%))
         (d-document (make-object style-delta%))
         (insert-text (λ (style str)
                        (send* page
                          (change-style style)
                          (insert str))))
         (insert-line (λ (style str)
                        (insert-text style str)
                        (send page insert "\n")))
         (insert-selector (λ (style str decorator)
                            (insert-text d-usual (++ "[" decorator "] "))
                            (insert-line style str)))
         (insert-justified (λ (style str)
                             (insert-text d-usual (make-string 6 #\space))
                             (insert-line style str))))

    (send* d-usual
      (set-family 'modern)
      (set-delta 'change-weight 'normal)
      (set-delta-foreground *fg-colour*)
      (set-delta-background *bg-colour*))
    (send* d-title
      (copy d-usual)
      (set-delta 'change-weight 'bold))
    (send* d-error
      (copy d-usual)
      (set-delta-foreground *error-colour*))
    ;; Menus we can navigate to
    (send* d-menu
      (copy d-usual)
      (set-delta-foreground *menu-colour*))
    ;; Outgoing (web) links
    (send* d-link
      (copy d-usual)
      (set-delta-foreground *link-colour*))
    ;; Links to text files we can render
    (send* d-document
      (copy d-usual)
      (set-delta-foreground *document-colour*))
    ;; Binary files we can download
    (send* d-download
      (copy d-usual)
      (set-delta-foreground *download-colour*))
    
    (for-each
     (λ (line)
       (let-values (((type text path domain port) (parse-selector line)))
         (case type
           (("i") (if (string=? path "TITLE")
                      (insert-line d-title text)
                      (insert-justified d-usual text)))
           (("1")
            (insert-justified d-menu text))
           (("3")
            (insert-justified d-error text))
           (("0")
            (insert-selector d-document text "txt"))
           (("+")
            (insert-selector d-menu text "dup"))
           (("h")
            (if (and (> (string-length path) 4)
                     (string=? "URL:" (substring path 0 4)))
                (insert-selector d-link text "www")
                (insert-selector d-document text "htm")))
           (("7")
            (insert-selector d-menu text "???"))
           (("g" "I")
            (insert-selector d-download text "img"))
           (("4" "5" "6" "9" "c" "d" "e" "s" ";")
            (insert-selector d-download text "bin"))
           (else (insert-text d-usual text)))))
     content)))


