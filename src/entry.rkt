#lang racket/gui
(provide render-menu render-text
         save-binary initialise-styles
         d-usual)

(require net/sendurl)
(require "config.rkt"
         "parser.rkt")


(define d-usual (make-object style-delta%))
(define d-title (make-object style-delta%))
(define d-error (make-object style-delta%))
(define d-menu (make-object style-delta%))
(define d-link (make-object style-delta%))
(define d-document (make-object style-delta%))
(define d-download (make-object style-delta%))
(define d-clicked (make-object style-delta%))

(define (initialise-styles)
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
  ;; Style after click
  (send* d-clicked
    (copy d-usual)
    (set-delta-foreground *clicked-colour*)))


(define (render-text page content go-to)
  (send page change-style d-usual)
  (for-each (λ (line)
              (send* page (insert line) (insert "\n")))
            content))

(define (save-binary content)
  (let ((file-path (put-file)))
    (when file-path
      (let ((output-file (open-output-file
                          file-path #:mode 'binary #:exists 'replace)))
        (write-bytes-avail/enable-break content output-file)
        (close-output-port output-file)))))

(define (render-menu page content go-to)
  (let* ((insert-text
          (λ (style str)
            (send* page
              ;; Set the desired style before inserting the text
              (change-style style)
              (insert str)
              ;; Then reset it.
              (change-style d-usual))))
         (insert-selector
          (λ (style str clickback
                    #:decorator (decorator #f)
                    #:justified? (justified #f))
            (when decorator
              (insert-text d-usual (string-append "[" decorator "] ")))
            (when justified
              (send page insert (make-string 6 #\space)))
            (let ((before (send page get-start-position))
                  (_ (insert-text style str))
                  (after (send page get-start-position)))
              (send* page
                (set-clickback before after clickback)
                (insert "\n"))))))
    (for-each
     (λ (line)
       ;; Pesky end-of-file dot...
       (unless (string=? line ".")
         (let-values (((type text path domain port)
                       (parse-selector line)))
           (let ((click (λ _ (go-to (string-append domain ":" port "/" type path)))))
             (case type
               (("i")
                (if (string=? path "TITLE")
                    (insert-text d-title text)
                    (insert-text d-usual (string-append
                                          (make-string 6 #\space) text)))
                (send page insert "\n"))
               (("1")
                (insert-selector d-menu text click #:justified? #t))
               (("3")
                (send page insert (make-string 6 #\space))
                (insert-text d-error text))
               (("0")
                (insert-selector d-document text click #:decorator "txt"))
               (("+")
                (insert-selector d-menu text click #:decorator "dup"))
               (("h")
                (if (and (> (string-length path) 4)
                         (string=? "URL:" (substring path 0 4)))
                    (insert-selector d-link text
                                     (λ _ (send-url (substring path 4)))
                                     #:decorator "url")
                    (insert-selector d-document text click "htm")))
               (("7")
                (insert-selector
                 d-menu text (λ _
                               (message-box
                                "Unimplemented"
                                "Queries are not yet implemented."))
                 #:decorator "???"))
               (("g" "I")
                (insert-selector d-download text click #:decorator "img"))
               (("4" "5" "6" "9" "c" "d" "e" "s" ";")
                (insert-selector d-download text click #:decorator "bin"))
               (else
                (insert-text d-usual text)))))))
     content)))
