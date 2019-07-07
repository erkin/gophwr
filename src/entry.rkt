#lang racket/base
(provide render-menu render-text
         write-file initialise-styles
         d-usual d-error)

(require racket/gui/base racket/class racket/match)
(require net/sendurl)
(require "const.rkt"
         "config.rkt"
         "parser.rkt")


(define d-usual (make-object style-delta%))
(define d-title (make-object style-delta%))
(define d-error (make-object style-delta%))
(define d-menu (make-object style-delta%))
(define d-link (make-object style-delta%))
(define d-document (make-object style-delta%))
(define d-binary (make-object style-delta%))
(define d-image (make-object style-delta%))
(define d-clicked (make-object style-delta%))

(define (initialise-styles)
  (send* d-usual
    (set-family 'modern)
    (set-face font-name)
    (set-delta 'change-size font-size)
    (set-delta-foreground fg-colour)
    (set-delta-background bg-colour))
  (send* d-title
    (copy d-usual)
    (set-delta 'change-size title-size))
  (send* d-error
    (copy d-usual)
    (set-delta-foreground error-colour))
  ;; Menus we can navigate to
  (send* d-menu
    (copy d-usual)
    (set-delta-foreground menu-colour))
  ;; Outgoing (web) links
  (send* d-link
    (copy d-usual)
    (set-delta-foreground link-colour))
  ;; Links to text files we can render
  (send* d-document
    (copy d-usual)
    (set-delta-foreground document-colour))
  ;; Binary files we can download
  (send* d-binary
    (copy d-usual)
    (set-delta-foreground binary-colour))
  ;; Images that can be rendered
  (send* d-image
    (copy d-usual)
    (set-delta-foreground image-colour))
  ;; Style after clicking
  (send* d-clicked
    (copy d-usual)
    (set-delta-foreground clicked-colour)))


(define (render-text page content go-to)
  (send page change-style d-usual)
  (for-each (λ (line)
              (send* page (insert line) (insert "\n")))
            content))

(define (write-file file-path content #:mode (mode 'binary))
  (when file-path
    (let ((output-file (open-output-file
                        file-path #:mode mode #:exists 'replace)))
      (if (eq? mode 'binary)
          (write-bytes-avail/enable-break content output-file)
          (write-string content output-file))
      (close-output-port output-file))))

(define (render-menu page lines go-to)
  (define (insert-text style str)
    (send* page
      ;; Set the desired style before inserting the text
      (change-style style)
      (insert str)))
  (define (insert-selector
           style str clickback
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
        (set-clickback before after clickback d-clicked)
        (insert "\n"))))
  (define (render-menu-item line)
    ;; Pesky end-of-file dot...
    (unless (member line '("" "."))
      (match-let* (((list type text path domain port) (parse-selector line))
                   (address (string-append domain ":" port "/" type path))
                   (click (λ _ (go-to address))))
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
          (("0" "M" "c" "e" "m" "w" "x")
           (insert-selector d-document text click #:decorator "txt"))
          (("H" "h")
           ;; Check for web links.
           (if (and (> (string-length path) 4)
                    (string=? "URL:" (substring path 0 4)))
               (insert-selector d-link text
                                ;; Send it to default web browser.
                                (λ _ (send-url (substring path 4) #f))
                                #:decorator "url")
               (insert-selector d-document text click
                                #:decorator "htm")))
          (("7")
           (insert-selector
            d-menu text
            (λ _
              (when-let (query (get-text-from-user "Query" text))
                        (go-to (string-append address "\t" query))))
            #:decorator " ? "))
          (("I" "g" "p" ":")
           (insert-selector d-image text click #:decorator "img"))
          (("4" "5" "6" "9" "P" "d" "s" ";" "<")
           (insert-selector d-binary text click #:decorator "bin"))
          (("+")
           (insert-selector d-menu text click #:decorator "dup"))
          (("2" "8" "T")
           (insert-selector
            d-error text
            ;; Warn the user that we don't do CSO/telnet.
            (λ _
              (message-box "Unsupported type"
                           "Session types (CSO/telnet) are not supported."
                           #f '(ok caution)))
            #:decorator "tel"))
          (else
           (insert-text d-error
                        (string-append "Unknown selector type: " type))
           (insert-text d-usual (string-append " " text "\n")))))))
  ;; Read and render each line of the menu.
  (for-each render-menu-item lines)
  ;; Reset style after rendering the page.
  (send page change-style d-usual))
