#lang racket/base
(provide render-menu render-text
         initialise-styles change-style)

(require racket/gui/base
         racket/class
         racket/match)
(require net/sendurl)
(require "const.rkt"
         "config.rkt"
         "parser.rkt")


;; Style after clicking
(define d-clicked
  (send (make-object style-delta%) set-delta-foreground clicked-colour))

(define (initialise-styles style-list)
  ;; Let's initialise the "Standard" style first.
  ;; This is the editor's default style, inherited from "Basic".
  (define standard (send style-list find-named-style "Standard"))
  (define standard-delta (make-object style-delta%))
  (send* standard-delta
    (set-family 'modern)
    (set-face font-name)
    (set-delta 'change-size font-size)
    (set-delta-foreground fg-colour)
    (set-delta-background bg-colour))
  (send standard set-delta standard-delta)

  (define (make-colour-style name colour)
    ;; Each style created with this procedure copies "Standard" style
    ;; and creates a new style by name 'name' and with the foreground
    ;; colour 'colour'.
    (send (send style-list new-named-style name standard)
          set-delta (send* (make-object style-delta%)
                      (copy standard-delta)
                      (set-delta-foreground colour))))

  ;; Titles are larger than usual text
  (send (send style-list new-named-style "Title" standard)
        set-delta (send* (make-object style-delta%)
                    (copy standard-delta)
                    (set-delta 'change-size title-size)))

  ;; Error messages, obviously enough
  (make-colour-style "Error" error-colour)
  ;; Directory links we can navigate to
  (make-colour-style "Menu" menu-colour)
  ;; Outgoing (web) links
  (make-colour-style "Link" link-colour)
  ;; Links to text files we can render
  (make-colour-style "Document" document-colour)
  ;; Binary files we can download
  (make-colour-style "Binary" binary-colour)
  ;; Images that can be rendered
  (make-colour-style "Image" image-colour))

(define (change-style page style)
  (send page change-style
        (send (send page get-style-list) find-named-style style)))

(define (render-text page content go-to)
  (change-style page "Standard")
  (for-each (λ (line)
              (send* page (insert line) (insert "\n")))
            content))

(define (render-menu page lines go-to)
  (define (insert-text style str)
    ;; Set the desired style before inserting the text
    (change-style page style)
    (send page insert str))
  (define (insert-selector style str clickback
           #:decorator (decorator #f)
           #:justified? (justified #f))
    (when decorator
      (insert-text "Standard" (string-append "[" decorator "] ")))
    (when justified
      (send page insert (make-string 6 #\space)))
    (let ((before (send page get-start-position))
          (_ (insert-text style str))
          (after (send page get-start-position)))
      (send page set-clickback before after clickback d-clicked)))
  (define (render-menu-item line)
    ;; Pesky end-of-file dot...
    (unless (member line '("" "."))
      (match-let* (((list type text path domain port) (parse-selector line))
                   (address (string-append domain ":" port "/" type path))
                   (click (λ _ (go-to address))))
        (case type
          (("i")
           (if (string=? path "TITLE")
               (insert-text "Title" text)
               (insert-text "Standard" (string-append
                                        (make-string 6 #\space) text))))
          (("1")
           (insert-selector "Menu" text click #:justified? #t))
          (("3")
           (send page insert (make-string 6 #\space))
           (insert-text "Error" text))
          (("0" "M" "c" "e" "m" "w" "x")
           (insert-selector "Document" text click #:decorator "txt"))
          (("H" "h")
           ;; Check for web links.
           (if (and (> (string-length path) 4)
                    (string=? "URL:" (substring path 0 4)))
               (insert-selector "Link" text
                                ;; Send it to default web browser.
                                (λ _ (send-url (substring path 4) #f))
                                #:decorator "url")
               (insert-selector "Document" text click
                                #:decorator "htm")))
          (("7")
           (insert-selector
            "Menu" text
            (λ _
              (when-let (query (get-text-from-user "Query" text))
                        (go-to (string-append address "\t" query))))
            #:decorator " ? "))
          (("I" "g" "p" ":")
           (insert-selector "Image" text click #:decorator "img"))
          (("4" "5" "6" "9" "P" "d" "s" ";" "<")
           (insert-selector "Binary" text click #:decorator "bin"))
          (("+")
           (insert-selector "Menu" text click #:decorator "dup"))
          (("2" "8" "T")
           (insert-selector
            "Error" text
            ;; Warn the user that we don't do CSO/telnet.
            (λ _
              (message-box "Unsupported type"
                           "Session types (CSO/telnet) are not supported."
                           #f '(ok caution)))
            #:decorator "tel"))
          (else
           (insert-text "Error"
                        (string-append "Unknown selector type: " type))
           (insert-text "Standard" text)))
        (send page insert "\n"))))
  ;; Read and render each line of the menu.
  (for-each render-menu-item lines)
  ;; Reset style after rendering the page.
  (change-style page "Standard"))
