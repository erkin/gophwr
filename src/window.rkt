#lang racket/gui
(provide initialise-window go-to)

(require "config.rkt"
         "const.rkt"
         "entry.rkt"
         "gopher.rkt"
         "parser.rkt")

(define *theme*
  (new style-delta%))

;;; The current page address
(define address "")
;;; Pseudostacks that hold previous and next addresses
(define previous-address '())
(define next-address '())

;;; Main window
(define frame
  (new frame%
       (label
        (string-append *project-name* " \u2014 " address))
       (width *initial-width*)
       (height *initial-height*)))

;;;; Menubar
(define menu-bar
  (new menu-bar% (parent frame)))

(define file-menu
  (new menu% (parent menu-bar)
       (label "&File")))

(define help-menu
  (new menu% (parent menu-bar)
       (label "&Help")))

(define (populate-menu-bar)
  ;; Save page to file.
  ;; Note that this saves the formatted version of menus.
  (new menu-item% (parent file-menu)
       (label "&Download")
       (callback (λ _
                   (send page-text save-file "" 'text))))
  (new menu-item% (parent file-menu)
       (label "&Quit")
       (callback (λ _
                   (exit '()))))
  ;; message-box is good enough for this.
  (new menu-item% (parent help-menu)
       (label "&About")
       (callback (λ _
                   (message-box
                    (string-append "About " *project-name*)
                    (string-join *version-message* "\n") frame
                    '(ok no-icon))))))


;;;; Address box
(define address-pane
  (new horizontal-pane% (parent frame)
       (alignment '(left top))
       (stretchable-width #t)
       (stretchable-height #f)
       (horiz-margin 10)))


;;;; Keys
(define back-key
  (new button% (parent address-pane)
       (label "\u2397")
       (callback (λ _ (go-back)))))
(define forward-key
  (new button% (parent address-pane)
       (label "\u2398")
       (callback (λ _ (go-forward)))))

(define refresh-key
  (new button% (parent address-pane)
       (label "\u21bb")
       (callback (λ _ (refresh)))))

(define home-key
  (new button% (parent address-pane)
       (label "\u2302")
       (callback (λ _ (go-to *homepage*)))))

(define address-field
  (new text-field% (parent address-pane)
       (label "")
       (init-value address)
       ;; Call navigate-addressbar iff the callback event is pressing return key.
       (callback (λ (f event)
                   (when (equal? (send event get-event-type) 'text-field-enter)
                     (go-to (send f get-value)))))))

(define address-button
  (new button% (parent address-pane)
       (label "\u2388") ; Helm sign
       (callback (λ _ (go)))))


;;;; Page view
(define page-canvas
  (new editor-canvas% (parent frame)
       ;; I need a better way to handle auto-wrap/hscroll
       (style '(no-focus no-hscroll auto-vscroll))
       (scrolls-per-page *scrolls-per-page*)
       (wheel-step *wheel-step*)
       ;; Minimum size the canvas can be shrunk to is 16 lines.
       (line-count 16)
       (stretchable-width #t)
       (stretchable-height #t)))

(define page-text
  (new text%
   (line-spacing 0.6)
       (auto-wrap #f)))


;;; GUI starts here.
(define (initialise-window)
  (error-display-handler
   (λ (str ex)
     (loaded)
     (error-page str)))

  (initialise-styles)
  (populate-menu-bar)

  (send* page-text
    (change-style d-usual)
    (set-max-undo-history 0))
  (send* page-canvas
    (set-canvas-background *bg-colour*)
    (set-editor page-text))

  ;; Here we go!
  (send frame create-status-line)
  (send frame show #t))


;;; Frame and page details
(define (clear-page)
  (send page-text select-all)
  (send page-text clear))

(define (loading urn)
  (send frame set-status-text
        (string-append "Loading " urn " \u231b")) ; hourglass
  (send address-field set-value urn)
  (send page-text begin-edit-sequence)
  (send page-canvas enable #f)
  (begin-busy-cursor))

(define (loaded)
  (send frame set-status-text "")
  (send frame set-label
        (string-append *project-name*
                       " \u2014 " address))
  (send page-text scroll-to-position 0)
  (when (send page-text in-edit-sequence?)
    (send page-text end-edit-sequence))
  (send page-canvas enable #t)
  (end-busy-cursor))


(define (error-page . strs)
  (clear-page)
  (loaded)
  (send frame set-status-text "Error!")
  (send page-text insert
        (string-append* *project-name* " error: " strs)))

;;; Navigation
(define (refresh)
  (go-to address))

(define (go-back)
  ;; Note that previous-address contains the current address as well.
  (when (> (length previous-address) 1)
    (set! next-address (cons address next-address))
    (let ((prev (cadr previous-address)))
      (set! previous-address (cddr previous-address))
      ;; To make sure the forward stack isn't munged.
      (go-to prev #:history #t))))

(define (go-forward)
  (unless (null? next-address)
    (let ((next (car next-address)))
      (set! next-address (cdr next-address))
      (go-to next #:history #t))))

(define (go)
  (go-to (send address-field get-value)))

(define (go-to uri #:history (history #f))
  (let-values
      (((urn domain port type path)
        ;; Strip out URL scheme from the address.
        (parse-urn (string-trim
                    (if (and (> (string-length uri) 8)
                             (string=? (substring uri 0 9) "gopher://"))
                        (substring uri 9)
                        uri) #:repeat? #t))))
    ;; Do nothing if the address is blank.
    (when (non-empty-string? urn)
      (case type
        ((#\0 #\1 #\7 #\m #\M #\p #\x)
         ;; Don't discard the next address stack if we're moving back
         ;; and forth.
         (unless history
           (set! next-address '()))
         (to-text urn domain port type path))
        ((#\4 #\5 #\6 #\9 #\c #\d #\e #\s #\;)
         (to-binary urn domain port type path))
        ((#\g #\I)
         (to-image urn domain port type path))
        (else
         (error-page "File type not recognised."))))))

(define (to-text urn domain port type path)
  (clear-page)
  (unless (string=? address urn)
    ;; Refresh the global address value, in case URL scheme was
    ;; stripped out.
    (set! address urn)
    ;; Omit duplicate entries.
    (when (or (null? previous-address)
            (not (string=? address (car previous-address))))
      (set! previous-address (cons address previous-address))))
  ;; Start the thread to dial the address and render the menu.
  (thread (λ ()
            (loading urn)
            ((if (char=? type #\1)
                 render-menu
                 render-text)
             page-text (fetch-file domain port path #:type 'text)
             go-to)
            (loaded))))

(define (to-binary urn domain port type path)
  (thread (λ ()
            (loading urn)
            (save-binary (fetch-file domain port path #:type 'binary))
            (loaded))))

;;; TODO: Display the image instead of downloading it.
(define to-image to-binary)
