#lang racket/gui
(provide initialise-window go-to)

(require (only-in openssl ssl-available?))

(require "config.rkt"
         "const.rkt"
         "entry.rkt"
         "gopher.rkt"
         "parser.rkt")

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

(define tools-menu
  (new menu% (parent menu-bar)
       (label "&Tools")))

(define help-menu
  (new menu% (parent menu-bar)
       (label "&Help")))

(define tls-toggle
  (new menu-item% (parent tools-menu)
       (label (if (tls-enabled?)
                  "Disable TL&S"
                  "Enable TL&S"))
       (callback (λ (item event)
                   (cond
                     ((tls-enabled?)
                      (tls-enabled? #f)
                      (send item set-label "Enable TL&S"))
                     (else
                      (tls-enabled? #t)
                      (send item set-label "Disable TL&S")))))))

(define (populate-menu-bar)
  (unless ssl-available?
    (send tls-toggle enable #f))
  ;; Save page to file.
  ;; Note that this saves the formatted version of menus.
  (new menu-item% (parent file-menu)
       (label "&Download")
       (callback
        (λ _
          ;; Try to guess the filename.
          (let ((address-parts (string-split address "/")))
            (save-file
             frame
             (if (> (length address-parts) 2)
                 (last address-parts) #f)
             (send page-text get-text)
             #:mode 'text)))))
  (new menu-item% (parent file-menu)
       (label "&Quit")
       (callback (λ _
                   (exit '()))))
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
       (label "\u21bb") ; Clockwise arrow
       (callback (λ _ (refresh)))))

(define home-key
  (new button% (parent address-pane)
       (label "\u2302") ; House sign
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
   (auto-wrap (auto-wrap?))))


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
    (set-editor page-text)
    (lazy-refresh #t))

  ;; Here we go!
  (send frame create-status-line)
  (send frame show #t))


;;; Frame and page details
(define (clear-page)
  (send page-text select-all)
  (send page-text clear))

(define (loading urn)
  (send frame set-status-text
        (string-append "\u231b Loading " urn)) ; hourglass
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
  (unless (or (send page-text in-edit-sequence?)
              (< (length previous-address) 2))
    (set! next-address (cons address next-address))
    (let ((prev (cadr previous-address)))
      (set! previous-address (cddr previous-address))
      ;; To make sure the forward stack isn't munged.
      (go-to prev #:history #t))))

(define (go-forward)
  (unless (or (send page-text in-edit-sequence?)
              (null? next-address))
    (let ((next (car next-address)))
      (set! next-address (cdr next-address))
      (go-to next #:history #t))))

(define (go)
  (go-to (send address-field get-value)))

(define (go-to uri #:history (history #f))
  (unless (send page-text in-edit-sequence?)
    (match-let
        (((list urn domain port type path)
          ;; Strip out URL scheme from the address.
          (parse-urn (string-trim
                      (if (and (> (string-length uri) 8)
                               (string=? (substring uri 0 9) "gopher://"))
                          (substring uri 9)
                          uri) #:repeat? #t))))
      ;; Do nothing if the address is blank.
      (when (non-empty-string? urn)
        (case type
          ;; See: https://github.com/erkin/gophwr/wiki/Entity-types
          ((#\0 #\1 #\7 #\H #\M #\c #\e #\h #\m #\w #\x)
           ;; Don't discard the next address stack if we're moving back
           ;; and forth.
           (unless history
             (set! next-address '()))
           (to-text urn domain port type path))
          ;; 4 and 6 are actually text but we're treating them like
          ;; binary. Watch out for a stray '.' at the end!
          ((#\4 #\5 #\6 #\9 #\P #\d #\s #\; #\<)
           (to-binary urn domain port type path))
          ((#\I #\g #\p #\:)
           (to-image urn domain port type path))
          ((#\2 #\8 #\T)
           (error-page "Session types not supported."))
          (else
           (error-page "Entity type not recognised.")))))))

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

  (loading urn)
  ;; Start the thread to fetch the page and display it.
  (thread (λ ()
            ((if (member type '(#\1 #\7))
                 render-menu
                 render-text)
             page-text (fetch-file domain port path #:type 'text)
             go-to)
            (loaded))))

(define (to-binary urn domain port type path)
  (loading urn)
  (thread (λ ()
            (let ((path-parts (string-split path "/")))
              (if (null? path-parts)
                  (error-page "No file path given to download.")
                  (save-file frame
                             (last path-parts)
                             (fetch-file domain port path #:type 'binary)
                             #:mode 'binary)))
            (loaded))))

;;; TODO: Display the image instead of downloading it.
(define to-image to-binary)
