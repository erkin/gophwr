#lang racket/gui
(provide initialise-window go-to)

(require "config.rkt"
         "const.rkt"
         "entry.rkt"
         "gopher.rkt")


;;; The current page address
(define address "")
;;; Pseudostacks that hold previous and next addresses
(define previous-address '())
(define next-address '())
;;; Thread for TCP connection (see gopher.rkt)
(define thread-custodian #f)


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

;; (define edit-menu
;;   (new menu% (parent menu-bar)
;;        (label "&Edit")))

;; (define view-menu
;;   (new menu% (parent menu-bar)
;;        (label "&View")))

(define help-menu
  (new menu% (parent menu-bar)
       (label "&Help")))

(define (populate-menu-bar)
  ;; Navigate to the currently loaded address
  (new menu-item% (parent file-menu)
       (label "&Refresh")
       (callback (λ _
                   (refresh))))
  ;; Gracefully shutdown the thread.
  (new menu-item% (parent file-menu)
       (label "&Stop")
       (callback (λ _
                   (custodian-shutdown-all thread-custodian)
                   (loaded)
                   (send frame set-status-text "\U26A0 Stopped!"))))
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
  (populate-menu-bar)

  (send* *theme*
    (set-face *font*)
    (set-delta-foreground *fg-colour*)
    (set-delta-background *bg-colour*))
  (send* page-text
    (change-style *theme*)
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
        (string-append "Loading " urn " \u231B")) ; hourglass
  (send address-field set-value urn)
  (begin-busy-cursor))

(define (loaded)
  (send frame set-status-text "")
  (send frame set-label
        (string-append *project-name*
                       " \u2014 " address))
  (send page-text scroll-to-position 0)
  (end-busy-cursor))


;;; Navigation
(define (refresh)
  (go-to address))

(define (go-back)
  (unless (null? previous-address)
    (set! next-address (cons address next-address))
    (go-to (car previous-address))
    (set! previous-address (cdr previous-address))))

(define (go-forward)
  (unless (null? next-address)
    (go-to (car next-address))
    (set! next-address (cdr next-address))))

(define (go)
  (go-to (send address-field get-value)))

(define (go-to uri)
  (define urn
    ;; Strip out URL scheme from the address.
    (if (and (> (string-length uri) 8)
             (string=? (substring uri 0 9) "gopher://"))
        (substring uri 9)
        uri))
  ;; Wipe the screen, regardless of whether the address is blank.
  (clear-page)
  ;; Navigate to the URN if it is not empty.
  (when (non-empty-string? urn)
    ;; Update the history stack, omitting duplicate or blank entries
    (unless (string=? address urn)
      (when (non-empty-string? address)
        (set! previous-address (cons address previous-address)))
      ;; Refresh the global address value, if the URL scheme was
      ;; stripped out.
      (set! address urn))
    ;; Start the thread to dial the address and render the page.
    (set! thread-custodian (make-custodian (current-custodian)))
    (thread (λ ()
              (current-custodian thread-custodian)
              (loading urn)
              (send page-text insert (get-page urn))
              (loaded)))))
