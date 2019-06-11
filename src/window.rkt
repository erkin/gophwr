#lang racket/gui
(provide initialise-window go-to clear-page)

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
;; We're overriding frame% to be able to implement window-wide
;; keybindings.
(define frame
  (new
   (class frame% (super-new)
     (define/override (on-subwindow-char receiver event)
       (or (handle-keycombo event)
           (send this on-menu-char event)
           (send this on-system-menu-char event)
           (send this on-traverse-char event))))
   (label project-name)
   (width initial-width)
   (height initial-height)))

(define/contract (handle-keycombo key)
  (-> (is-a?/c key-event%) (or/c void? false/c))
  (let ((ctrl? (send key get-control-down))
        (meta? (send key get-meta-down))
        (key-code (send key get-key-code)))
    (cond
      ((eq? key-code 'f5)
       (refresh))
      ((and meta? (eq? key-code 'left))
       (go-back))
      ((and meta? (eq? key-code 'right))
       (go-forward))
      ((and meta? (eq? key-code 'home))
       (go-to homepage))
      ((and ctrl? (eq? key-code #\l))
       (send address-field focus)
       (send (send address-field get-editor) select-all))
      (else
       #f))))

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
  (new checkable-menu-item% (parent tools-menu)
       (label "Enable TL&S")
       (help-string "Exclusively use TLS when connecting to gopherholes")
       (checked (tls-enabled?))
       (callback (λ (item event)
                   (if (send item is-checked?)
                       (tls-enabled? #t)
                       (tls-enabled? #f))))))

(define (populate-menu-bar)
  (unless ssl-available?
    (send tls-toggle enable #f))
  ;; Save page to file.
  ;; Note that this saves the formatted version of menus.
  (new menu-item% (parent file-menu)
       (label "&Save")
       (help-string "Save current file to disk")
       (shortcut #\s)
       (callback
        (λ _
          (let* ((filename (put-file "Choose a download location"
                                     frame download-folder (last (string-split address "/")))))
            (when filename
              (save-file filename
                         (send page-text get-text)
                         #:mode 'text))))))
  (new separator-menu-item% (parent file-menu))
  (new menu-item% (parent file-menu)
       (label "&Quit")
       (help-string "Exit gophwr")
       (shortcut #\q)
       (callback (λ _
                   (exit null))))
  (new menu-item% (parent help-menu)
       (label "&About")
       (help-string "Show version and licence info")
       (callback (λ _
                   (message-box
                    (string-append "About " project-name)
                    (string-join version-message "\n") frame
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
       (label "\u2b05") ; Back arrow
       (enabled #f)
       (horiz-margin 0)
       (callback (λ _ (go-back)))))
(define forward-key
  (new button% (parent address-pane)
       (label "\u27a1") ; Forward arrow
       (enabled #f)
       (horiz-margin 0)
       (callback (λ _ (go-forward)))))

(define refresh-key
  (new button% (parent address-pane)
       (label "\u21bb") ; Clockwise arrow
       (horiz-margin 0)
       (callback (λ _ (refresh)))))

(define home-key
  (new button% (parent address-pane)
       (label "\u2302") ; House sign
       (horiz-margin 0)
       (callback (λ _ (go-to homepage)))))

(define address-field
  (new text-field% (parent address-pane)
       (label "")
       (init-value address)
       (style '(single))
       ;; Call navigate-addressbar iff the callback event is pressing return key.
       (callback (λ (f event)
                   (when (equal? (send event get-event-type) 'text-field-enter)
                     (go-to (send f get-value)))))))

(define address-button
  (new button% (parent address-pane)
       (label "\u2388") ; Helm sign
       (vert-margin 1)
       (horiz-margin 0)
       (callback (λ _ (go)))))

;;;; Page view
(define page-canvas
  (new editor-canvas% (parent frame)
       ;; I need a better way to handle auto-wrap/hscroll
       (style '(no-focus auto-hscroll auto-vscroll))
       (wheel-step wheel-step)
       (stretchable-width #t)
       (stretchable-height #t)))

(define page-text
  (new text%
       (auto-wrap auto-wrap?)))


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
    (set-canvas-background bg-colour)
    (set-editor page-text)
    (lazy-refresh #t))

  ;; Here we go!
  (send frame create-status-line)
  (send frame show #t))


;;; Frame and page details
(define (clear-page)
  (send page-text erase))

(define (loading urn)
  (send* frame
    (set-status-text
     (string-append "\u231b Loading " urn)) ; hourglass
    (modified #t))
  (send address-field set-value urn)
  (send page-text begin-edit-sequence)
  (send page-canvas enable #f)
  (begin-busy-cursor))

(define (loaded)
  (send* frame
    (set-status-text "")
    (set-label
     (string-append project-name " \u2014 " address)) ; em-dash
    (modified #f))
  (send page-text scroll-to-position 0)
  (when (send page-text in-edit-sequence?)
    (send page-text end-edit-sequence))
  (send page-canvas enable #t)
  (end-busy-cursor))

(define (error-page . strs)
  (clear-page)
  (loaded)
  (send frame set-status-text "Error!")
  (send* page-text
    (change-style d-error)
    (insert (string-append* project-name " error: " strs))))

(define/contract (make-image-snip image-bytes type)
  (-> bytes? symbol? (is-a?/c image-snip%))
  (make-object image-snip%
               (make-object bitmap%
                            (open-input-bytes image-bytes)
                            type bg-colour #t)))

;;; Navigation
(define (refresh)
  ;; Keep history stacks intact when refreshing.
  (go-to address #:history #t))

(define (go-back)
  ;; Note that previous-address contains the current address as well.
  (unless (or (send page-text in-edit-sequence?)
              (< (length previous-address) 2))
    (set! next-address (cons address next-address))
    (let ((prev (cadr previous-address)))
      ;; To make sure the forward stack isn't munged.
      (set! previous-address (cddr previous-address))
      (when (null? previous-address)
        (send back-key enable #f))
      (send forward-key enable #t)
      (go-to prev #:history #t))))

(define (go-forward)
  (unless (or (send page-text in-edit-sequence?)
              (null? next-address))
    (let ((next (car next-address)))
      (set! next-address (cdr next-address))
      (when (null? next-address)
        (send forward-key enable #f))
      (send back-key enable #t)
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
        ;; See: https://github.com/erkin/gophwr/wiki/Entity-types
        (case type
          ;; + type isn't meant to be called directly, so we'll assume
          ;; it's a text file for convenience.
          (("0" "1" "7" "H" "M" "c" "e" "h" "m" "w" "x" "+")
           ;; Don't discard the next address stack if we're moving back
           ;; and forth.
           (unless history
             (set! next-address '())
             (send forward-key enable #f)
             (send back-key enable #t))
           (to-text urn domain port type path))
          ;; 4 and 6 are actually text but we're treating them like
          ;; binary. Watch out for a stray '.' at the end!
          (("4" "5" "6" "9" "P" "d" "s" ";" "<")
           (to-binary urn domain port type path))
          (("I" "g" "p" ":")
           (to-image urn domain port type path))
          (("2" "8" "T")
           (error-page "Session types not supported."))
          (else
           (error-page "Entity type not recognised: " (make-string type))))))))

(define (update-address urn)
  (unless (string=? address urn)
    ;; Refresh the global address value, in case URL scheme was
    ;; stripped out.
    (set! address urn)
    ;; Omit duplicate entries.
    (when (or (null? previous-address)
              (not (string=? address (car previous-address))))
      (set! previous-address (cons address previous-address)))))

(define (to-text urn domain port type path)
  (clear-page)
  (update-address urn)
  (loading urn)
  ;; Start the thread to fetch the page and display it.
  (thread
   (thunk
    ((if (member type '("1" "7"))
         render-menu
         render-text)
     page-text (fetch-file domain port path #:type 'text)
     go-to)
    (loaded)))
  (void))

(define (to-binary urn domain port type path)
  (loading urn)
  (thread
   (thunk
    (let ((filename (put-file "Choose a download location"
                              frame download-folder (last (string-split path "/")))))
      (when filename
        (save-file filename
                   (fetch-file domain port path #:type 'binary)
                   #:mode 'binary)))
    (loaded)))
  (void))

(define (to-image urn domain port type path)
  (clear-page)
  (update-address urn)
  (loading urn)
  (thread
   (thunk
    (send page-text insert
          (make-image-snip (fetch-file domain port path #:type 'binary)
                           (case type
                             (("I" ":") 'unknown/alpha)
                             (("g") 'gif/alpha)
                             (("p") 'png/alpha))))
    (loaded)))
  (void))
