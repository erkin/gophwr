#lang racket/gui
(provide initialise-window navigate)

(require framework)
(require net/url)

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
(define dial-thread #f)


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

(define edit-menu
  (new menu% (parent menu-bar)
       (label "&Edit")))

;; (define view-menu
;;   (new menu% (parent menu-bar)
;;        (label "&View")))

(define help-menu
  (new menu% (parent menu-bar)
       (label "&Help")))

(define (populate-menu-bar)
  ;; Open local file
  (new menu-item% (parent file-menu)
       (label "&Open")
       (callback
        (λ _
          (navigate
           (url->string (path->url ; oh god why
                         (send page-text get-file
                               (find-system-path 'home-dir))))))))
  ;; Navigate to the currently loaded address
  (new menu-item% (parent file-menu)
       (label "&Refresh")
       (callback (λ _
                   (navigate address))))
  ;; Kill the thread that's loading the page.
  ;; Surely there's a better way to do this.
  ;; It works but it's very buggy.
  (new menu-item% (parent file-menu)
       (label "&Stop")
       (callback (λ _
                   (kill-thread dial-thread))))
  ;; Save page to file
  ;; Note that this saves the formatted version of menus
  (new menu-item% (parent file-menu)
       (label "&Download")
       (callback (λ _
                   (send page-text save-file "" 'text))))
  (new menu-item% (parent file-menu)
       (label "&Quit")
       (callback (λ _
                   (exit:exit))))
  ;; Preferences dialog doesn't have anything right now.
  ;; (new menu-item% (parent edit-menu)
  ;;      (label "&Preferences")
  ;;      (callback (λ _
  ;;                  (preferences:show-dialog))))
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
;;; Stubs for now
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
       (callback (λ _ (navigate *homepage*)))))

(define address-field
  (new text-field% (parent address-pane)
       (label "")
       (init-value address)
       ;; Call navigate-addressbar iff the callback event is pressing return key.
       (callback (λ (f event)
                   (when (equal? (send event get-event-type) 'text-field-enter)
                     (navigate (send f get-value)))))))

(define address-button
  (new button% (parent address-pane)
       (label "\u2388") ; Helm sign
       (callback (λ _ (navigate (send address-field get-value))))))


;;;; Page view
(define page-canvas
  (new canvas:color% (parent frame)
       ;; I need a better way to handle auto-wrap/hscroll
       (style '(no-focus no-hscroll auto-vscroll))
       (scrolls-per-page *scrolls-per-page*)
       (wheel-step *wheel-step*)
       ;; Minimum size the canvas can be shrunk to is 16 lines.
       (line-count 16)
       (stretchable-width #t)
       (stretchable-height #t)))

(define page-text
  (new text:basic%
       (line-spacing 0.6)
       (auto-wrap #f)))


;;; GUI starts here.
(define (initialise-window)
  (application:current-app-name *project-name*)
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


;;; TODO: This seriously needs a cleanup.
;;; Fetches the page from gopher or file URLs and renders it.
;;; Must be called in a thread. See navigate below.
(define (get-page url)
  ;; Concatenate path components
  (define path
    (let ((path-string (string-join (map path/param-path (url-path url)) "/")))
      (cond
        ((string=? (url-scheme url) "file") ; File links have no data types.
         (string-append "/" path-string)) ; Always an absolute path.
        ;; Take the first element of the path
        ((non-empty-string? path-string)
         (cons (substring path-string 0 1) (substring path-string 1)))
        (else ; a Gopher home page link
         (cons "1" "/")))))

  (case (url-scheme url)
    (("gopher")
     (send page-text begin-edit-sequence #f #f)
     ;;; dial-server returns a list of lines returned from the server
     ;;; or 'error symbol if connection attempt failed.
     (let ((entries (dial-server (url-host url) (url-port url) (cdr path))))
       (if (eq? entries 'error)
           (insert-error "Unable to connect to " (url->string url))
           (case (car path)
             ;;; MENU
             (("1")
              ;; Insert entries line by line.
              (for-each (λ (line)
                          (send page-text insert
                                ;; Parse each line and return entries.
                                (generate-entry line))) entries))
             ;;; TEXT
             (("0" "m" "M" "p" "x")
              ;; Insert the text all at once.
              (send page-text insert
                    (apply string-append entries)))
             ;;; other?
             (else
              ;; TODO: Save binary files.
              ;; ";" (video) type is especially problematic
              ;; as the URL parser skips through it.
              (insert-error "I don't know how to handle " (car path) " type.")))))
     (send page-text end-edit-sequence))
    (("file")
     (with-handlers ((exn:fail:filesystem?
                      (λ _
                        (insert-error "Unable to open file " path))))
       (send page-text load-file path 'text)))
    (else
     (insert-error "Unsupported URL scheme: " (url-scheme url))))

  ;; This'll have to do until I figure out
  ;; how to disable insertion scrolling.
  (send page-text scroll-to-position 0)
  (send frame set-status-text "")
  (send frame set-label
        (string-append *project-name* " \u2014 " address)))


(define (insert-error . strings)
  (send page-text insert (string-append* "Error: " strings)))

;;; Start the thread to fetch the page and render it
(define (navigate uri)
  ;; Wipe the canvas
  (send page-text select-all)
  (send page-text clear)

  (define url
    (let ((scheme (url-scheme (string->url uri))))
      (if scheme
          ;; Check if string->url misinterpreted the port.
          ;; The URI foo.bar:70 is somehow interpreted to be a URL with
          ;; the scheme "foo.bar" with "70" being the path string.
          (if (string-contains? scheme ".")
              ;; In this case, our URI clearly has no scheme.
              (string-append "gopher://" uri)
              ;; Only now can we assume there's actually a separate
              ;; URL scheme. It had better be "file".
              uri)
          ;; We're assuming the absence of a scheme implies gopher
          ;; for convenience.
          (string-append "gopher://" uri))))

  ;; Don't add duplicate or blank entries to the history stack
  (unless (string=? address url)
    (when (non-empty-string? address)
      (set! previous-address (cons address previous-address)))
    (set! address url))

  (send frame set-status-text
        (string-append "Loading " url " \u231B")) ; hourglass
  (send address-field set-value url)

  ;; Start the thread
  (set! dial-thread
        (thread (λ _
                  (gui-utils:show-busy-cursor
                   (λ _
                     (get-page (string->url url))))))))

(define (go-back)
  (unless (null? previous-address)
    (set! next-address (cons address next-address))
    (navigate (car previous-address))
    (set! previous-address (cdr previous-address))))

(define (go-forward)
  (unless (null? next-address)
    (navigate (car next-address))
    (set! next-address (cdr next-address))))
