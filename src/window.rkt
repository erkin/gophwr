#lang racket/gui
(provide initialise-window navigate)

(require framework)
(require net/url)
(require "config.rkt")
(require "const.rkt")
(require "gopher.rkt")
(require "entry.rkt")


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

(define view-menu
  (new menu% (parent menu-bar)
       (label "&View")))

(define help-menu
  (new menu% (parent menu-bar)
       (label "&Help")))

(define (populate-menu-bar)
  ;; Open local file
  ;; Doesn't work.
  ;; TODO: Adjust addressbar accordingly
  (new menu-item% (parent file-menu)
       (label "&Open")
       (callback
        (λ _
          (navigate
           (url->string (path->url ; oh god why
                         (send page-text get-file (find-system-path 'home-dir))))))))
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
  ;; Doesn't work right now.
  (new menu-item% (parent file-menu)
       (label "&Download")
       (callback (λ _
                   (send page-text put-file #f #f))))
  (new menu-item% (parent file-menu)
       (label "&Quit")
       (callback (λ _
                   (exit:exit))))
  ;; Preferences dialog doesn't have anything right now.
  (new menu-item% (parent edit-menu)
       (label "&Preferences")
       (callback (λ _
                   (preferences:show-dialog))))
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

;;; Stubs
(define back-key
  (new button% (parent address-pane)
       (label "\u2397")))
(define forward-key
  (new button% (parent address-pane)
       (label "\u2398")))

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
  (new editor-canvas% (parent frame)
       ;; I need a better way to handle auto-wrap/hscroll
       (style '(no-focus no-hscroll auto-vscroll))
       (scrolls-per-page 3000)
       (wheel-step *wheel-step*)
       ;; Minimum size the canvas can be shrunk to is 16 lines.
       (line-count 16)
       (stretchable-width #t)
       (stretchable-height #t)))

(define page-text
  (new text:basic%
       (line-spacing 0.6)
       (auto-wrap #f)))


;;; TODO: Cleanup
(define (get-page destination)
  (define url (string->url destination))
  ;; We're assuming the absence of a scheme implies gopher for convenience.
  (unless (url-scheme url)
    (let ((new-url (string-append "gopher://" destination)))
      (send frame set-status-text
            (string-append "Loading " new-url " \u231B"))
      (send address-field set-value new-url)
      (set! url (string->url new-url))))

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
     ;; We have some redrawing issues here. Probably a race condition.
     (send page-text begin-edit-sequence #f #f)
     (let ((entries (dial-server (url-host url) (url-port url) (cdr path))))
       (case (car path)
         (("1")
          (for-each (λ (line)
                      (send page-text insert ; Insert entries line by line
                            (generate-entry line))) entries)) ; Only parse gophermaps
         (("0" "m" "M" "p" "x")
          (send page-text insert ; Insert it all at once
                (apply string-append entries)))
         (else
          (send page-text insert
                "I don't know how to handle this type of data."))))
     (send page-text end-edit-sequence))
    (("file")
     (send page-text load-file path))
    (else
     (send page-text insert "Error: Unsupported URL scheme")))

  ;; This'll have to do until I figure out
  ;; how to disable insertion scrolling
  (send page-text scroll-to-position 0)
  (send frame set-status-text "")
  (send frame set-label
        (string-append *project-name* " \u2014 " address)))

;;; Start the thread to fetch the page and render it
(define (navigate page)
  ;; Wipe the canvas before starting the thread
  (send page-text select-all)
  (send page-text clear)

  (send address-field set-value page)
  (send frame set-status-text
        (string-append "Loading " page " \u231B"))
  ;; Set global var
  (set! address page)

  (set! dial-thread
        (thread (λ _
                  (gui-utils:show-busy-cursor
                   (λ _ (get-page page)))))))

;;; GUI portion starts here.
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

