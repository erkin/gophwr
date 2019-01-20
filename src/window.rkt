#lang racket/gui
(provide frame navigate
         populate-menu-bar populate-options
         page-canvas page-text get-page)

(require "config.rkt")
(require "const.rkt")
(require "gopher.rkt")
(require "snip.rkt")
(require net/url)

(define address *homepage*)
(define previous-address '())
(define next-address '())

(define dial-thread #f)


;;; Main window
(define frame
  (new frame%
       (label *project-name*)
       (width 1024)
       (height 768)))


;;;; Dialogs
(define options-dialog
  (new dialog% (parent frame)
       (label (string-append *project-name* " Preferences"))
       (width 640)
       (height 480)
       (enabled #f)))

;; TODO
(define (populate-options)
  (new message% (parent options-dialog)
       (label "Preferences")))


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
       (callback (lambda _
                   (send page-text get-file #f))))
  ;; Navigate to the currently loaded address
  (new menu-item% (parent file-menu)
       (label "&Refresh")
       (callback (lambda _
                   (navigate address))))
  ;; Kill the thread that's loading the page.
  ;; Surely there's a better way to do this.
  ;; It works but it's very buggy.
  (new menu-item% (parent file-menu)
       (label "&Stop")
       (callback (lambda _
                   (kill-thread dial-thread))))
  ;; Save page to file
  ;; Doesn't work right now.
  (new menu-item% (parent file-menu)
       (label "&Download")
       (callback (lambda _
                   (send page-text put-file #f #f))))
  ;; I hope calling exit is graceful enough.
  (new menu-item% (parent file-menu)
       (label "&Quit")
       (callback (lambda _
                   (exit '()))))
  ;; Preferences dialog doesn't have anything right now.
  (new menu-item% (parent edit-menu)
       (label "&Preferences")
       (callback (lambda _
                   (send options-dialog show #t))))
  ;; message-box is good enough for this.
  (new menu-item% (parent help-menu)
       (label "&About")
       (callback (lambda _
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

(define address-field
  (new text-field% (parent address-pane)
       (label "Address")
       (init-value address)
       ;; Call navigate-addressbar iff the callback event is pressing return key.
       (callback (lambda (activity event)
                   (when (equal? (send event get-event-type) 'text-field-enter)
                     (navigate-addressbar))))))

(define address-button
  (new button% (parent address-pane)
       (label "Go")
       (callback (lambda _ (navigate-addressbar)))))


;;;; Page view
(define page-canvas
  (new editor-canvas% (parent frame)
       ;; I need a better way to handle auto-wrap/hscroll
       (style '(no-focus no-hscroll auto-vscroll))
       (wheel-step *wheel-step*)
       ;; Minimum size the canvas can be shrunk to is 16 lines.
       (line-count 16)
       (stretchable-width #t)
       (stretchable-height #t)))

(define page-text
  (new text%
       (line-spacing 0.6)
       (auto-wrap #f)))


;;; TODO: Cleanup
(define (get-page destination)
  (begin-busy-cursor)
  ;; Prepare the canvas
  (send page-text select-all)
  (send page-text clear)
  ;; Set global var (will help with back/forward and history)
  (set! address destination)
  (define url (string->url destination))
  ;; We're assuming the absence of a scheme implies gopher for convenience.
  (unless (url-scheme url)
    (let ((new-url (string-append "gopher://" destination)))
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
     (let ((entries (dial-server (url-host url) (url-port url) (cdr path))))
       (if (string=? (car path) "1") ; Only parse gophermaps
           (for-each (lambda (line) ; TODO: Properly implement snips here.
                       (send page-text insert ; Insert entries line by line
                             (parse-entry line))) entries)
           (send page-text insert ; Insert it all at once
                 (apply string-append entries)))))
    (("file")
     (send page-text load-file path))
    (else
     (send page-text insert "Error: Unsupported URL scheme")))
  (end-busy-cursor))

(define (navigate page)
  (set! dial-thread
        (thread (lambda _
                  (get-page page)))))

(define (navigate-addressbar)
  (navigate (send address-field get-value)))
