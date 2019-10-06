#lang racket/base
(provide initialise-window go-to clear-page page-text)

(require racket/gui/base
         racket/class
         racket/format
         racket/list
         racket/match
         racket/string)
(require openssl)
(require "config.rkt"
         "const.rkt"
         "entry.rkt"
         "gopher.rkt"
         "icon.rkt"
         "parser.rkt")


;;; The current page address
(define address "")
;;; Pseudostacks that hold previous and next addresses
(define previous-address '())
(define next-address '())

;;; Tree of bookmarks
(define bookmarks user-bookmarks)

;;;; Main window
;;; We're overriding frame% to be able to implement window-wide
;;; keybindings.
(define frame
  (new
   (class frame% (super-new)
     (define (handle-keycombo key)
       (let ((ctrl? (send key get-control-down))
             (meta? (send key get-meta-down))
             (key-code (send key get-key-code)))
         (cond
           ((eq? key-code 'f5)
            (refresh))
           ((eq? key-code 'escape)
            (send search-panel show #f))
           ((and meta? (eq? key-code 'left))
            (go-back))
           ((and meta? (eq? key-code 'right))
            (go-forward))
           ((and meta? (eq? key-code 'home))
            (go-home))
           ((and ctrl? (eq? key-code #\l))
            (send address-field focus)
            (send (send address-field get-editor) select-all))
           ((and ctrl? (eq? key-code #\f))
            (show-search-field))
           ;; Return #f if we don't recognise this key code so that it can be
           ;; delegated to lower levels in on-subwindow-char (such as the
           ;; canvas or the text).
           (else
            #f))))
     (define/override (on-subwindow-char receiver event)
       (or (handle-keycombo event)
           (send this on-menu-char event)
           (send this on-system-menu-char event)
           (send this on-traverse-char event))))
   (label project-name)
   (width initial-width)
   (height initial-height)))


;;;; Address box
(define address-pane
  (new horizontal-pane% (parent frame)
       (alignment '(left top))
       (stretchable-width #t)
       (stretchable-height #f)
       (horiz-margin 10)))

(define back-key
  (new button% (parent address-pane)
       (label "\u2190") ; Back arrow
       (enabled #f)
       (horiz-margin 0)
       (callback (λ _ (go-back)))))
(define forward-key
  (new button% (parent address-pane)
       (label "\u2192") ; Forward arrow
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
       (callback (λ _ (go-home)))))

(define address-field
  (new text-field% (parent address-pane)
       (label "")
       (init-value address)
       (style '(single))
       ;; Call navigate-addressbar iff the callback event is pressing
       ;; return key.
       (callback (λ (f event)
                   (when (equal? (send event get-event-type)
                                 'text-field-enter)
                     (go))))))

(define address-button
  (new button% (parent address-pane)
       (label "\u2388") ; Helm sign
       (style '(border))
       (vert-margin 1)
       (horiz-margin 0)
       (callback (λ _ (go)))))


;;;; Page view
;;; Overriding text% to implement a right-click menu.
(define page-text
  (new
   (class text% (super-new)
     (inherit get-admin get-start-position get-end-position hide-caret)
     (define/override (on-default-event event)
       (if (send event button-down? 'right)
           ;; TODO: Fix right-click glitch
           (let ((x (send event get-x)) (y (send event get-y)))
             (send (get-admin) popup-menu right-click-menu x y))
           (super on-default-event event)))
     (define/augment (after-set-position)
       ;; Check if any text is selected.
       (let ((no-selection? (= (get-start-position)
                               (get-end-position))))
         ;; No? Then hide the caret.
         (hide-caret no-selection?)
         ;; Yes? Enable the copy button in right-click menu.
         (send copy-menu-item enable (not no-selection?)))))
   (auto-wrap auto-wrap?)))

(define page-canvas
  (new editor-canvas% (parent frame)
       (style '(no-focus auto-hscroll auto-vscroll))
       (editor page-text)
       (wheel-step wheel-step)))

(define search-panel
  (new horizontal-panel% (parent frame)
       (alignment '(left bottom))
       (stretchable-height #f)))

(define search-field
  (new text-field% (parent search-panel)
       (label "Search")
       (min-width 300)
       (stretchable-width #f)
       (callback
        (λ (item event)
          (when (eq? 'text-field (send event get-event-type))
            (find-in-page page-text))))))

(define case-sensitive-tickbox
  (new check-box% (parent search-panel)
       (label "Match Case")
       (callback
        (λ (item event)
          (when (eq? 'check-box (send event get-event-type))
            (find-in-page page-text))))))

;;;; General procedures
(define (quit)
  (custodian-shutdown-all (current-custodian))
  (queue-callback exit #t))

(define (about)
  (define dialog
    (new dialog% (parent frame)
         (label (string-append "About " project-name))
         (spacing 10)
         (border 10)))
  (define panel
    (new horizontal-panel% (parent dialog)
         (alignment '(center center))
         (spacing 10)))
  (new message% (parent panel)
       (label huge-icon))
  (new message% (parent panel)
       (label version-message))
  (new button% (parent dialog)
       (label "OK")
       (callback (λ _ (send dialog show #f))))
  (send dialog show #t))

(define (clear-page page)
  (send page erase))

(define (clear-selection page)
  (send page set-position 0 'same #f #f 'local))

(define (highlight-positions page positions len)
  ;; Hilite and scroll to the first position.
  (send page set-position-bias-scroll 'start
        (car positions) (+ (car positions) len) #f #t 'local))

(define (show-search-field)
  (send search-panel show #t)
  (send search-field focus)
  (send (send search-field get-editor) select-all))

(define (find-in-page page)
  (clear-selection page)
  (unless (send page in-edit-sequence?)
    (let ((search-string
           (send search-field get-value)))
      ;; Do nothing if the user clicks cancel.
      (if (non-empty-string? search-string)
          (let ((search-results (send page find-string-all search-string
                                      'forward 'start 'eof #t
                                      (send case-sensitive-tickbox get-value))))
            (if (pair? search-results)
                (begin
                  (send search-field set-field-background address-success-colour)
                  (highlight-positions page search-results
                                       (string-length search-string)))
                (send search-field set-field-background address-error-colour)))
          (send search-field set-field-background address-bg-colour)))))

;; Traverse through the bookmark list (see config.rkt)
;; and generate menu items out of them.
(define (populate-bookmarks menu items)
  (unless (or (not (pair? items)) (null? items))
    (let ((item (car items)))
      (cond
        ;; Item is not a list.
        ;; Just a label. (Disabled menu item.)
        ((not (list? item)) (send
                             (new menu-item% (parent menu)
                                  (label (~a item))
                                  (callback void))
                             enable #f))
        ;; Item is an empty list.
        ;; A separator.
        ((null? item) (new separator-menu-item% (parent menu)))
        ;; Item contains only one element.
        ;; A link with no label. Link itself becomes the label.
        ((null? (cdr item)) (new menu-item% (parent menu)
                                 (label (~a (car item)))
                                 (callback (λ _ (go-to (car item))))))
        ;; Item contains a label and a list.
        ;; A submenu to be parsed. (Same syntax.)
        ((list? (cadr item)) (populate-bookmarks
                              (new menu% (parent menu)
                                   (label (~a (car item))))
                              (cadr item)))
        ;; Item contains a label and a link.
        ;; A link with a label. (Duh!)
        (else (new menu-item% (parent menu)
                   (label (~a (car item)))
                   (callback (λ _ (go-to (cadr item))))))))
    (populate-bookmarks menu (cdr items))))

(define (bookmark-dialog menu)
  (define (add-bookmark label link type)
    (let ((bookmark
           ;; TODO: A smarter way to do this.
           (case type
             (("Separator") (if (non-empty-string? label)
                                label
                                (list)))
             (("Bookmark") (if (non-empty-string? label)
                               (list label link)
                               (list link)))
             (("Folder") (list label (list))))))
      (populate-bookmarks menu (list bookmark))
      (set! bookmarks (append bookmarks (list bookmark)))))

  (define dialog
    (new dialog% (parent frame)
         (label "Bookmark Page")
         (border 5)
         (spacing 5)))

  (define (no-blank-bookmark)
    (send ok-button enable
         (not (and (zero? (send choice get-selection))
                   (zero? (string-length
                           (send link-field get-value)))))))

  (define panel
    (new horizontal-panel% (parent dialog)))

  (define choice
    (new radio-box% (parent panel)
        (label "")
        (choices '("Bookmark"
                   "Separator"
                   "Folder"))
        (callback
         (λ _ (send link-field enable
                    (zero? (send choice get-selection)))
            (no-blank-bookmark)))))

  (define field-panel
    (new vertical-panel% (parent panel)
         (alignment '(right center))
         (border 2)
         (spacing 3)))

  (define label-field
    (new text-field% (parent field-panel)
         (label "Label ")))
  (define link-field
    (new text-field% (parent field-panel)
         (label "Link ")
         (init-value address)
         (callback (λ (item event)
                     (when (eq? (send event get-event-type) 'text-field)
                       (no-blank-bookmark))))))

  (define button-panel
    (new horizontal-panel% (parent dialog)
         (alignment '(center center))))
  (new button% (parent button-panel)
       (label "Cancel")
       (callback (λ _ (send dialog show #f))))
  (define ok-button
    (new button% (parent button-panel)
        (label "OK")
        (callback (λ _
                    (add-bookmark
                     (send label-field get-value)
                     (send link-field get-value)
                     (send choice get-item-label
                           (send choice get-selection)))
                    (send dialog show #f)))))
  (when (system-position-ok-before-cancel?)
    (send button-panel change-children reverse))

  (send dialog show #t))

;;;; Menubar
(define (populate-menu-bar)
  (define menu-bar (new menu-bar% (parent frame)))

  (let ((file-menu (new menu% (parent menu-bar)
                        (label "&File"))))
    (new menu-item% (parent file-menu)
         (label "&Save Page")
         (help-string "Save page to device")
         (shortcut #\s)
         (callback (λ _ (save-page page-text))))
    (new separator-menu-item% (parent file-menu))
    (new menu-item% (parent file-menu)
         (label "&Quit")
         (help-string "Exit gophwr")
         (shortcut #\q)
         (callback (λ _ (quit)))))

  (when bookmarks
    (let ((bookmarks-menu (new menu% (parent menu-bar) (label "&Bookmarks"))))
      (new menu-item% (parent bookmarks-menu)
           (label "Add Bookmark")
           (help-string "Add this page to your bookmarks")
           (shortcut #\d)
           (callback (λ _ (bookmark-dialog bookmarks-menu))))
      (new menu-item% (parent bookmarks-menu)
           (label "Save Bookmarks")
           (help-string "Write your bookmarks to the config file")
           (callback (λ _
                       (save-preferences '(bookmarks) (list bookmarks))
                       (message-box
                        "Success!" "Bookmarks were saved to config." frame))))
      (new separator-menu-item% (parent bookmarks-menu))
      (populate-bookmarks bookmarks-menu bookmarks)))

  (let ((tools-menu (new menu% (parent menu-bar) (label "&Tools"))))
    (new menu-item% (parent tools-menu)
         (label "&Find in Page")
         (help-string "Find a string in this page")
         (callback (λ _ (show-search-field))))
    ;; Grey out TLS toggle button if TLS is not available in the system.
    (send (new checkable-menu-item% (parent tools-menu)
               (label "Enable TL&S")
               (help-string
                "Exclusively use TLS when connecting to gopherholes")
               (checked (tls-enabled?))
               (callback (λ (item event)
                           (tls-enabled? (send item is-checked?)))))
          enable ssl-available?))

  (let ((help-menu (new menu% (parent menu-bar) (label "&Help"))))
    (new menu-item% (parent help-menu)
         (label "gophwr Home")
         (help-string "Go to gophwr gopherhole (gophwrhole?)")
         (callback (λ _ (go-to project-home))))
    (new separator-menu-item% (parent help-menu))
    (new menu-item% (parent help-menu)
         (label "&About gophwr")
         (help-string "Show version and licence info")
         (callback (λ _ (about)))))

  (when (debug-mode?)
    (let ((debug-menu (new menu% (parent menu-bar) (label "&Debug"))))
      (new menu-item% (parent debug-menu)
           (label "Collect &Garbage")
           (shortcut #\g)
           (callback (λ _
                       (unless (send page-text in-edit-sequence?)
                         (collect-garbage)))))
      (new separator-menu-item% (parent debug-menu))
      (new menu-item% (parent debug-menu)
           (label "&Dump WXME")
           (callback (λ _
                       (unless (send page-text in-edit-sequence?)
                         (send page-text save-file "" 'standard)))))
      (new menu-item% (parent debug-menu)
           (label "&Load WXME")
           (callback (λ _
                       (unless (send page-text in-edit-sequence?)
                         (send page-text load-file "" 'standard))))))))


;;;; Right-click menu
(define right-click-menu (new popup-menu%))

(define copy-menu-item
  (new menu-item% (parent right-click-menu)
       (label "&Copy")
       (help-string "Copy selected text")
       (shortcut #\c)
       (callback
        (λ _ (send page-text copy)))))

(define (populate-right-click-menu)
  (send copy-menu-item enable #f)
  (new menu-item% (parent right-click-menu)
       (label "Select &All")
       (help-string "Select all text in the page")
       (shortcut #\a)
       (callback (λ _ (send page-text select-all))))
  (new separator-menu-item% (parent right-click-menu))
  (new menu-item% (parent right-click-menu)
       (label "&Save Page")
       (help-string "Save page to device")
       (shortcut #\s)
       (callback (λ _ (save-page page-text)))))


;;; GUI starts here.
(define (initialise-window)
  (error-display-handler
   (λ (str ex)
     (loaded)
     (send address-field set-field-background address-error-colour)
     (error-page str)))

  ;; Select means copy for X11.
  (editor-set-x-selection-mode #t)

  (application-quit-handler quit)
  (application-about-handler about)

  (send page-text set-max-undo-history 0)
  (send* page-canvas
    (set-canvas-background bg-colour)
    (force-display-focus #t)
    (lazy-refresh #t))

  (initialise-styles (send page-text get-style-list))
  (populate-right-click-menu)
  (populate-menu-bar)

  (send search-panel show #f)

  (send* frame
    (set-icon small-icon #f 'small)
    (set-icon large-icon #f 'large)
    (create-status-line)
    (show #t)))


;;;; Auxiliary procedures
;;; Make it abundantly clear that we're busy loading the page.
(define (loading urn)
  (send* frame
    (set-status-text
     (string-append "\u231b Loading " urn)) ; hourglass
    (modified #t))
  (send* address-field
    (set-value urn)
    (set-field-background address-bg-colour))
  (send page-text begin-edit-sequence #f)
  (send page-canvas enable #f)
  (begin-busy-cursor))

;;; Reset window state. Everything is fine now.
(define (loaded)
  (send frame set-status-text "")
  (send frame set-label
        (let ((label (string-append project-name " \u2014 " address))) ; em-dash
          ;; Truncate window label if it's longer than the hardcoded limit.
          (if (> (string-length label) 200)
              (string-append (substring label 0 199) "…")
              label)))
  (send frame modified #f)
  (send page-text set-position 0)
  (when (send page-text in-edit-sequence?)
    (send page-text end-edit-sequence))
  (send page-canvas enable #t)
  (end-busy-cursor))

;; Run stuff in a thread, sandwiched between preparation and
;; cleanup procedures. Return void.
(define (load-stuff urn stuff)
  (loading urn)
  (void (thread (λ () (stuff) (loaded)))))

(define (error-page . strs)
  (clear-page page-text)
  (loaded)
  (send frame set-status-text "\u26a0 Error!") ; warning sign
  (change-style page-text "Error")
  (send page-text insert (string-append* project-name " error: " strs)))

(define (save-page page)
  ;;; Note that this saves the formatted version of menus.
  (when-let (filename
             (put-file "Choose a download location"
                       frame
                       download-folder
                       (last (string-split address "/"))))
            (write-file filename
                        (send page get-text)
                        #:mode 'text)))


;;;; Navigation procedures
(define (refresh)
  ;; Keep history stacks intact when refreshing.
  (go-to address #:history #t))

(define (go-home)
  (go-to homepage))

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

(define (go-to destination-address #:history (history #f))
  (unless (send page-text in-edit-sequence?)
    (let* ((uri (string-downcase destination-address))
           (url
            (parse-urn
             ;; Strip out URL scheme from the address.
             (string-trim
              (if (and (> (string-length uri) 8)
                       (string=? (substring uri 0 9)
                                 "gopher://"))
                  (substring uri 9)
                  uri) #:repeat? #t))))
      ;; Do nothing if the address is blank.
      (when (non-empty-string? (gopher-urn url))
        ;; See: https://github.com/erkin/gophwr/wiki/Entity-types
        (case (gopher-type url)
          ;; + type isn't meant to be called directly, so we'll assume
          ;; it's a text file for convenience.
          (("0" "1" "7" "H" "M" "c" "e" "h" "m" "w" "x" "+")
           ;; Don't discard the next address stack if we're moving back
           ;; and forth.
           (unless history
             (set! next-address '())
             (send forward-key enable #f)
             (send back-key enable #t))
           (to-text url))
          ;; 4 and 6 are actually text but we're treating them like
          ;; binary. Watch out for a stray '.' at the end!
          (("4" "5" "6" "9" "P" "d" "s" ";" "<")
           (to-binary url))
          (("I" "g" "p" ":")
           (to-image url))
          (("2" "8" "T")
           (error-page "Session types not supported."))
          (else
           (error-page "Entity type not recognised: "
                       (make-string (gopher-type url)))))))))

(define (update-address urn)
  (unless (string=? address urn)
    ;; Refresh the global address value, in case URL scheme was
    ;; stripped out.
    (set! address urn)
    ;; Omit duplicate entries.
    (when (or (null? previous-address)
              (not (string=? address (car previous-address))))
      (set! previous-address (cons address previous-address)))))

(define (to-text url)
  ;; Parse redirections according to Sean Conner's suggestion.
  ;; See: <http://boston.conman.org/2019/09/30.4>
  (define (redirect-target lines)
    (and (> (length lines) 0)
         (with-handlers ([exn:fail? (lambda (exn) #f)])
           (match-let (((list type text path domain port)
                        (parse-selector (car lines))))
             (and (string=? type "3")
                  (string=? text "Permanent redirect")
                  (gopher (string-append domain ":" port "/1/" path)
                          domain
                          (string->number port)
                          "1"
                          path))))))
  (clear-page page-text)
  (update-address (gopher-urn url))
  (load-stuff
   (gopher-urn url)
   (λ ()
     (let resolve ((response (fetch-file url #:type 'text)))
       (if-let (target (redirect-target response))
               (begin
                 (update-address (gopher-urn target))
                 (resolve (fetch-file target #:type 'text)))
               ((if (member (gopher-type url) '("1" "7"))
                    render-menu
                    render-text)
                page-text
                response
                go-to))))))

(define (to-binary url)
  (loading (gopher-urn url))
  (load-stuff
   (gopher-urn url)
   (λ ()
     (when-let (filename (put-file "Choose a download location"
                                   frame
                                   download-folder
                                   (last (string-split (gopher-path url) "/"))))
               (write-file filename
                           (fetch-file url #:type 'binary)
                           #:mode 'binary)))))

(define (to-image url)
  (define (make-image-snip image-bytes type)
    (make-object image-snip%
                 (make-object bitmap%
                              (open-input-bytes image-bytes)
                              type bg-colour #t)))
  (clear-page page-text)
  (update-address (gopher-urn url))
  (load-stuff
   (gopher-urn url)
   (λ ()
     (send page-text insert
           (make-image-snip (fetch-file url #:type 'binary)
                            (case (gopher-type url)
                              (("I" ":") 'unknown/alpha)
                              (("g") 'gif/alpha)
                              (("p") 'png/alpha)))))))
