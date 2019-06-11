(
 (homepage "gopher.erkin.party:70/1/links")
 (download-folder "/home/erkin/tmp")

 (scrolls-per-page 500)
 (wheel-step 1)
 (auto-wrap? #f)

 (initial-width 1024)
 (initial-height 768)

 ;; A string containing the name of the font
 ;; #f for default monospace.
 (font-name #f)
 (font-size 11)
 (title-size 23)

 ;; Besides hexcodes, strings of colour names are accepted as well.
 ;; To see a list of colours, try:
 ;; (require racket/draw) (for-each displayln (send the-color-database get-names))

 (fg-colour (#xEE #xEE #xEE))
 (bg-colour (#x11 #x11 #x11))
 (menu-colour (#xAA #xAA #xEE))
 (link-colour (#xAA #xEE #xAA))
 (error-colour (#xEE #xAA #xAA))
 (document-colour (#xEE #xEE #xAA))
 (download-colour (#xEE #xAA #xEE))
 (clicked-colour (#xAA #xEE #xEE))
)
