(
 ;; Homepage string. gopher:// URL scheme not necessary.
 ;; #f for blank page.
 (homepage "gopher.erkin.party:70/1/links")
 ;; Default download directory.
 ;; #f for the directory gophwr was run in.
 (download-folder #f)

 ;; Number of lines scrolled through with each wheel click.
 ;; #f disables scrolling. (Probably not what you want.)
 (wheel-step 3)
 (auto-wrap? #f)

 ;; Window dimensions in pixels.
 ;; #f for default.
 (initial-width 1024)
 (initial-height 768)

 ;; A string containing the name of the font
 ;; #f for default monospace.
 (font-name #f)
 (font-size 11)
 (title-size 23)


 ;; Colours are in RGB(A) format as a list of three bytes (or four, with alpha).
 ;; Besides bytes, strings of colour names are accepted as well.
 ;; To see a list of named colours, try:
 ;; > (require racket/draw) (for-each displayln (send the-color-database get-names))

 ;; Different representations of the same colour:
 ;; (#x40 #xD0 #xCC)
 ;; (#x40 #xD0 #xCC 1.0)
 ;; (64 208 204)
 ;; "turquoise"
 ;; (#o100 #o320 #o314)
 ;; (#b01000000 #b11010000 #b11001100)

 (fg-colour       (#xEE #xEE #xEE))
 (bg-colour       (#x11 #x11 #x11))

 (menu-colour     (#xAA #xAA #xEE))
 (link-colour     (#xAA #xEE #xAA))
 (error-colour    (#xEE #xAA #xAA))
 (document-colour (#xEE #xEE #xAA))
 (binary-colour   (#xAA #xEE #xEE))
 (image-colour    (#xEE #xAA #xEE))
 (clicked-colour  (#xAA #xAA #xAA))
)
