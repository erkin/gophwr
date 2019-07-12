(
 ;; Homepage string. gopher:// URL scheme not necessary.
 ;; #f for blank page.
 (homepage "gopher.erkin.party:70/1/links")
 ;; Default download directory.
 ;; #f for the directory gophwr was run in.
 (download-folder #f)

 ;; Window dimensions in pixels.
 ;; #f for default.
 (initial-width 1024)
 (initial-height 768)

 ;; Number of lines scrolled through with each wheel click.
 ;; #f disables scrolling. (Probably not what you want.)
 (wheel-step 3)

 ;; Wrap around text when it hits the eastern border.
 (auto-wrap? #f)

 ;; A string containing the name of the font.
 ;; #f for default monospace.
 (font-name #f)
 (font-size 11)
 (title-size 19)


 ;; Bookmarks are in an S-expression format, as a list of lists.
 ;; The format is (label link), any third item is ignored.
 ;; Note: This isn't an association list: No '(foo . bar) format.

 ;; A link with a separate label.
 ;; ("Friend's phlog" "example.org/1/my-phlog")

 ;; The link also serves as a label here.
 ;; ("example.org")

 ;; A separator.
 ;; ()

 ;; A dummy button (greyed-out), can be used for categorisation.
 ;; "Favourite phlogs"

 ;; A submenu with two items.
 ;; ("Sandwich recipes"
 ;;  (("Avocado" "example.org/0/sandwich.txt")
 ;;   ("Tomato" "example.org/0/recipes/001.txt")))

 ;; Set the entire bookmarks value to #f to disable the bookmarks menu outright.

 (bookmarks (("Veronica-2" "floodgap.com/1/v2")
             ("Bitreich" "bitreich.org")))


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

 (address-bg-colour "white")
 (address-error-colour "peachpuff")
)
