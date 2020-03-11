#lang htdp/bsl
(require 2htdp/image)
(require 2htdp/universe)

(define-struct editor [pre post])
; An Editor is a structure:
;  (make-editor String String)
; interpretation (make-editor s t) describes an editor
; whose visible text is (string-append s t) with
; the cursor displayed between s and t

(define HEIGHT 20) ; distance in pixels
(define WIDTH 200)
(define SCN (empty-scene WIDTH HEIGHT))
(define CURSOR (rectangle 1  HEIGHT "solid" "red"))

; Editor -> Image
;  (render (make-editor "a" "b")
; interpretation render draws an editor
; window on a canvas
(define (render ed)
  (place-image/align
   (beside
    (text (editor-pre ed) 20 "black")
    CURSOR
    (text (editor-post ed) 20 "black"))
   2 (/ HEIGHT 2) "left" "center" SCN))

(render (make-editor "this" " word"))
(render (make-editor "a" " b"))
