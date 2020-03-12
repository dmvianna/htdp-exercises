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

; Editor KeyEvent -> Editor
;  (edit (make-editor "a" "b") "\b")
; interpretation KeyEvents can
; - Add a character to pre:
;  - all ascii characters (one letter events)
; - Delete a character from pre:
;  - \b (backspace)
; - move a character from post to pre:
;  - "left" arrow
; - move a character from pre to post:
;  - "right" arrow
; - ignore other key events
(define (edit ed ke)
  (cond
    [(and (string=? ke "\b") (chars? ed))
     (make-editor (substring (editor-pre ed)
                             0
                             (sub1 (string-length (editor-pre ed))))
                  (editor-post ed))]
    [(string=? ke "\b") ed]
    [(and (string=? ke "left") (chars? ed))
     (make-editor (substring (editor-pre ed)
                             0
                             (sub1 (string-length (editor-pre ed))))
                  (string-append (string-ith
                                  (editor-pre ed)
                                  (sub1 (string-length (editor-pre ed))))
                                 (editor-post ed)))]
    [(and (string=? ke "right") (not (string=? "" (editor-post ed))))
     (make-editor (string-append (editor-pre ed)
                                 (string-ith (editor-post ed) 0))
                  (substring (editor-post ed)
                             1
                             (if (= 1 (string-length (editor-post ed)))
                                 1
                                 (sub1 (string-length (editor-post ed))))))]
    [(and (not (string=? " " ke)) (string-whitespace? ke))
     ed]
    [(= 1 (string-length ke))
     (make-editor (string-append (editor-pre ed)
                                 ke)
                  (editor-post ed))]
    [else ed]
    ))
(check-expect (edit (make-editor "a" "b") "c")
              (make-editor "ac" "b"))
(check-expect (edit (make-editor "a" "b") " ")
              (make-editor "a " "b"))
(check-expect (edit (make-editor "a" "b") "left")
              (make-editor "" "ab"))
(check-expect (edit (make-editor "a" "b") "right")
              (make-editor "ab" ""))
(check-expect (edit (make-editor "" "ab") "left")
              (make-editor "" "ab"))
(check-expect (edit (make-editor "ab" "") "right")
              (make-editor "ab" ""))
(check-expect (edit (make-editor "ab" "") "left")
              (make-editor "a" "b"))
(check-expect (edit (make-editor "" "ab") "\b")
              (make-editor "" "ab"))
(check-expect (edit (make-editor "ab" "") "\b")
              (make-editor "a" ""))
(check-expect (edit (make-editor "a" "b") "\t")
              (make-editor "a" "b"))
(check-expect (edit (make-editor "a" "b") "\r")
              (make-editor "a" "b"))

; Editor -> Boolean
;  (chars? (make-editor "a" "b"))
; interpretation checks whether there
; are characters in editor-pre
(define (chars? ed)
  (not (string=? "" (editor-pre ed))))
(check-expect (chars? (make-editor "a" ""))
              #true)
(check-expect (chars? (make-editor "" "a"))
              #false)
