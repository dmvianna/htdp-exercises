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
;  (pre-render (make-editor "a" "b")))
; interpretation pre-render the text that would be
; added to the canvas before the fact, so we can
; measure its width or insert it into the canvas.
(define (pre-render ed)
  (beside
   (text (editor-pre ed) HEIGHT "black")
   CURSOR
   (text (editor-post ed) HEIGHT "black"))
  )

; Editor -> Image
;  (render (make-editor "a" "b")
; interpretation render draws an editor
; window on a canvas
(define (render ed)
  (place-image/align
   (pre-render ed)
   2 (/ HEIGHT 2) "left" "center" SCN))

;; (render (make-editor "this" " word"))
;; (render (make-editor "a" " b"))

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
     (make-editor (string-remove-last (editor-pre ed))
                  (editor-post ed))]
    [(string=? ke "\b") ed]
    [(> (image-width (pre-render ed)) (- WIDTH HEIGHT)) ed]
    [(and (string=? ke "left") (chars? ed))
     (make-editor (string-remove-last (editor-pre ed))
                  (string-append (string-last (editor-pre ed))
                                 (editor-post ed)))]
    [(and (string=? ke "right") (not (string=? "" (editor-post ed))))
     (make-editor (string-append (editor-pre ed)
                                 (string-first (editor-post ed)))
                  (string-rest (editor-post ed)))]
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

; String -> String
;  (string-first "a")
; interpretation returns the first
; 1String from a string
(define (string-first s)
  (if (< 0 (string-length s))
      (string-ith s 0)
      s))
(check-expect (string-first "abc") "a")
(check-expect (string-first "") "")

; String -> String
;  (string-rest "abc")
; interpretation returns the cdr
; of a string
(define (string-rest s)
  (cond
    [(< 1 (string-length s))
     (substring s 1 (string-length s))]
    [(= 1 (string-length s))
     ""]
    [else s]))
(check-expect (string-rest "abc") "bc")
(check-expect (string-rest "a") "")
(check-expect (string-rest "ab") "b")

; String -> String
;  (string-last "abc")
; interpretation returns the
; last 1String from a string
(define (string-last s)
  (if
   (<= 1 (string-length s))
   (string-ith s (sub1 (string-length s)))
   s))
(check-expect (string-last "abc") "c")
(check-expect (string-last "a") "a")
(check-expect (string-last "") "")

; String -> String
;  (string-remove-last "abc")
; interpretation returns the
; string without the last 1String
(define (string-remove-last s)
  (if
   (< 0 (string-length s))
   (substring s 0 (sub1 (string-length s)))
   s))
(check-expect (string-remove-last "abc") "ab")
(check-expect (string-remove-last "a") "")
(check-expect (string-remove-last "") "")

; Editor -> Editor
; runs the editor
(define (run ed)
  (big-bang ed
            [to-draw render]
            [on-key edit]))

(run (make-editor "" ""))
