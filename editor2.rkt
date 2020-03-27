#lang htdp/bsl
(require 2htdp/image)
(require 2htdp/universe)

(define-struct editor [str ix])
; An Editor is a structure:
;  (make-editor String Number)
; interpretation (make-editor str ix) describes an editor
; whose visible text is str with the cursor displayed
; between (substring str 0 ix) and (substring str ix (string-length str))

(define HEIGHT 20) ; distance in pixels
(define WIDTH 200)
(define SCN (empty-scene WIDTH HEIGHT))
(define CURSOR (rectangle 1 HEIGHT "solid" "red"))

; Editor -> Image
;  (pre-render (edit (make-editor "ab" 1))
; interpretation pre-render the text that would be added
; to the canvas before the fact, so we can measure its
; width or insert it into the canvas
(define (pre-render ed)
  (beside
   (text (editor-pre ed) HEIGHT "black")
   CURSOR
   (text (editor-post ed) HEIGHT "black")))

; Editor -> String
;  (editor-pre ed)
; interpretation returns the string before the cursor
(define (editor-pre ed)
  (substring (editor-str ed) 0 (editor-ix ed)))
(check-expect (editor-pre (make-editor "ab" 1)) "a")
(check-expect (editor-pre (make-editor "" 0)) "")
(check-expect (editor-pre (make-editor "ab" 0)) "")
(check-expect (editor-pre (make-editor "ab" 2)) "ab")

; Editor -> String
;  (editor-post ed)
; intepretation returns the string after the cursor
(define (editor-post ed)
  (substring (editor-str ed) (editor-ix ed) (string-length (editor-str ed))))
(check-expect (editor-post (make-editor "ab" 0)) "ab")
(check-expect (editor-post (make-editor "ab" 1)) "b")
(check-expect (editor-post (make-editor "ab" 2)) "")

; Editor -> Image
;  (render (make-editor "a" "b")
; interpretation render draws an editor
; window on a canvas
(define (render ed)
  (place-image/align
   (pre-render ed)
   2 (/ HEIGHT 2) "left" "center" SCN))

(define (edit ed ke)
  (cond
    [(and (string=? ke "\b") (chars? ed))
     (make-editor (rm-1str (editor-str ed) (editor-ix ed))
                  (sub1 (editor-ix ed)))]
    [(string=? ke "\b") ed]
    [(> (image-width (pre-render ed)) (- WIDTH HEIGHT)) ed]
    [(and (string=? ke "left") (chars? ed))
     (make-editor (editor-str ed) (sub1 (editor-ix ed)))]
    [(and (string=? ke "right") (not (string=? "" (editor-post ed))))
     (make-editor (editor-str ed) (add1 (editor-ix ed)))]
    [(and (not (string=? " " ke)) (string-whitespace? ke))
     ed]
    [(= 1 (string-length ke))
     (make-editor (add-1str (editor-str ed)
                            (editor-ix ed)
                            ke)
                  (add1 (editor-ix ed)))]
    [else ed]
    ))

(check-expect (edit (make-editor "ab" 1) "c")
              (make-editor "acb" 2))
(check-expect (edit (make-editor "ab" 1) " ")
              (make-editor "a b" 2))
(check-expect (edit (make-editor "ab" 1) "left")
              (make-editor "ab" 0))
(check-expect (edit (make-editor "ab" 1) "right")
              (make-editor "ab" 2))
(check-expect (edit (make-editor "ab" 0) "left")
              (make-editor "ab" 0))
(check-expect (edit (make-editor "ab" 2) "right")
              (make-editor "ab" 2))
(check-expect (edit (make-editor "ab" 2) "left")
              (make-editor "ab" 1))
(check-expect (edit (make-editor "ab" 0) "\b")
              (make-editor "ab" 0))
(check-expect (edit (make-editor "ab" 2) "\b")
              (make-editor "a" 1))
(check-expect (edit (make-editor "ab" 1) "\t")
              (make-editor "ab" 1))
(check-expect (edit (make-editor "ab" 1) "\r")
              (make-editor "ab" 1))

; Editor -> Boolean
;  (chars? (make-editor "a" "b"))
; interpretation checks whether there
; are characters in editor-pre
(define (chars? ed)
  (positive? (editor-ix ed)))
(check-expect (chars? (make-editor "a" 1))
              #true)
(check-expect (chars? (make-editor "a" 0))
              #false)

; String Number -> String
;  (rm-1str "abc" 1)
; interpretation returns a string with
; the string at indicated position removed
(define (rm-1str str pos)
  (if (= pos 0)
      str
      (string-append (substring str 0 (- pos 1))
                     (substring str pos (string-length str)))))
(check-expect (rm-1str "abc" 1) "bc")
(check-expect (rm-1str "abc" 0) "abc")
(check-expect (rm-1str "abc" 2) "ac")

; String Number 1String -> String
;  (add-1str "ab" 2 "c")
; interpretation adds a character into
; the string at indicated position
(define (add-1str str pos ch)
  (string-append (substring str 0 pos)
                 ch
                 (substring str pos (string-length str))))
(check-expect (add-1str "bc" 0 "a") "abc")
(check-expect (add-1str "ac" 1 "b") "abc")
(check-expect (add-1str "ab" 2 "c") "abc")


; Editor -> Editor
; runs the editor
(define (run ed)
  (big-bang ed
            [to-draw render]
            [on-key edit]))

(run (make-editor "" 0))
