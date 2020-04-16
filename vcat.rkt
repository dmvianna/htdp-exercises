#lang htdp/bsl
(require 2htdp/image)
(require 2htdp/universe)

; A VCat is a structure:
;  (make-vcat Number Number)
; interpretation (make-vcat x h) describes a cat
; which is at position pos
; and has happiness level hap
(define-struct vcat [position gauge])


; The scaling constant is an integer
(define SCALE 5)

; Canvas
; The canvas is a rectangle
(define HEIGHT-OF-WORLD (* 5 SCALE))
(define WIDTH-OF-WORLD (* 50 SCALE))
(define GROUND-LEVEL SCALE)
(define BACKGROUND (empty-scene WIDTH-OF-WORLD HEIGHT-OF-WORLD))

;; GAUGE

; Happy
; The world state is an integer that represents
; levels of happiness. The maximum score:
(define MAX-SCORE HEIGHT-OF-WORLD)
(define WIDTH-OF-GAUGE (+ (/ SCALE 10) SCALE))

; The gauge is represented by a rectangle
(define (gauge level) (rectangle SCALE level "solid" "black"))

; Happy -> Image
; Draws the gauge against the canvas

(define (render-gauge level bkg)
  (place-image (gauge level)
               (/ WIDTH-OF-GAUGE 2)
               (- HEIGHT-OF-WORLD
                  (/ (image-height (gauge level)) 2))
               bkg))
(check-expect (render-gauge 2 BACKGROUND)
              (place-image (gauge 2)
                           (/ WIDTH-OF-GAUGE 2)
                           (- HEIGHT-OF-WORLD
                              (/ (image-height (gauge 2)) 2))
                           BACKGROUND))

; the animal loses happiness -0.1 each tick
(define (leak-gauge happy)
  (max 0 (- happy 0.1)))

(define (interact-gauge happy ke)
  (cond
    [(string=? ke "down") (min MAX-SCORE (+ happy (/ happy 5)))]
    [(string=? ke "up") (min MAX-SCORE (+ happy (/ happy 3)))]
    [else happy]))

(define (sad-gauge happy)
  (= 0 happy))


;; CAT MOVEMENT

; The cat is represented by a brown circle
(define cat1 (circle SCALE "solid" "brown"))
(define cat2 (circle SCALE "solid" "black"))

; CatMove
; The world state is a struct with velocity and position
; represented as integers
(define-struct cat-move (position velocity))

; CatMove -> Image
; places the image of the cat x pixels from
; the left margin of the BACKGROUND image
; when the cat disappears to the right, it
; reappears on the left and vice-versa
(define (render-catmove x bkg)
  (place-image (if (odd? (cat-move-position x)) cat1 cat2)
               (cat-move-position x)
               (- HEIGHT-OF-WORLD
                  GROUND-LEVEL)
               bkg))
(check-expect (render-catmove (make-cat-move 20 2) BACKGROUND)
              (place-image cat1 20 (- HEIGHT-OF-WORLD GROUND-LEVEL) BACKGROUND))

; CatMove -> KeyPress -> CatMove
; stops when any key is pressed
(define (change-speed-catmove cat-move ke)
  (cond [(string=? ke "up") (make-cat-move (cat-move-position cat-move) (add1 (cat-move-velocity cat-move)))]
        [(string=? ke  "down") (make-cat-move (cat-move-position cat-move) (sub1 (cat-move-velocity cat-move)))]
        [true cat-move]))
(check-expect (change-speed (make-cat-move 1 1) "up") (make-cat-move 1 2))
(check-expect (change-speed (make-cat-move 1 1) "down") (make-cat-move 1 0))
(check-expect (change-speed (make-cat-move 1 1) "left") (make-cat-move 1 1))

; CatMove -> CatMove
; adds 3 to x to move the cat right
(define (tock cat-move)
  (make-cat-move (+ (cat-move-position cat-move) (cat-move-velocity cat-move))
        (cat-move-velocity cat-move)))
(check-expect (tock (make-cat-move 1 1)) (make-cat-move 2 1))
(check-expect (tock (make-cat-move 1 2)) (make-cat-move 3 2))

(define (droopy-catmove cat-move)
  (cond
    [(> (- (cat-move-position (tock cat-move)) SCALE) WIDTH-OF-WORLD) (make-cat-move 0 (cat-move-velocity cat-move))]
    [(< (+ (cat-move-position (tock cat-move)) SCALE) 0) (make-cat-move WIDTH-OF-WORLD (cat-move-velocity cat-move))]
    [else (tock cat-move)]))

;; JOIN BEHAVIOR

; VCat -> Image
; places the image of the cat x pixels from
; the left margin of the BACKGROUND image
; when the cat disappears to the right, it
; reappears on the left and vice-versa;
; a happiness gauge is visible on the left.
(define (render x)
  (render-catmove (vcat-position x)
                  (render-gauge (vcat-gauge x) BACKGROUND)))

(define (droopy x)
  (make-vcat (droopy-catmove (vcat-position x))
             (leak-gauge (vcat-gauge x))))

(define (interact x ke)
  (make-vcat (change-speed-catmove (vcat-position x) ke)
             (interact-gauge (vcat-gauge x) ke)))

(define (sad x)
  (sad-gauge (vcat-gauge x)))

; CatMove -> CatMove
; launches the program from some initial state
(define (main cat-move)
  (big-bang cat-move
            [on-tick droopy]
            [to-draw render]
            [on-key interact]
            [stop-when sad]
            [close-on-stop #true]
            ))

(main (make-vcat (make-cat-move 0 1) 10))


