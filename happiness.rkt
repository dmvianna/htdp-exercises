#lang htdp/bsl
(require 2htdp/image)
(require 2htdp/universe)

; The scaling constant is an integer
(define SCALE 50)

; The gauge is represented by a rectangle
(define (gauge level) (rectangle SCALE level "solid" "black"))

; Canvas
; The canvas is a rectangle
(define HEIGHT-OF-WORLD (* 5 SCALE))
(define WIDTH-OF-WORLD (+ (/ SCALE 10) SCALE))
(define GROUND-LEVEL SCALE)
(define BACKGROUND (empty-scene WIDTH-OF-WORLD HEIGHT-OF-WORLD))

; WorldState
; The world state is an integer that represents
; levels of happiness. The maximum score:
(define MAX-SCORE HEIGHT-OF-WORLD)

; WorldState -> Image
; Draws the gauge against the canvas
;
(define (render level)
  (place-image (gauge level)
               (/ WIDTH-OF-WORLD 2)
               (- HEIGHT-OF-WORLD
                  (/ (image-height (gauge level)) 2))
               BACKGROUND))
(check-expect (render 2)
              (place-image (gauge 2) 20 (- HEIGHT-OF-WORLD GROUND-LEVEL) BACKGROUND))

; the animal loses happiness -0.1 each tick
(define (leak ws)
  (max 0 (- ws 0.1)))

(define (interact ws ke)
  (cond
    [(string=? ke "down") (min MAX-SCORE (+ ws (/ ws 5)))]
    [(string=? ke "up") (min MAX-SCORE (+ ws (/ ws 3)))]
    [else ws]))

(define (main ws)
  (big-bang ws
            [on-tick leak]
            [to-draw render]
            [on-key interact]
            ))

(main HEIGHT-OF-WORLD)
