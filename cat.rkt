#lang htdp/bsl
(require 2htdp/image)
(require 2htdp/universe)

; The scaling constant is an integer
(define SCALE 5)

; The cat is represented by a brown circle
(define cat1 (circle SCALE "solid" "brown"))
(define cat2 (circle SCALE "solid" "black"))

; WorldState
; The world state is a struct with velocity and position
; represented as integers
(define-struct ws (position velocity))

; Canvas
; The canvas is a rectangle
(define HEIGHT-OF-WORLD (* 5 SCALE))
(define WIDTH-OF-WORLD (* 50 SCALE))
(define GROUND-LEVEL SCALE)
(define BACKGROUND (empty-scene WIDTH-OF-WORLD HEIGHT-OF-WORLD))

; WorldState -> Image
; places the image of the cat x pixels from
; the left margin of the BACKGROUND image
; when the cat disappears to the right, it
; reappears on the left and vice-versa
(define (render x)
  (place-image (if (odd? (ws-position x)) cat1 cat2)
               (ws-position x)
               (- HEIGHT-OF-WORLD
                  GROUND-LEVEL)
               BACKGROUND))
(check-expect (render (make-ws 20 2))
              (place-image cat1 20 (- HEIGHT-OF-WORLD GROUND-LEVEL) BACKGROUND))

; WorldState -> KeyPress -> WorldState
; stops when any key is pressed
(define (change-speed ws ke)
  (cond [(string=? ke "up") (make-ws (ws-position ws) (add1 (ws-velocity ws)))]
        [(string=? ke  "down") (make-ws (ws-position ws) (sub1 (ws-velocity ws)))]
        [true ws]))
(check-expect (change-speed (make-ws 1 1) "up") (make-ws 1 2))
(check-expect (change-speed (make-ws 1 1) "down") (make-ws 1 0))
(check-expect (change-speed (make-ws 1 1) "left") (make-ws 1 1))

; WorldState -> WorldState
; adds 3 to x to move the cat right
(define (tock ws)
  (make-ws (+ (ws-position ws) (ws-velocity ws))
        (ws-velocity ws)))
(check-expect (tock (make-ws 1 1)) (make-ws 2 1))
(check-expect (tock (make-ws 1 2)) (make-ws 3 2))

(define (droopy ws)
  (cond
    [(> (- (ws-position (tock ws)) SCALE) WIDTH-OF-WORLD) (make-ws 0 (ws-velocity ws))]
    [(< (+ (ws-position (tock ws)) SCALE) 0) (make-ws WIDTH-OF-WORLD (ws-velocity ws))]
    [else (tock ws)]))

; WorldState -> WorldState
; launches the program from some initial state
(define (main ws)
  (big-bang ws
            [on-tick droopy]
            [to-draw render]
            [on-key change-speed]
            ))

(main (make-ws 0 1))
