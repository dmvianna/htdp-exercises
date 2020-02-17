#lang htdp/bsl
(require 2htdp/image)
(require 2htdp/universe)

(define HEIGHT-OF-WORLD 100)
(define WIDTH-OF-WORLD 200)

(define WHEEL-RADIUS 5)
(define WHEEL-DISTANCE (* WHEEL-RADIUS 5))
(define WHEEL
  (circle WHEEL-RADIUS "solid" "black"))
(define SPACE
  (rectangle (* 2 WHEEL-RADIUS) 0 "solid" "white"))
(define BOTH-WHEELS (beside WHEEL SPACE WHEEL))
(define CAR-CHASSIS (underlay/offset
             (rectangle (* 8 WHEEL-RADIUS) (* 2 WHEEL-RADIUS) "solid" "red")
             0
             WHEEL-RADIUS
             BOTH-WHEELS
             ))
(define CAR (underlay/offset
             (rectangle (* 4 WHEEL-RADIUS) (* 2 WHEEL-RADIUS) "solid" "red")
             0
             (* 2 WHEEL-RADIUS)
             CAR-CHASSIS))
(define GROUND-LEVEL (/ (image-height CAR) 2))
(define BACKGROUND (empty-scene WIDTH-OF-WORLD HEIGHT-OF-WORLD))

; A WorldState is a Number.
; interretation the number of pixels between
; the left border of the scene and the car

; WorldState -> Image
; places the image of the car x pixels from
; the left margin of the BACKGROUND image
(define (render x)
  (place-image CAR
               x
               (- HEIGHT-OF-WORLD
                  GROUND-LEVEL)
               BACKGROUND))

; WorldState -> WorldState
; adds 3 to x to move the car right
(define (tock x)
  (add1 x))

; WorldState -> KeyPress -> WorldState
; stops when any key is pressed
(define (stop ws ke) WIDTH-OF-WORLD)

; WorldState -> WorldState
; checks if the world should end
(define (end ws) (= ws (- WIDTH-OF-WORLD (/ (image-width CAR) 2))))

; WorldState -> WorldState
; launches the program from some initial state
(define (main ws)
  (big-bang ws
            [on-tick tock]
            [to-draw render]
            [on-key stop]
            [stop-when end]))

(main 0)