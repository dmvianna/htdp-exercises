#lang htdp/bsl
(require 2htdp/image)
(require 2htdp/universe)

;; WorldState
;; (list Position Velocity)
(define WHEEL-RADIUS 10)
(define HEIGHT-OF-WORLD (* 5 WHEEL-RADIUS))
(define WIDTH-OF-WORLD (* 50 WHEEL-RADIUS))
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
               (car x)
               (- HEIGHT-OF-WORLD
                  GROUND-LEVEL)
               BACKGROUND))
(check-expect (render (list 20 2))
              (place-image CAR 20 (- HEIGHT-OF-WORLD GROUND-LEVEL) BACKGROUND))

; WorldState -> WorldState
; adds 3 to x to move the car right
(define (tock ws)
  (list (+ (car ws) (cadr ws))
        (cadr ws)))
(check-expect (tock (list 1 1)) (list 2 1))
(check-expect (tock (list 1 2)) (list 3 2))


; WorldState -> KeyPress -> WorldState
; stops when any key is pressed
(define (change-speed ws ke)
  (cond [(string=? ke "up") (list (car ws) (add1 (cadr ws)))]
        [(string=? ke  "down") (list (car ws) (sub1 (cadr ws)))]
        [true ws]))

; WorldState -> WorldState
; checks if the world should end
(define (end ws) (> (car ws) (- WIDTH-OF-WORLD (/ (image-width CAR) 2))))

; WorldState Number Number String -> WorldState
; places the car at x-mouse
; if the given me is "button-down"
; given: 21 10 20 "enter"
; wanted: 21
; given: 42 10 20 "button-down"
; wanted: 10
(define (hyper ws x-mouse y-mouse me)
  (cond
    [(string=? "button-down" me) (list x-mouse (cadr ws))]
    [else ws]))
(check-expect (hyper (list 20 1) 10 10 "button-down")
              (list 10 1))
(check-expect (hyper (list 20 1) 10 10 "enter")
              (list 20 1))

; WorldState -> WorldState
; launches the program from some initial state
(define (main ws)
  (big-bang ws
            [on-tick tock]
            [to-draw render]
            [on-key change-speed]
            [on-mouse hyper]
            [stop-when end]))

(main (list 0 1))

