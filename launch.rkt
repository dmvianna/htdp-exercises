#lang htdp/bsl
(require 2htdp/image)
(require 2htdp/universe)

; An LR (short for launching rocket) is one of:
; - "resting"
; - NonnegativeNumber
; interpretation "resting represents a grounded rocket
; a number denotes the height of a rocket in flight

(define HEIGHT 300) ; distance in pixels
(define WIDTH 100)
(define YDELTA 3)

(define BACKG (empty-scene WIDTH HEIGHT))
(define ROCKET (rectangle 5 30 "solid" "red"))
(define CENTER (/ (image-height ROCKET) 2))

; An LRCD (for launching rocket countdown) is one of:
; - "resting"
; - a Number between -3 and -1
; - a NonnegativeNumber
; interpretation a grounded rocket, in countdown mode,
; a number denotes the number of pixels between the
; top of the canvas and the rocket (its height)

; A RocketPosition is a natural number that indicates
; the position of the bottom of the rocket in the
; background

; RocketPosition -> Image
(define (place-rocket x)
  (place-image ROCKET 10 (- x CENTER) BACKG))

; LRCD -> Image
; renders the state as a resting or flying rocket
(define (show x)
  (cond
    [(string? x)
     (place-rocket HEIGHT)]
    [(<= -3 x -1)
     (place-image (text (number->string x) 20 "red")
                  10 (* 3/4 WIDTH)
                  (place-rocket HEIGHT))]
    [(>= x 0)
     (place-rocket (- HEIGHT x))]))

(check-expect
 (show "resting")
 (place-image ROCKET 10 (- HEIGHT CENTER) BACKG))
(check-expect
 (show -2)
 (place-image (text "-2" 20 "red")
              10 (* 3/4 WIDTH)
              (place-image ROCKET 10 (- HEIGHT CENTER) BACKG)))
(check-expect
 (show 53)
 (place-image ROCKET 10 (- 53 CENTER) BACKG))
(check-expect
 (show HEIGHT)
 (place-image ROCKET 10 (- HEIGHT CENTER) BACKG))
(check-expect
 (show 0)
 (place-image ROCKET 10 (- 0 CENTER) BACKG))

; LRCD KeyEvent -> LRCD
; starts the countdown when space bar is pressed,
; if the rocket is still resting
(define (launch x ke)
  (cond
    [(string? x) (if (string=? " " ke) -3 x)]
    [(<= -3 x -1) x]
    [(>= x 0) x]))
(check-expect (launch "resting" " ") -3)
(check-expect (launch "resting" "a") "resting")
(check-expect (launch -3 " ") -3)
(check-expect (launch -1 " ") -1)
(check-expect (launch 33 " ") 33)
(check-expect (launch 33 "a") 33)

; LRCD -> LRCD
; raises the rocket by YDELTA,
; if it is moving already

(check-expect (fly "resting") "resting")
(check-expect (fly -3) -2)
(check-expect (fly -2) -1)
(check-expect (fly -1) 0)
(check-expect (fly 10) (+ 10 YDELTA))
(check-expect (fly 22) (+ 22 YDELTA))

(define (fly x)
  (cond
    [(string? x) x]
    [(<= -3 x -1) (if (= x -1) 0 (+ x 1))]
    [(>= x 0) (+ x YDELTA)]))

; LRCD -> LRCD
(define (main1 s)
  (big-bang s
            [to-draw show]
            [on-key launch]))
; (main1 "resting")

; LRCD -> Boolean
; stops the animation when state
; is equal to YDELTA
(define (finished s)
  (and (number? s) (= s HEIGHT)))

; LRCD -> LRCD
(define (main2 s)
  (big-bang s
            [to-draw show]
            [on-key launch]
            [on-tick fly 1/25]
            [stop-when finished show]))
(main2 "resting")

