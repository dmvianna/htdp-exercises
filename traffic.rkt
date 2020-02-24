#lang htdp/bsl
(require 2htdp/image)
(require 2htdp/universe)

(define DIAMETER 15)
(define WIDTH (+ (* DIAMETER 2) (/ DIAMETER 2)))
(define HEIGHT 100)
(define MTSCN (empty-scene WIDTH HEIGHT "black"))

; TrafficLight colour -> TrafficLight colour
; yields the next state given current state s
(check-expect (traffic-light-next "red") "green")
(check-expect (traffic-light-next "green") "yellow")
(check-expect (traffic-light-next "yellow") "red")
(define (traffic-light-next s)
  (cond
    [(string=? "red" s) "green"]
    [(string=? "yellow" s) "red"]
    [(string=? "green" s) "yellow"])
  )

; TrafficLight colour -> TrafficLight mode list
; Translate colour into traffic light state
(define (traffic-light-state s)
  (cond
    [(string=? "red" s) (list "solid" "outline" "outline")]
    [(string=? "yellow" s) (list "outline" "solid" "outline")]
    [(string=? "green" s) (list "outline" "outline" "solid")])
  )

; Mode -> Colour -> Bulb
(define (bulb m c) (circle DIAMETER m c))

; TrafficLight mode list -> TrafficLight image
; draws a traffic light
(define (traffic-light m)
  (above (bulb (first m) "red")
         (bulb (second m) "yellow")
         (bulb (third m) "green")))

; WorldState
; WorldState is the currently lit colour

; TrafficLight -> Image
; for each colour it will render a three light
; traffic light with the corresponding colour
; in solid, and the others in outline
(define (render ws)
  (place-image
   (traffic-light
    (traffic-light-state ws)) (/ WIDTH 2) (/ HEIGHT 2) MTSCN)
  )

; main

(define (main ws)
  (big-bang ws
            [on-tick traffic-light-next 2 10]
            [to-draw render]
            ))

(main "red")
