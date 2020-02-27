#lang htdp/bsl
(require 2htdp/image)
(require 2htdp/universe)

(define DIAMETER 10)
(define LENGTH 90)
(define WIDTH 30)
(define MTSCN (empty-scene LENGTH WIDTH "black"))
(define RED 0)
(define GREEN 1)
(define YELLOW 2)

; An N-TrafficLight is one of:
; - RED
; - GREEN
; - YELLOW


; TrafficLight -> Image
; for each colour it will render a three light
; traffic light with the corresponding colour
; in solid, and the others in outline
(define (tl-render cs)
  (place-image
   (beside (circle DIAMETER (if (equal? RED cs) "solid" "outline") "red")
           (square DIAMETER "solid" "black")
           (circle DIAMETER (if (equal? YELLOW cs) "solid" "outline") "yellow")
           (square DIAMETER "solid" "black")
           (circle DIAMETER (if (equal? GREEN cs) "solid" "outline") "green"))
   (/ LENGTH 2) (/ WIDTH 2) MTSCN))
(check-expect
 (tl-render RED)
 (place-image 
  (beside (circle DIAMETER "solid" "red")
          (square DIAMETER "solid" "black")
          (circle DIAMETER "outline" "yellow")
          (square DIAMETER "solid" "black")                       
          (circle DIAMETER "outline" "green"))
  (/ LENGTH 2)(/ WIDTH 2)  MTSCN))


; TrafficLight -> TrafficLight
; yields the next state, given current state cs
; TrafficLight colour -> TrafficLight colour
; yields the next state given current state s
(check-expect (tl-next RED) GREEN)
(check-expect (tl-next GREEN) YELLOW)
(check-expect (tl-next YELLOW) RED)
(define (tl-next cs)
  (cond
    [(equal? cs RED) GREEN]
    [(equal? cs GREEN) YELLOW]
    [(equal? cs YELLOW) RED]))


; TrafficLight -> TrafficLight
; simulates a clock-based American traffic light
(define (traffic-light-simulation initial-state)
  (big-bang initial-state
            [to-draw tl-render]
            [on-tick tl-next 1]))

(traffic-light-simulation 0)
