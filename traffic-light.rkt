#lang htdp/bsl
(require 2htdp/image)
(require 2htdp/universe)

(define DIAMETER 10)
(define MTSCN (empty-scene 90 30 "black"))

; An N-TrafficLight is one of:
; - 0 interpretation the traffic light shows red
; - 1 interpretation the traffic light shows green
; - 2 interpretation the traffic light shows yellow

; TrafficLight -> Image
; for each colour it will render a three light
; traffic light with the corresponding colour
; in solid, and the others in outline
(define (tl-render cs)
  (place-image
   (beside (circle DIAMETER (if (string=? "red" cs) "solid" "outline") "red")
           (square DIAMETER "solid" "black")
           (circle DIAMETER (if (string=? "yellow" cs) "solid" "outline") "yellow")
           (square DIAMETER "solid" "black")
           (circle DIAMETER (if (string=? "green" cs) "solid" "outline") "green"))
   45 15 MTSCN))
(check-expect
 (tl-render "red")
 (place-image 
  (beside (circle DIAMETER "solid" "red")
          (square DIAMETER "solid" "black")
          (circle DIAMETER "outline" "yellow")
          (square DIAMETER "solid" "black")                       
          (circle DIAMETER "outline" "green"))
  45 15 MTSCN))


; TrafficLight -> TrafficLight
; yields the next state, given current state cs
; TrafficLight colour -> TrafficLight colour
; yields the next state given current state s
(check-expect (traffic-light-next "red") "green")
(check-expect (traffic-light-next "green") "yellow")
(check-expect (traffic-light-next "yellow") "red")
(define (tl-next s)
  (cond
    [(string=? "red" s) "green"]
    [(string=? "yellow" s) "red"]
    [(string=? "green" s) "yellow"])
  )


;; (define (tl-next cs)
;;   (cond
;;     [(= 0 (remainder cs 3)) "red"]
;;     [(= 1 (remainder cs 3)) "yellow"]
;;     [(= 2 (remainder cs 3)) "green"]))
;; (check-expect (tl-next 0) "red")
;; (check-expect (tl-next 1) "yellow")
;; (check-expect (tl-next 2) "green")
;; (check-expect (tl-next 3) "red")

; TrafficLight -> TrafficLight
; simulates a clock-based American traffic light
(define (traffic-light-simulation initial-state)
  (big-bang initial-state
            [to-draw tl-render]
            [on-tick tl-next 1]))
(traffic-light-simulation "red")
