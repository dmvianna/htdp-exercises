#lang htdp/bsl
(require 2htdp/image)
(require 2htdp/universe)


; A DoorState is one of:
(define LOCKED "locked") ; - LOCKED
(define CLOSED "closed") ; - CLOSED
(define OPEN "open")     ; - OPEN

; An Action is one of:
(define UNLOCK "u") ; - UNLOCK
(define LOCK "l")   ; - LOCK
(define PUSH " ")   ; - PUSH the door open

; Time is a clock tick

; DoorState -> DoorState
; closes an open door over the period of one tick
(define (door-closer state-of-door)
  (cond
    [(string=? LOCKED state-of-door) LOCKED]
    [(string=? CLOSED state-of-door) CLOSED]
    [(string=? OPEN state-of-door) CLOSED]))
(check-expect (door-closer LOCKED) LOCKED)
(check-expect (door-closer CLOSED) CLOSED)
(check-expect (door-closer OPEN) CLOSED)


; DoorState -> Action -> DoorState
(define (door-action s k)
  (cond
    [(and (string=? LOCKED s) (string=? "u" k)) CLOSED]
    [(and (string=? CLOSED s) (string=? "l" k)) LOCKED]
    [(and (string=? CLOSED s) (string=? " " k)) OPEN]
    [else s]))
(check-expect (door-action LOCKED "u") CLOSED)
(check-expect (door-action CLOSED "l") LOCKED)
(check-expect (door-action CLOSED " ") OPEN)
(check-expect (door-action OPEN "a") OPEN)
(check-expect (door-action CLOSED "a") CLOSED)

; DoorState -> Image
; translates the state s into a large text image
(check-expect (door-render CLOSED)
              (text CLOSED 40 "red"))
(define (door-render s)
  (text s 40 "red"))

; DoorState -> DoorState
; simulates a door with an automatic door closer
(define (door-simulation initial-state)
  (big-bang initial-state
            [on-tick door-closer 3]
            [on-key door-action]
            [to-draw door-render]))

(door-simulation LOCKED)
