;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname 5a) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp")) #f)))
;;;;;;;;;;;;;;;;;;
; Problem Set 5a ;
;;;;;;;;;;;;;;;;;;


; Exercise 2
; ----------

; Data definition wishlist:
; - world state
; - grid state
; - player state
; - item
; - tnt state

; Function wishlist:
; - on-tick (move player around, update grid state, add gold, update tnt state)
; - on-draw (draw scene)
; - on-key
;   - smash
;   - place
;   - move
;   - switch-materials
; - 