;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname 7b) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; EXERCISE 2
; ==========

(define p1 (make-posn 0 0))
(define p2 (make-posn 1 2))
(define p3 (make-posn -12 23))

(define l0 '())
(define l1 (list p1 p2))
(define l2 (list p1 p2 p3))

; y-coords-below : [List-of Posn] Number -> Boolean
; Checks if the y-coordinate of every posn in l is below num.
(define (y-coords-below l num)
  (andmap (λ (x) (< x num)) (map posn-y l)))

(check-expect (y-coords-below l0 2) #t)
(check-expect (y-coords-below l1 3) #t)
(check-expect (y-coords-below l2 3) #f)


; EXERCISE 3
; ==========
(define list1 (list "hello" "my" "name"))
(define list2 (list "Joe" "first" "is"))
(define list-hyphenated (list "hello-Joe" "my-first" "name-is"))

; hyphenate : [List-of String] [List-of String] -> [List-of String]
; Joins strings in each list in corresponding positions with a hyphen
(define (hyphenate l1 l2)
  (map (λ (fst snd) (string-append fst "-" snd)) l1 l2))

(check-expect (hyphenate l0 l0) l0)
(check-expect (hyphenate list1 list2) list-hyphenated)


; EXERCISE 4
; ==========
(define f1 identity)
(define f2 sqr)
(define f3 cos)
(define in1 0)
(define in2 1000)

; bigger-transformation : [Number -> Number] [Number -> Number] -> [Number -> Number]
; Returns whichever of op1 and op2 gives a larger output
(define (bigger-transformation op1 op2)
  (λ (n) (if (>= (op1 n) (op2 n))
             (op1 n)
             (op2 n))))

(check-expect ((bigger-transformation f1 f3) in1) (f3 in1))
(check-expect ((bigger-transformation f1 f3) in2) (f1 in2))
(check-expect ((bigger-transformation f1 f2) in1) (f1 in1))
(check-expect ((bigger-transformation f1 f2) in2) (f2 in2))