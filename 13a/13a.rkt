;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname 13a) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; Exercise 1
; ----------

; A Payment is one of:
; - Number in [5, 10, 20]
; - 'cc
; Interpretation: the method someone is using to pay for something. Either a $5, $10, or $20 bill,
; or a credit card.
; Examples:
(define P5 5)
(define P10 10)
(define P20 20)
(define PCC 'cc)
; Template:
#;(define (payment-temp p)
    (cond [(symbol? p) ... p ...]
          [(= p 20) ... p ...]
          [(= p 10) ... p ...]
          [(= p 5) ... p ...]))

(define-struct customer [payment])
; A Customer is a (make-customer Payment)
; Interpretation: a customer at the movie theater
; - the first field is their payment method
; Examples:
(define C5 (make-customer P5))
(define C10 (make-customer P10))
(define C20 (make-customer P20))
(define CCC (make-customer PCC))
; Template:
(define (customer-temp c)
  (... (payment-temp (customer-payment c)) ...))

(define-struct register [fives tens twenties])
; A Register is a (make-register Natural Natural Natural)
; Interpretation:
; - the first field is the number of $5 bills in the register
; - the second field is the number of $10 bills in the register
; - the third field is the number of $20 bills in the register
; Examples:
(define R1 (make-register 0 0 0))
(define R2 (make-register 4 1 2))
(define R3 (make-register 3 5 7))
; Template:
(define (register-template r)
  (... (register-fives r) ...
       (register-tens r) ...
       (register-twenties r) ...))


; num-tickets-sold : Natural [List-of Customer] -> Natural
; Given n $5 bills, how many of the customers in loc can change be made for?
(define (num-tickets-sold n loc0)
  (local [; num-tickets-sold/acc : Register [List-of Customer] Natural -> Natural
          ; Iterates over the list of customers and adds up how many can have change made for them
          (define (num-tickets-sold/acc r loc sold)
            (local [(define change (if (zero? (length loc)) #false (make-change r (first loc))))]
              (if (boolean? change)
                  sold
                  (num-tickets-sold/acc change (rest loc) (add1 sold)))))]
    (num-tickets-sold/acc (make-register n 0 0) loc0 0)))

(check-expect (num-tickets-sold 5 (list C5 C10 CCC C20)) 4)
(check-expect (num-tickets-sold 1 (list C5 C10 CCC C20)) 4)
(check-expect (num-tickets-sold 1 (list C10 C20 C5 CCC C5)) 1)
(check-expect (num-tickets-sold 0 (list C10 C20)) 0)
(check-expect (num-tickets-sold 2 (list C10 C5 C20 CCC C10 C20)) 5)

; make-change : Register Customer -> [Union Register Boolean]
; Attempts to make change for the given customer; returns the updated register if making change was
; possible, otherwise returns false
(define (make-change r c)
  (local [(define new-reg (cond [(symbol? (customer-payment c)) r]
                                [(= (customer-payment c) 20) (make-register
                                                              (sub1 (register-fives r))
                                                              (sub1 (register-tens r))
                                                              (add1 (register-twenties r)))]
                                [(= (customer-payment c) 10) (make-register
                                                              (sub1 (register-fives r))
                                                              (add1 (register-tens r))
                                                              (register-twenties r))]
                                [(= (customer-payment c) 5) (make-register
                                                             (add1 (register-fives r))
                                                             (register-tens r)
                                                             (register-twenties r))]))]
    (if (and (>= (register-fives new-reg) 0) (>= (register-tens new-reg) 0)) new-reg #false)))
        
(check-expect (make-change R2 C20) (make-register 3 0 3))
(check-expect (make-change R3 C10) (make-register 2 6 7))
(check-expect (make-change R1 C5) (make-register 1 0 0))
(check-expect (make-change R1 C10) #false)


; Exercise 2
; ----------

; nth-smallest : Natural [List-of Real] -> Real
; Gets the nth smallest number in lor
#;(define (nth-smallest n lor)
    (cond [(= (length lor) 1) (first lor)]))

;(check-expect (nth-smallest 3 '(-3 4 2 0 23 18)) 4)
;(check-expect (nth-smallest 2 '(-23 20 12 -4 88 7)) 20)

; partition-to-fives : [List-of X] -> [List-of [List-of X]]
; Breaks lox into lists of length 5 (plus a shorter list at the end if necessary)
(define (partition-to-fives lox0)
  (local [(define (partition-to-fives/acc lox clox lolox)
            (cond [(empty? lox)
                   (append lolox (list clox))]
                  [(= (length clox) 5)
                   (partition-to-fives/acc (rest lox) (list (first lox)) (append lolox (list clox)))]
                  [else
                   (partition-to-fives/acc (rest lox) (append clox (list (first lox))) lolox)]))]
    (partition-to-fives/acc lox0 '() '())))

(check-expect (partition-to-fives '(3 1 53 20 -4 64 -22 0 12 -12 8 -90))
              '((3 1 53 20 -4) (64 -22 0 12 -12) (8 -90)))
(check-expect (partition-to-fives '(187 23)) '((187 23)))

; Exercise 3
; ----------

; A Circle is a [CircleMessage -> Any]
 
; A CircleMessage is one of:
; - 'center
; - 'radius
; - 'resize
; - 'equal
; and represents a message to circle, requesting either:
; its center (a Posn)
; its radius (a Number)
; how much to addtively change its radius by (a [Number -> Circle])
; whether or not it has the same size and position as another circle (a [Circle -> Boolean])


; new-circle : Posn Number -> Circle
; Generates a new circle that can correctly respond to CircleMessages
(define (new-circle posn n)
  (λ (msg) (cond [(symbol=? msg 'center) posn]
                 [(symbol=? msg 'radius) n]
                 [(symbol=? msg 'resize) (λ (add-to-rad) (new-circle posn (+ n add-to-rad)))]
                 [(symbol=? msg 'equal) (λ (c) (and (= (c 'radius) n) (posn=? (c 'center) posn)))])))

(define c0 (new-circle (make-posn 10 20) 4))
(define c1 (new-circle (make-posn 10 20) 9))
(check-expect (c0 'radius) 4)
(check-expect (c0 'center) (make-posn 10 20))
(check-expect (((c0 'resize) 10) 'radius) 14)
(check-expect ((c1 'equal) c0) #f)
(check-expect ((((c1 'resize) -5) 'equal) c0) #t)

; posn=? : Posn Posn -> Boolean
; Checks if the given Posns are the same
(define (posn=? p1 p2)
  (and (= (posn-x p1) (posn-x p2))
       (= (posn-y p1) (posn-y p2))))

(check-expect (posn=? (make-posn 1 1) (make-posn 1 1)) #true)
(check-expect (posn=? (make-posn 1 2) (make-posn 2 1)) #false)