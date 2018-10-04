;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname 5b) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; An LoN (List of Numbers) is one of:
; - '()
; - (cons Number LoN)

; Do we need to write templates here or are we abandoning that like we mentioned in class? :D

; Exercise 2.

; interleave : LoN LoN -> LoN
; Produces a list of all the items in both lists, alternating items from each list.
; If the lists are different lengths, the output list ends with the remaining items
; from the longer list.
(define (interleave lon1 lon2)
  (cond
    [(empty? lon1) lon2]
    [(empty? lon2) lon1]
    [(and (cons? lon1) (cons? lon2))
     `(,(first lon1)
       ,(first lon2)
       ,@(interleave (rest lon1) (rest lon2)))]))
; The above shortcuts are things that I don't understand and I don't know where they come from./
; The code I would have written is (append (list (first lon1) (first lon2))
;                                     (interleave (rest lon1) (rest lon2)))

(check-expect (interleave '() '()) '())
(check-expect (interleave '() '(1 2 3)) '(1 2 3))
(check-expect (interleave '(4 5 6) '()) '(4 5 6))
(check-expect (interleave '(1 3 5) '(2 4 6)) '(1 2 3 4 5 6))

; Exercise 3.

; An LoLoN (List of LoN) is one of:
; - '()
; - (cons LoN LoLoN)

; powerlist : LoN -> LoLoN
; returns a list of all the possible sublists of the given LoN.
(define (powerlist l)
  (cond [(empty? l) (list empty)]
        [(cons? l) (append (addelementstolists (powerlist (rest l)) (first l))
                           (powerlist (rest l)))]))

(check-expect (powerlist empty) (list empty))
(check-expect (powerlist (list 1 2)) (list (list 1 2) (list 1) (list 2) empty))
(check-expect (length (powerlist (list 1 2 3 4))) 16)

; addelementtolists : LoLoN Number -> LoN
; returns the given lists of numbers with an element added to the head.
(define (addelementstolists ll n)
  (cond [(empty? ll) empty]
        [(cons? ll) (cons (cons n (first ll)) (addelementstolists (rest ll) n))]))

(check-expect (addelementstolists (list (list 1 2 3)
                                        (list 45 46)) 100)
              (list (list 100 1 2 3) (list 100 45 46)))