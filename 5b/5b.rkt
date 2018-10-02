;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname 5b) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; An LoN (List of Numbers) is one of:
; - '()
; - (cons Number LoN)


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

(check-expect (interleave '() '()) '())
(check-expect (interleave '() '(1 2 3)) '(1 2 3))
(check-expect (interleave '(4 5 6) '()) '(4 5 6))
(check-expect (interleave '(1 3 5) '(2 4 6)) '(1 2 3 4 5 6))