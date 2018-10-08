;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname 5b) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; An LoN (List of Numbers) is one of:
; - '()
; - (cons Number LoN)

; We're not including examples/templates under the assumption that this has been used so
; many times in/out of class that we no longer need to, similarly to Posn.

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
     (append (list (first lon1) (first lon2))
             (interleave (rest lon1) (rest lon2)))]))

(check-expect (interleave '() '()) '())
(check-expect (interleave '() '(1 2 3)) '(1 2 3))
(check-expect (interleave '(4 5 6) '()) '(4 5 6))
(check-expect (interleave '(1 3 5) '(2 4 6)) '(1 2 3 4 5 6))

; Exercise 3.

; An LoLoN (List of LoN) is one of:
; - '()
; - (cons LoN LoLoN)
; Examples:
(define lolon0 '())
(define lolon1 (list (list 1 2 3) (list 3 2 1) '()))
; Template:
#;
(define (lolon-temp l)
  (cond
    [(empty? l) ...]
    [(cons? l) (... (lon-temp (first l)) ...
                    (lolon-temp (rest l)) ...)]))

; powerlist : LoN -> LoLoN
; returns a list of all the possible sublists of the given LoN.
(define (powerlist l)
  (cond [(empty? l) (list '())]
        [(cons? l) (append (add-elements-to-lists (powerlist (rest l)) (first l))
                           (powerlist (rest l)))]))

(check-expect (powerlist '()) (list '()))
(check-expect (powerlist (list 1 2)) (list (list 1 2) (list 1) (list 2) '()))
(check-expect (length (powerlist (list 1 2 3 4))) 16)

; addelementtolists : LoLoN Number -> LoN
; returns the given lists of numbers with an element added to the head.
(define (add-elements-to-lists ll n)
  (cond [(empty? ll) '()]
        [(cons? ll) (cons (cons n (first ll)) (add-elements-to-lists (rest ll) n))]))

(check-expect (add-elements-to-lists (list (list 1 2 3) (list 45 46)) 100)
              (list (list 100 1 2 3) (list 100 45 46)))


; Exercise 4
; ----------

; An NELoLoN (Non Empty List of LoN) is one of:
; - (cons LoN '())
; - (cons LoN NELoLoN)
; Example:
(define nelolon1 (list (list 1 2 3) (list 3 2 1) '()))
; Template:
#;
(define (nelolon-temp nel)
  (cond
    [(empty? nel) ...]
    [(cons? nel) (... (lon-temp (first nel)) ...
                      (nelolon-temp (rest nel)) ...)]))

; intersection : LoLoN -> LoN
; returns the list of numbers that are in all sublists of the given list of list of numbers.
(define (intersection lln)
  (cond
    [(empty? (rest lln)) (first lln)]
    [(cons? lln) (two-list-intersection (first lln) (intersection (rest lln)))]))

(check-expect (intersection (list (list 1 2 3) (list 2 3 4))) (list 2 3))
(check-expect (intersection (list (list 1 2 3) (list 2 3 4) (list 3 4 5))) (list 3))
(check-expect (intersection (list '() (list 1 2 3))) '())
(check-expect (intersection (list (list 1 2 3) '())) '())

; two-list-intersection : LoN LoN -> LoN
; finds the common items between the two lists of numbers.
(define (two-list-intersection lon1 lon2)
  (cond
    [(empty? lon1) '()]
    [(cons? lon1)
     (cond
       [(member? (first lon1) lon2) (cons (first lon1) (two-list-intersection (rest lon1) lon2))]
       [else (two-list-intersection (rest lon1) lon2)])]))

(check-expect (two-list-intersection (list 1 2 3) (list 2 3 4)) (list 2 3))
(check-expect (two-list-intersection '() (list 1 2 3)) '())
(check-expect (two-list-intersection (list 1 2 3) '()) '())