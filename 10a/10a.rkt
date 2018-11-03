;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname 10a) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; insert : [List-of X] X Natural -> [List-of X]
; Inserts the given element in the given list at the given index
; Assumes the index is valid, starting at 0
(define (insert lox x n)
  (cond [(and (empty? lox) (= n 0)) (list x)]
        [(and (cons? lox) (= n 0)) (cons x lox)]
        [(and (cons? lox) (not (zero? n))) (cons (first lox) (insert (rest lox) x (sub1 n)))]
        [else (error "Invalid index")]))

(check-expect (insert '(1 2 3 4) 2 2) '(1 2 2 3 4))
(check-expect (insert '(1 2 3 4) 2 0) '(2 1 2 3 4))


; permutations : [List-of X] -> [List-of [List-of X]]
; Returns a list of all possible permutations of the given list
(define (permutations lox)
  (cond [(empty? lox) '()]
        [(= (length lox) 1) (list lox)]
        [(cons? lox) (apply append (map (λ (l) (build-list (add1 (length l))
                                                           (λ (n) (insert l (first lox) n))))
                                        (permutations (rest lox))))]))

(check-expect (permutations '()) '())
(check-expect (permutations '(1)) '((1)))
(check-expect (permutations '(1 2)) '((1 2) (2 1)))
(check-expect (permutations '(1 2 3)) (list
                                       (list 1 2 3)
                                       (list 2 1 3)
                                       (list 2 3 1)
                                       (list 1 3 2)
                                       (list 3 1 2)
                                       (list 3 2 1)))
(check-expect (length (permutations '(1 2 3 4))) 24)


(define-struct pair [fst snd])
; a [Pair-of A B] is a (make-pair A B)
 
; A Type is one of:
; - 'number
; - 'boolean
; - 'string
(define-struct pair-ty [fst snd])
; - (make-pair-ty Type Type)
(define-struct fun-ty [arg ret])
; - (make-fun-ty Type Type)
(define-struct list-ty [itm])
; - (make-list-ty Type)
 
; Interpretation: a Type represents different types of data we use in our programs.
; In particular, these are some of the types we write in our signatures.
 
(define (type-temp type)
  (cond [(equal? type 'number) ...]
        [(equal? type 'boolean) ...]
        [(equal? type 'string) ...]
        [(pair-ty? type) (... (type-temp (pair-ty-fst type)) ...
                              (type-temp (pair-ty-snd type)) ...)]
        [(fun-ty? type) (... (type-temp (fun-ty-arg type)) ...
                             (type-temp (fun-ty-ret type)) ...)]
        [(list-ty? type) (... (type-temp (list-ty-itm type)) ...)]))
 
(define Number 'number)
(define Boolean 'boolean)
(define String 'string)
(define (Pair-of A B) (make-pair-ty A B))
(define (Function X Y) (make-fun-ty X Y))
(define (List X) (make-list-ty X))
 
 
; check : Type X -> X
; ensures the argument x behaves like the type,
; erroring otherwise (either immediately or when used)
(define (check type x)
  (local ((define (err _) (error "the type didn't match: "
                                 x " : " type)))
    (cond [(equal? type 'number) (if (number? x) x (err 1))]
          [(equal? type 'boolean) (if (boolean? x) x (err 1))]
          [(equal? type 'string) (if (string? x) x (err 1))]
          [(pair-ty? type) (if (pair? x)
                               (make-pair
                                (check (pair-ty-fst type) (pair-fst x))
                                (check (pair-ty-snd type) (pair-snd x)))
                               (err 1))]
 
          [(fun-ty? type)
           (if (procedure? x)
               (lambda (y)
                 (local ((define _ (check (fun-ty-arg type) y)))
                   (check (fun-ty-ret type) (x y))))
               (err 1))]
          
          [(list-ty? type) (if (list? x)
                               (cons (check (list-ty-itm type) (first x))
                                     (cond [(empty? (rest x)) '()]
                                           [(cons? (rest x)) (check type (rest x))]))
                               (err 1))])))
 
(check-expect (check Number 1) 1)
(check-error (check Number "hi"))
(check-expect (check Boolean #true) #true)
(check-expect (check String "hi") "hi")
(check-error (check String 34))
(check-expect (check (Pair-of Number Number) (make-pair 1 2)) (make-pair 1 2))
(check-error (check (Pair-of Number String) 1))
(check-expect ((check (Function Number Number) (lambda (x) x)) 1) 1)
(check-error ((check (Function Number Number) (lambda (x) x)) "hi"))
(check-error ((check (Function Number String) (lambda (x) x)) 1))
;(check-expect (check (List Number) (list 1 2 3)) (list 1 2 3))
(check-expect (check (List String) '("abc" "def" "ghi")) '("abc" "def" "ghi"))
(check-expect ((first (check
                       (List (Function Number String))
                       (list
                        number->string
                        (λ (n) (string-append "b" (number->string n)))))) 4) (number->string 4))
;(check-error (check (List Number) (list 2 3 "abc")))
(check-error (check (List (Function Number String)) (list number->string string->number)))

; sum-list : [List-of Number] -> Number
; Adds up the numbers in the list
(define sum-list (check (List Number)
                        (λ (lon)
                          (cond
                            [(empty? lon) 0]
                            [(cons? lon) (+ (first lon) (sum-list (first lon)))]))))
 
; contains-frog? : [List-of String] -> Boolean
; Returns whether or not the list contains "frog"
(define contains-frog? (check (List String)
                              (λ (los)
                                (cond
                                  [(empty? los) "false"]
                                  [(cons? los) (or (string=? (first los) "frog")
                                                   (contains-frog? (first los)))]))))


; type->string : Type -> String
(define (type->string type)
  (cond [(equal? type 'number) "Number"]
        [(equal? type 'boolean) "Boolean"]
        [(equal? type 'string) "String"]
        [(pair-ty? type) (string-append "[Pair-of "
                                        (type->string (pair-ty-fst type))
                                        " "
                                        (type->string (pair-ty-snd type))
                                        "]")]
        [(fun-ty? type) (string-append  "["
                                        (type->string (fun-ty-arg type))
                                        " -> "
                                        (type->string (fun-ty-ret type))
                                        "]")]
        [(list-ty? type) (string-append "[List-of "
                                        (type->string (list-ty-itm type))
                                        "]")]))
(check-expect (type->string String)
              "String")
(check-expect (type->string (Pair-of Number Boolean))
              "[Pair-of Number Boolean]")
(check-expect (type->string (Function (Function Number Number) String))
              "[[Number -> Number] -> String]")
(check-expect (type->string (List (Function Number Number)))
              "[List [Number -> Number]]")