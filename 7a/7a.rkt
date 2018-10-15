;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname 7a) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp")) #f)))
; EXERCISE 2
; ==========

; A [Keyed X] is a (list Number X)
; and represents an X and its extracted "key" 

; A Movie is a (make-movie String Number)
(define-struct movie [title runtime])
; and represents a movie's title and runtime in minutes

; sort-by : [List-of X -> List-of X] [List-of X] -> [List-of X]
; Sorts a list using the given function to create a sortable numeric value for each list element
(define (sort-by fn l)
  (local [; insert-by : [Keyed X] [List-of [Keyed X]] -> [List-of [Keyed X]]
          ; Find the correct spot for the list item
          (define (insert-by kx lokx)
            (cond [(empty? lokx) (list kx)]
                  [(cons? lokx)
                   (if (<= (first kx) (first (first lokx)))
                       (cons kx lokx)
                       (cons (first lokx) (insert-by kx (rest lokx))))]))]
    (map second
         (foldr insert-by '()
                (map list (map fn l) l)))))

(check-expect (sort-by abs (list -3 -1 0 2 4)) (list 0 -1 2 -3 4))


; sort-by-title-length : [List-of Movie] -> [List-of Movie]
; Sort the movies by their title's length (ascending)
(define (sort-by-title-length lom)
  (sort-by (compose string-length movie-title) lom))

(check-expect (sort-by-title-length
               (list (make-movie "Sorry To Bother You" 111)
                     (make-movie "Hereditary" 127)
                     (make-movie "Annihilation" 120)
                     (make-movie "Blindspotting" 96)
                     (make-movie "You Were Never Really Here" 95)))
              (list
               (make-movie "Hereditary" 127)
               (make-movie "Annihilation" 120)
               (make-movie "Blindspotting" 96)
               (make-movie "Sorry To Bother You" 111)
               (make-movie "You Were Never Really Here" 95)))


; An [NEList-of X] (Non-Empty List of X) is one of:
; - (cons X '()))
; - (cons X [NEList-of X])

; sort-by-biggest : [List-of [NEList-of Number]] -> [List-of [NEList-of Number]]
; Sort the lists by their biggest element (ascending)
(define (sort-by-biggest lonelon)
  (local [; biggest : [NEList-of Number] -> Number
          ; Find the biggest number in the non-empty list of numbers
          (define (biggest nelon)
            (foldr max (first nelon) (rest nelon)))]
    (sort-by biggest lonelon)))

(check-expect (sort-by-biggest (list (list 6) (list 1 2 3) (list 5 6) (list 23)))
              (list (list 1 2 3) (list 6) (list 5 6) (list 23)))


; EXERCISE 3
; ==========

; copy-even-strings : [List-of String] -> [List-of String]
; Outputs the given list with two copies of all strings with an even string-length.
(define (copy-even-strings los)
  (local [; listify : String -> [List-of String]
          ; Turns a string into a list with two copies of itself if its length is even, and one copy
          ; if its length is odd
          (define (listify s)
            (if (even? (string-length s))
                (list s s)
                (list s)))]
    (foldr append '() (map listify los))))

(check-expect (copy-even-strings (list "hello" "my" "name")) (list "hello" "my" "my" "name" "name"))
(check-expect (copy-even-strings '()) '())


; EXERCISE 4
; ==========

; scalar-matrix : Natural Number -> [List-of [List-of Number]]
; Creates an n x n matrix with a diagonal with the given value k
(define (scalar-matrix n k)
  (local [; make-row : Natural -> [List-of Number]
          ; Creates a list of length n with k at element i
          (define (make-row i)
            (append (make-list i 0) (list k) (make-list (- n i 1) 0)))]
            
    (map make-row (build-list n identity))))

(check-expect (scalar-matrix 0 3) '())
(check-expect (scalar-matrix 1 3) (list (list 3)))
(check-expect (scalar-matrix 2 2) (list (list 2 0) (list 0 2)))
(check-expect (scalar-matrix 3 1) (list (list 1 0 0) (list 0 1 0) (list 0 0 1)))


; EXERCISE 5
; ==========

; A Network is a [List-of Person]
 
; A Person is a (make-person String Pet [List-of String])
(define-struct person [name pet friends])
; and represents their name, their type of pet, and the name of their friends
; assume all of the names in the network are unique, and that the names of friends are unique
; and represent actual people in the network
 
; A Pet is one of:
; - "dog"
; - "cat"
 
(define NETWORK
  (list
   (make-person "Alice" "dog" (list "Carol" "Heidi"))
   (make-person "Bob" "cat" (list "Carol" "Dan"))
   (make-person "Carol" "dog" (list "Alice"))
   (make-person "Dan" "cat" (list "Carol" "Eric" "Frank" "Grace"))
   (make-person "Eric" "dog" (list "Alice" "Bob" "Carol" "Dan" "Frank" "Grace"))
   (make-person "Frank" "cat" (list "Alice" "Bob" "Carol" "Dan" "Grace"))
   (make-person "Grace" "dog" (list "Bob" "Frank"))
   (make-person "Heidi" "cat" (list "Alice" "Bob" "Carol" "Dan" "Eric" "Grace"))))
(define ONEP-NETWORK (list (make-person "Ed" "dog" (list "Ed"))))
 
; get-person : Network String -> Person
; Get the person in the network (assume they are there)
(define (get-person n s)
  (cond [(string=? s (person-name (first n))) (first n)]
        [else (get-person (rest n) s)]))
(check-expect (get-person NETWORK "Bob") (make-person "Bob" "cat" (list "Carol" "Dan")))

; convert-to-pets : [List-of String] Network -> [List-of Pet]
; Converts a list of people's names to a list of those people's pets
(define (convert-to-pets lop network)
  (local
    (; pet-from-name : String -> Pet
     ; Gets the pet of the person with the given name
     (define (pet-from-name name)
       (person-pet (get-person network name))))
    (map pet-from-name lop)))

(check-expect (convert-to-pets (list "Alice" "Bob") NETWORK) (list "dog" "cat"))

; update-network : Network -> Network
; Updates the pet of every person in the network to be the pet that the majority of their friends have
(define (update-network n)
  (local [; pick-pet : [List-of Pet] Pet -> Boolean
          ; Finds the majority pet among a list of pets. If it's a tie, returns the given pet.
          (define (pick-pet lop p)
            (local [
                    (define num-dogs (length (filter dog? lop)))
                    (define half-num-pets (/ (length lop) 2))]
              (cond
                [(> num-dogs half-num-pets) "dog"]
                [(< num-dogs half-num-pets) "cat"]
                [else p])))
          ; dog? : Pet -> Boolean
          ; Checks if the given pet is a dog
          (define (dog? pet)
            (string=? pet "dog"))
          ; update-person : Person -> Person
          ; Updates a person with their new pet
          (define (update-person p)
            (make-person (person-name p)
                         (pick-pet (convert-to-pets (person-friends p) n) (person-pet p))
                         (person-friends p)))]
    (map update-person n)))

(check-expect (update-network NETWORK)
              (list
               (make-person "Alice" "dog" (list "Carol" "Heidi"))
               (make-person "Bob" "cat" (list "Carol" "Dan"))
               (make-person "Carol" "dog" (list "Alice"))
               (make-person "Dan" "dog" (list "Carol" "Eric" "Frank" "Grace"))
               (make-person "Eric" "dog" (list "Alice" "Bob" "Carol" "Dan" "Frank" "Grace"))
               (make-person "Frank" "dog" (list "Alice" "Bob" "Carol" "Dan" "Grace"))
               (make-person "Grace" "cat" (list "Bob" "Frank"))
               (make-person "Heidi" "dog" (list "Alice" "Bob" "Carol" "Dan" "Eric" "Grace"))))
(check-expect (update-network ONEP-NETWORK) ONEP-NETWORK)
(check-expect (update-network '()) '())

; EXERCISE 6
; ==========

; bubbled? : Person Network -> Boolean
; Checks if a person is only friends with people who have the same pet as they do
(define (bubbled? p n)
  (local [
          (define person (get-person n p))
          ; same-pet : Pet -> Boolean
          ; Checks if the given pet is the same as the pet of the person being checked for bubblage
          (define (same-pet pet)
            (string=? pet (person-pet person)))]
    (andmap same-pet (convert-to-pets (person-friends person) n))))

(check-expect (bubbled? "Carol" NETWORK) #t)
(check-expect (bubbled? "Alice" NETWORK) #f)
(check-expect (bubbled? "Ed" ONEP-NETWORK) #t)

; EXERCISE 7
; ==========

; bubble : Pet Network -> Network
; Removes everyone from the network who doesn't have the given pet
(define (bubble pet network)
  (local [; modify-friends : Person -> Person
          ; modifies the friends of a person to exclude wrong pet owners.
          (define (modify-friends person)
            (make-person (person-name person)
                         (person-pet person)
                         (filter good-friend? (person-friends person))))
          ; good-friend? : String -> Boolean
          ; returns if the person with the given name has the right pet.
          (define (good-friend? name)
            (right-pet? (get-person network name)))
          ; right-pet? : Person -> Boolean
          ; determines if the given person has the right pet.
          (define (right-pet? person)
            (string=? (person-pet person) pet))]
    (filter right-pet? (map modify-friends network))))

(check-expect (bubble "dog" NETWORK) (list
                                      (make-person "Alice" "dog" (list "Carol"))
                                      (make-person "Carol" "dog" (list "Alice"))
                                      (make-person "Eric" "dog" (list "Alice" "Carol" "Grace"))
                                      (make-person "Grace" "dog" '())))
(check-expect (bubble "dog" ONEP-NETWORK) ONEP-NETWORK)
(check-expect (bubble "cat" ONEP-NETWORK) '())