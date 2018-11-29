;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname 13b) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; SIMPLE TYPED LANGUAGE
 
; An Expression is a:
; - Number
; - Boolean
; - (list AopName Expression Expression)
; - (list BopName Expression Expression)
; - (list CmpopName Expression Expression)
; - (list 'if Expression Expression Expression)
; - (list 'var Symbol)
; - (list 'lam Symbol Type Expression)
; - (list 'app Expression Expression)
; - (list 'pair Expression Expression)
; - (list 'fst Expression)
; - (list 'snd Expression)
; - (list 'inleft Type Expression)
; - (list 'inright Type Expression)
; - (list 'case Expression Expression 
 
(define AOPS '(+ -))
; An AopName is a member of AOPS, all of which have type: Number Number -> Number
(define BOPS '(and or))
; An BopName is a member of BOPS, all of which have type: Boolean Boolean -> Boolean
(define CmpOPS '(> < =))
; An CmpopName is a member of CmpOPS, all of which have type: Number Number -> Boolean
 
; A Type is one of:
; - 'Number
; - 'Boolean
(define-struct funty [arg ret])
; - (make-funty Type Type)
(define-struct pairty [fst snd])
; - (make-pairty Type Type)
(define-struct sumty [left right])
; - (make-sumty Type Type)
 
; ensuretype : Environment Expression Type -> Type
; Check that expression e has type t, error if e's type does not match t
(define (ensuretype env e t)
  (local ((define ty-e (typecheck-env env e)))
    (if (equal? ty-e t)
        t
        (error "Expression " e " has type " ty-e " but was expected to have type " t))))
    
 
(define-struct var:ty [var ty])
; An Environment is a [List of (make-var:ty Symbol Type)]
; Interp: A mapping from variables to types
 
; typecheck-env : Environment Expresssion -> Type
; return the type of the expression e or error if the expression is not well typed
; Accumulator: env represents...
(define (typecheck-env env e)
  (cond [(number? e) 'Number]
        [(boolean? e) 'Boolean]
        [(member? (first e) AOPS)
         (local ((define t-1 (ensuretype env (second e) 'Number))
                 (define t-2 (ensuretype env (third e) 'Number)))
           'Number)]
        [(member? (first e) BOPS)
         (local ((define t-1 (ensuretype env (second e) 'Boolean))
                 (define t-2 (ensuretype env (third e) 'Boolean)))
           'Boolean)]
        [(member? (first e) CmpOPS)
         (local ((define t-1 (ensuretype env (second e) 'Number))
                 (define t-2 (ensuretype env (third e) 'Number)))
           'Boolean)]
        [(symbol=? (first e) 'if)
         (local ((define t-0 (ensuretype env (second e) 'Boolean))
                 (define ty-e1 (typecheck-env env (third e)))
                 (define ty-e2 (typecheck-env env (fourth e))))
           (if (equal? ty-e1 ty-e2)
               ty-e1
               (error "Branches of if expression " e "have different types")))]
        [(symbol=? (first e) 'lam)
         (local [(define var-name (second e))
                 (define arg-type (third e))
                 (define t-2 (typecheck-env (cons (make-var:ty var-name arg-type) env) (fourth e)))]
           (make-funty arg-type t-2))]
        [(symbol=? (first e) 'var)
         (local [(define var-name (second e))
                 (define env-filtered (filter (λ (vt) (symbol=? (var:ty-var vt) var-name)) env))
                 (define var (if (= (length env-filtered) 1)
                                 (first env-filtered)
                                 (error "Variable " var-name " has not been defined")))
                 (define var-type (var:ty-ty var))]
           var-type)]
        [(symbol=? (first e) 'app)
         (local [(define e1 (second e))
                 (define e2 (third e))
                 (define t1 (typecheck-env env e1))
                 (define t1-arg-type (funty-arg t1))
                 (define t1-ret-type (funty-ret t1))
                 (define t2 (ensuretype env e2 t1-arg-type))]
           t1-ret-type)]
        [(symbol=? (first e) 'pair)
         (local [(define t1 (typecheck-env env (second e)))
                 (define t2 (typecheck-env env (third e)))]
           (make-pairty t1 t2))]
        [(symbol=? (first e) 'fst)
         (local [(define t1-t2 (typecheck-env env (second e)))
                 (define fst-type (pairty-fst t1-t2))]
           fst-type)]
        [(symbol=? (first e) 'snd)
         (local [(define t1-t2 (typecheck-env env (second e)))
                 (define snd-type (pairty-snd t1-t2))]
           snd-type)]
        [(symbol=? (first e) 'inleft)
         (local [(define t1+t2 (second e))
                 (define valid-sumty (if (sumty? t1+t2)
                                         t1+t2
                                         (error "Expected " t1+t2 " to be a sumty")))
                 (define t1 (sumty-left valid-sumty))
                 (define t1-valid (ensuretype env (third e) t1))]
           t1+t2)]
        [(symbol=? (first e) 'inright)
         (local [(define t1+t2 (second e))
                 (define valid-sumty (if (sumty? t1+t2)
                                         t1+t2
                                         (error "Expected " t1+t2 " to be a sumty")))
                 (define t2 (sumty-right valid-sumty))
                 (define t2-valid (ensuretype env (third e) t2))]
           t1+t2)]
        [(symbol=? (first e) 'case)
         (local [(define val (second e))
                 (define val-ty (typecheck-env env val))
                 (define (get-expr _)
                   (cond [(typecheck-env env (list 'inleft val-ty (third e)) ...]
                         [(typecheck-env env (list 'inleft (typecheck-env val-ty))) ... (third e) ...]
        
        
                 
 
; typecheck : Expression -> Type
; return the type of the expression e or error if the expression is not well typed
(define (typecheck e)
  (typecheck-env '() e))
 
(check-expect (typecheck 1) 'Number)
(check-expect (typecheck #false) 'Boolean)
(check-expect (typecheck (list '+ 1 2)) 'Number)
(check-expect (typecheck (list '- (list '+ 1 2) 5)) 'Number)
(check-error (typecheck (list '+ #false 2)))
 
(check-error (typecheck (list 'if 1 #true #false)))
(check-error (typecheck '(if (> 3 9) 1 #true)))
(check-expect (typecheck '(if (> 3 9) 1 4)) 'Number)
(check-expect (typecheck '(and #false (> 2 2))) 'Boolean)
(check-error (typecheck '(and #false (+ 2 2))))

(check-expect (typecheck (list 'lam 'x 'Number 5)) (make-funty 'Number 'Number))
(check-expect (typecheck (list 'lam 'x 'Number (list 'var 'x))) (make-funty 'Number 'Number))
(check-error (typecheck (list 'lam 'x 'Number (list 'var 'y))))
(check-expect (typecheck (list 'app (list 'lam 'x 'Number (list 'var 'x)) 5))
              'Number)
(check-error (typecheck (list 'app (list 'lam 'x 'Number (list 'var 'x)) #false)))

(check-expect (typecheck (list 'pair 5 5)) (make-pairty 'Number 'Number))
(check-expect (typecheck (list 'pair (list 'lam 'x 'Number (list 'var 'x)) 5))
              (make-pairty (make-funty 'Number 'Number) 'Number))
(check-expect (typecheck (list 'fst (list 'pair (list 'lam 'x 'Number (list 'var 'x)) 5)))
              (make-funty 'Number 'Number))
(check-expect (typecheck (list 'snd (list 'pair (list 'lam 'x 'Number (list 'var 'x)) 5)))
              'Number)
