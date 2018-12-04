;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname 14b) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; 
; StackLang ;;
; 
 
 
; P ::= (i-1,...,i-n)
; i ::= ret v | ret 'x | sub | if0 P P | call | lam x.P
; S ::= (v-1, ..., v-n)
; v ::= n | thunk P
; 
; (P,S) ==> S'
; 
; (i,P,S) ==> S'
 
; A Program is a [List-of Instruction]
 
; An Instruction is a:
; - (list 'push Value)
; - (list 'sub)
; - (list 'mul)
; - (list 'add)
; - (list 'if0 Program Program)
; - (list 'call)
; - (list 'lam Symbol Program)
; - (list 'fst Value)
; - (list 'snd Value)
; - (list 'unwrap)
 
; A Stack is a [List-of Value]
 
; A Value is a:
; - Symbol
; - (list 'num Number)
; - (list 'thunk Program)
; - (list 'pair Value Value)
 
; 
; INTERPRETER ;;
; 
 
; interp : Program Stack -> Stack
; runs a program with a given starting stack, producing the final stack or erroring.
(define (interp prog stk)
  (cond
    [(empty? prog) stk]
    [(cons? prog) (interp-instr (first prog) (rest prog) stk)]))
 
(check-expect (interp (list '(push (num 2)))
                      (list))
              (list '(num 2)))
(check-expect (interp (list '(push (num 2)) '(push (num 1)) '(sub))
                      (list))
              (list '(num -1)))
(check-error (interp '((push x))
                     (list)))
 
(check-expect (interp (list '(push (thunk ((push (num 1))))) '(call))
                      (list))
              (list '(num 1)))
 
(check-expect (interp (list '(push (num 0)) '(if0 ((push (num 10))) ((push (num 20)))))
                      (list))
              (list '(num 10)))
 
(define p1 (list '(push (num 1)) '(push (thunk ((lam x ((push x)))))) '(call)))
(check-expect (interp p1 (list))
              (list '(num 1)))
(define p2
  (list '(push (num 2)) '(push (num 1)) '(push (num 3)) '(push (thunk ((lam 'x ((sub)))))) '(call)))
(check-expect (interp p2 (list))
              (list '(num -1)))
 
; interp-instr : Instruction Program Stack -> Stack
; runs an instruction with a stack and rest of program, producing the final stack or erroring.
(define (interp-instr i prog stk)
  (cond
    [(symbol=? (first i) 'push)
     (if (symbol? (second i))
         (error "Trying to interp a free variable: " (second i))
         (interp prog (cons (second i) stk)))]
    [(symbol=? (first i) 'sub) (interp-sub prog stk)]
    [(symbol=? (first i) 'add) (interp-add prog stk)]
    [(symbol=? (first i) 'mul) (interp-mul prog stk)]
    [(symbol=? (first i) 'if0) (interp-if0 (second i) (third i) prog stk)]
    [(symbol=? (first i) 'call) (interp-call prog stk)]
    [(symbol=? (first i) 'lam) (interp-lam (second i) (third i) prog stk)]
    [(symbol=? (first i) 'unwrap) (interp-unwrap prog stk)]))
 
 
(check-expect (interp-instr '(sub)
                            (list)
                            '((num 1) (num 2)))
              (list '(num -1)))
(check-expect (interp-instr '(push (num 1))
                            (list '(sub))
                            '((num 1)))
              (list '(num 0)))
(check-expect (interp-instr '(push (num 1))
                            (list)
                            '((num 1)))
              '((num 1) (num 1)))
 
 
; interp-sub : Program Stack -> Stack
; applies subtraction and then continues running the rest of the program on resulting stack
(define (interp-sub prog stk)
  (local [(define n1 (first stk))
          (define n2 (second stk))
          (define ret (list 'num (- (second n1) (second n2))))]
    (interp prog (cons ret (rest (rest stk))))))

(check-expect (interp-sub '() '((num 1) (num 2))) '((num -1)))

; interp-add : Program Stack -> Stack
; applies addition and then continues running the rest of the program on resulting stack
(define (interp-add prog stk)
  (local [(define n1 (first stk))
          (define n2 (second stk))
          (define ret (list 'num (+ (second n1) (second n2))))]
    (interp prog (cons ret (rest (rest stk))))))
(check-expect (interp-add '() '((num 1) (num 2))) '((num 3)))

; interp-mul : Program Stack -> Stack
; applies multiplication and then continues running the rest of the program on resulting stack
(define (interp-mul prog stk)
  (local [(define n1 (first stk))
          (define n2 (second stk))
          (define ret (list 'num (* (second n1) (second n2))))]
    (interp prog (cons ret (rest (rest stk))))))
(check-expect (interp-mul '() '((num 1) (num 2))) '((num 2)))
 
; interp-call : Program Stack -> Stack
; pops a Program off the top of the stack and continues running the program, erroring if no thunk.
(define (interp-call prog stk)
  (cond
    [(< (length stk) 1)
     (error "Could not apply 'call, as the stack was empty.")]
    [(or (not (list? (first stk)))
         (not (equal? 'thunk (first (first stk)))))
     (error "Could not apply 'call, as the top of the stack was not a thunk.")]
    [else (interp (append (second (first stk)) prog)
                  (rest stk))]))
 
(check-expect (interp-call (list)
                           (list (list 'thunk '((push (num 1))))))
              (list '(num 1)))
(check-expect (interp-call (list)
                           (list (list 'thunk '((sub))) '(num 2) '(num 1)))
              (list '(num 1)))
 
; interp-if0 : Program Program Program Stack -> Stack
; pops a number off the stack;
; if number is 0, run thn Program followed by prog on the resulting stack,
; otherwise run els Program and then prog on the resulting stack;
; error if no number on top of stack.
(define (interp-if0 thn els prog stk)
 (local [(define n (first stk))]
   (if (number? (second n))
       (if (zero? (second n))
           (interp prog (interp thn (rest stk)))
           (interp prog (interp els (rest stk))))
       (error ("Expected input to if0 to be a number, got " n)))))
(check-expect (interp-if0 '((push (num 10))) '((push (num 5))) '() '((num 0))) '((num 10)))
(check-expect (interp-if0 '((push (num 10))) '((push (num 5))) '() '((num 1))) '((num 5)))
(check-error (interp-if0 '((push (num 10))) '((push (num 5))) '() '((x))))

; —————————————————-
; interp-lam : Symbol Program Program Stack -> Stack
; substitutes into a lambda and stores result on the stack
(define (interp-lam x body prog stk)
  (cond
    [(< (length stk) 1)
     (error "could not apply 'lam, as the stack was empty")]
    [else (interp (append (substitute x (first stk) body) prog)
                  (rest stk))]))
 
(check-expect (interp-lam 'x (list '(push x)) '()
                          (list '(num 1)))
              (list '(num 1)))
 
; substitute : Symbol Value Program -> Program
; substitutes free occurrences of x with v in prog
(define (substitute x v prog)
  (cond [(empty? prog) prog]
        [(cons? prog) (cons (substitute-instr x v (first prog)) (substitute x v (rest prog)))]))
(check-expect (substitute 'x '(num 1) (list '(push x) '(push (num 2))))
              (list '(push (num 1)) '(push (num 2))))
(check-expect (substitute 'x '(num 1) (list '(push x) '(lam x (push x))))
              (list '(push (num 1)) '(lam x (push x))))
 
; substitute-instr : Symbol Value Instruction -> Instruction
; substitutes free occurrences of x with v in instruction i
(define (substitute-instr x v i)
  (cond
    [(symbol=? (first i) 'push)
     (list 'push (substitute-val x v (second i)))]
    [(symbol=? (first i) 'sub) i]
    [(symbol=? (first i) 'mul) i]
    [(symbol=? (first i) 'if0) (list 'if0 (substitute x v (second i))
                                          (substitute x v (third i)))]
    [(symbol=? (first i) 'call) i]
    [(symbol=? (first i) 'lam)
     (if (not (symbol=? (second i) x))
         (list 'lam (second i) (substitute x v (third i)))
         i)]))
 
(check-expect (substitute-instr 'x '(num 1) '(push x))
              '(push (num 1)))
(check-expect (substitute-instr 'x '(num 1) '(lam x ((push x))))
              '(lam x ((push x))))
 
; substitute-val : Symbol Value Value -> Value
; substitutes free occurrences of x with v in original value v0
(define (substitute-val x v v0)
  (cond [(symbol? v0)
         (if (symbol=? x v0) v v0)]
        [(symbol=? (first v0) 'num) v0]
        [(symbol=? (first v0) 'thunk) (list 'thunk (substitute x v (second v0)))]))

; Exercise 2

; dup : Program
; dup duplicates the top value on the stack,
; transforming a stack from v1,v2,v3... to v1,v1,v2,v3...
(define dup
  '((push (thunk ((lam x ((push x) (push x)))))) (call)))
(check-expect (interp dup '((num 5))) '((num 5) (num 5)))
(check-expect (interp dup '((num 5) (num 6))) '((num 5) (num 5) (num 6)))
(check-expect (interp dup '((thunk ((push 5))))) '((thunk ((push 5))) (thunk ((push 5)))))
 
; drop : Program
; drop drops the top value from the stack,
; transforming a stack from v1,v2,v3... to v2,v3...
(define drop
  '((push (thunk ((lam x ())))) (call)))
(check-expect (interp drop '((num 5))) '())
(check-expect (interp drop '((num 5) (num 6))) '((num 6)))
(check-expect (interp drop '((thunk ((push 5))))) '())
 
; swap : Program
; swap swaps the top two values on the stack,
; transforming a stack from v1,v2,v3... to v2,v1,v3...
(define swap
  '((push (thunk ((lam x ((push (thunk ((lam y ((push x) (push y)))))) (call)))))) (call)))
(check-expect (interp swap '((num 5) (num 6))) '((num 6) (num 5)))
(check-expect (interp swap '((thunk ((push (num 1)))) (num 5) (num 6)))
              '((num 5) (thunk ((push (num 1)))) (num 6)))

; Exercise 3
; interp-unwrap : Program Stack -> Stack
; Pops the top value off the stack, assuming its a pair, pushes its two elements back on the stack
; and then continues running the rest of the program on resulting stack
(define (interp-unwrap prog stk)
  (local [(define p (first stk))
          (define left (if (= (length p) 3)
                           (second p)
                           (error "Expected top of stack to be a pair, got " p)))
          (define right (third p))]
    (interp prog (append (list left right) (rest stk)))))

(check-expect (interp-unwrap '() '((pair (num 3) (num 2)) (num 5))) '((num 3) (num 2) (num 5)))


; Exercise 4

; An Expression is a:
; - Number
; - Boolean
; - (list '+ Expression Expression)
; - (list 'if Expression Expression Expression)
; - (list 'var Symbol)
; - (list 'lam Symbol Type Expression)
; - (list 'app Expression Expression)
; - (list 'pair Expression Expression)
; - (list 'fst Expression)
; - (list 'snd Expression)
; - (list 'inleft Type Type Expression)
; - (list 'inright Type Type Expression)
; - (list 'case Expression Symbol Expression Symbol Expression)

; compile : Expression -> Program
; translates Expression into a Program where the Program evaluates to what
; the Expression evaluates to
(define (compile e)
  (cond [(number? e) `((push (num ,e)))]
        [(boolean? e) ...]
        [(symbol=? (first e) '+) (append (compile (second e)) ;;; n1, ...
                                         (compile (third e))  ;;; n2,n1 ...
                                         '((add)))]
        [(symbol=? (first e) 'if) ...]
        [(symbol=? (first e) 'var) ...]
        [(symbol=? (first e) 'lam) ...]
        [(symbol=? (first e) 'app) ...]
        [(symbol=? (first e) 'pair) (compile-pair (second e) (third e))]
        [(symbol=? (first e) 'fst) ...]
        [(symbol=? (first e) 'snd) ...]
        [(symbol=? (first e) 'inleft) (compile-inleft (fourth e))]
        [(symbol=? (first e) 'inright) (compile-inright (fourth e))]
        [(symbol=? (first e) 'case)
         (compile-case (second e) (third e) (fourth e) (fifth e) (sixth e))]))


(check-expect (interp (compile 1) '()) '((num 1)))
(check-expect (interp (compile '(+ 1 2)) '()) '((num 3)))

; compile-pair : Expression Expression -> Program
; translates the Expressions into in a pair
(define (compile-pair e1 e2)
  `((push (pair ,(compile e1) ,(compile e2)))))

(check-expect (interp (compile-pair 3 #t rue)) '(pair 3 #true)) 

; compile-inleft : Expression -> Program
; translates Expression in an inleft into corresponding StackLang Program
(define (compile-inleft e)
  `((unwrap) (swap) (drop) ,(compile e)))

(check-expect (compile-inleft '(inleft Number Boolean (pair 3 #true)))
              '((unwrap) (swap) (drop) (num 3)))
 
; compile-inright : Expression -> Program
; translates Expression in an inright into corresponding StackLang Program
(define (compile-inright e)
  `((unwrap) (drop) ,(compile e)))
 
; compile-case : Expression Symbol Expression Symbol Expression -> Program
; translates Expressions, bindings, and branches of a case into corresponding StackLang Program
(define (compile-case e x e1 y e2)
  `((push (pair (thunk ((lam ,x ,(compile e1))))
                (thunk ((lam ,y ,(compile e2))))))
    ,(compile e) (call)))
