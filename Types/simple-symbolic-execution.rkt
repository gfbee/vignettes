#lang racket #| Simple Type Checking as Symbolic Execution |#

; Override numeric and string literals to produce the symbols 'Number and 'String.
(require (for-syntax syntax/parse))
(define-syntax #%datum
  (syntax-parser
   [(_ . datum:number) #''Number]
   [(_ . datum:string) #''String]
   [(_ . datum) #''datum]))

123
"hello"

; Overrides unary function call to "call" '(A → R) with 'A to produce 'R,
;  otherwise produce #false.
(require (only-in racket (#%app ~app)))
(define-syntax #%app
  (syntax-parser
    [(_ f a) #'(match f
                 [`(,a′ → ,r) (and a′ (~app equal? a′ a) r)]
                 #;[f′ #:when (~app procedure? f′) (~app f′ a)]
                 [else #false])]))

; Overrides unary λ to have parameter type annotation, and immediately evaluates
;  the body in the scope of the parameter with that type.
(require (only-in racket (λ ~λ)))
(define-syntax λ
  (syntax-parser
    [(_ (an-id:id a-type:expr) body:expr) #'`(,a-type → ,(~app (~λ (an-id) body) a-type))]))

('(X → Y) 'X)
('(X → Y) 'Z)

('(Number → String) 'Number)
('(Number → String) 123)
('(Number → String) "hi")

(define string-length '(String → Number))
(define string-append '(String → (String → String)))
(define sqr '(Number → Number))

(define s "b")
s
(string-length s)
(sqr s)
(sqr (string-length s))

(string-append "a")
((string-append "a") s)
(string-length ((string-append "a") s))
(sqr (string-length ((string-append "a") s)))

(define f (λ (s 'String) (string-length ((string-append "a") s))))
f
(define * '(Number → (Number → Number)))
(define cube (λ (x 'Number) ((* ((* x) x)) x)))
cube
