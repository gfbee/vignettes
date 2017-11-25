#lang racket #| Simple Type Checking as Symbolic Execution |#

#| Running this program checks and produces the types of its top-level expressions and variables.

 All runtime values are types.

 To turn evaluation into checking, we override the meaning of racket's:
   • numeric and string literals
   • unary function literals (λ)
   • unary function call |#

#| Language of types.
   • 'String,  'Number
   • '(<Type> → <Type>)
 In particular, lacks: subtyping, polymorphism. |#

#| Language of expressions.

 Literals
   • <string-literal>, <numeric-literal>
   • (λ (<identifier> <Type>) <expression>)
   • <Type>
  Since values are types, a type can be used as a “canonical” literal of the type.

 Unary Function Call
   • (<function-expression> <argument-expression>)

 Variable Access
   • <identifier>

 Automatically, a racket binder for an explicitly provided expression can be used
  for non-recursive binding. In particular:
   • (define <id> <expr>)
   • (let* ([<id> <expr>] ...) <expr>)
  but not
   • (define (<id> (<id>)) <expr>)
   • letrec

 In particular, the language lacks: recursive binding, conditionals, mutation. |#

; Override numeric and string literals to produce the symbols 'Number and 'String.
(require (for-syntax syntax/parse))
(define-syntax #%datum (syntax-parser [(_ . datum:number) #''Number]
                                      [(_ . datum:string) #''String]
                                      [(_ . datum) #''datum]))

123
"hello"

; Override unary function call to "call" '(A → R) with 'A to produce 'R, otherwise produce #false.
(require (only-in racket (#%app ~app)))
(define-syntax #%app (syntax-parser [(_ f a) #'(match f
                                                 [`(,a′ → ,r) (and a′ (~app equal? a′ a) r)]
                                                 [else #false])]))

; Override unary λ to require parameter type annotation, and immediately evaluate the body
;  in the scope of the parameter with that type.
; In other words, calling the function exactly once, when created, with the “one” value of the
;  argument type, is the minimal sufficient representative of all possible calls.
(require (only-in racket (λ ~λ)))
(define-syntax λ (syntax-parser [(_ (an-id:id a-type:expr) body:expr)
                                 ; Could use any naming mechanism, but uses built-in lambda to
                                 ;  maximize the connection to “ordinary” evaluation.
                                 #'`(,a-type → ,(~app (~λ (an-id) body) a-type))]))

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
