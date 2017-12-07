#lang racket #| Gradual Typing |#

#| Compatibility Relation

 There's a type ‘?’ for “unknown”.

 Compatibility relation ‘∼’ [\sim, not tilde, in this file] between two types:
   τ ∼ τ
   ? ∼ τ, and τ ∼ ?
   σ1 ∼ τ1  ∧  σ2 ∼ τ2  ⇒  (σ1 → τ1) ∼ (σ2 → τ2)
  • in particular, it's symmetric

 But it's not transitive, so not an equivalence relation. For example:
   (? → Number) ∼ (Boolean → ?)
   (? → Number) ∼ (Number  → Number)

 Typing it via evaluation:
   (       ? τ) = ?
   ((τ′ → σ) τ) = σ , if  τ ∼ τ′ , otherwise fail

 An obstruction before Siek and Taha's 2006 paper was people's focus on subtyping.
 ToDo: where does it differ from a polymorphic type, and/or an Any type. |#

(module compatibility racket
  (provide ∼)
  (define/match (∼ τ σ)
    [(_ '?) #true]
    [('? _) #true]
    [(`(,τ1 → ,τ2) `(,σ1 → ,σ2)) (and (∼ τ1 σ1) (∼ τ2 σ2))]
    [(_ _) (equal? τ σ)])
  
  (require rackunit)
  (check-true  (∼ '(?       → Number)
                  '(Boolean → ?)))
  (check-true  (∼ '(?       → Number)
                  '(Number  → Number)))
  (check-false (∼ '(Boolean → ?)
                  '(Number  → Number))))

(require (for-syntax syntax/parse)
         (only-in racket (#%app racket:app) (λ racket:λ))
         'compatibility)
(define-syntax #%datum (syntax-parser [(_ . datum:number) #''Number]
                                      [(_ . datum:string) #''String]
                                      [(_ . datum) #''datum]))
(define-syntax #%app (syntax-parser [(_ f:expr a:expr) #'(match f
                                                           [#false #false]
                                                           ['? (and a '?)]
                                                           [`(,τ → ,σ) (and τ σ (racket:app ∼ τ a) σ)]
                                                           [_ #false])]))

(define-syntax λ (syntax-parser [(_ (an-id:id a-type:expr) body:expr)
                                 #'`(,a-type → ,(racket:app (racket:λ (an-id) body) a-type))]))

; What's the standard approach for gradually typing recursion?
;
; Should this take the join of the two?
; If it's then retyped with that annotation, can it change? Does that converge?
(define-syntax rec (syntax-parser [(_ (f-id:id (an-id:id a-type:expr) f-type:expr)
                                      body:expr)
                                   #'(local [(define f-id `(,a-type → ,f-type))
                                             (define f (λ (an-id a-type) body))]
                                       (and (racket:app ∼ f-id f) f-id))]))

('(? → Number) 'Number)
(define add1 '(Number → Number))
(add1 "hi")
(λ (x 'Number) (add1 x))
((λ (x 'Number) (add1 x)) "hi")
((λ (x '?) (add1 x)) "hi") 
(λ (f '(? → Number))
  (f 1))
((λ (f '(? → Number))
   (f 1))
 add1)
((λ (f '(? → Number))
   (f 1))
 (λ (x 'Number) (add1 x)))

; My implementation uses the declared result type if compatible, even if less informative.
(rec (f (n 'Number) '?)
  ; Body types as 'Number, as demonstrated by (add1 (f 'Number)) below.
  (add1 (f n)))
(define f (rec (f (n 'Number) '?)
            (add1 (f n))))
(add1 (f 'Number))

(rec (f (n 'Number) 'Number)
  (add1 (f n)))

#;{('(X → Y) 'X)
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
   cube}
