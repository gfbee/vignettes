#lang racket #| Binding Concepts : let, let*, letrec, letrec* |#

#| An expression in the scope of some named values is “parameterized” by those values,
    in other words is a function of those values. So local naming constructs tend to be
    shorthands for making and immediately calling a function.

 The ‘let’ form is a straightforward syntactic abbreviation of that pattern.

 The four forms ‘let’, ‘let*’, ‘letrec’, and ‘letrec*’ combine:
   • a sequence of: identifier and associated initialization expression
   • a body to evaluate in the scope of those identifiers

 The semantic distinctions involve scope and order of initialization.

 The ‘let*’ form varies in how it sequences the initialization of multiple names.
 The ‘letrec’ form allows an initialization expression to refer to its identifier.

 The ‘letrec*’ form combines the aspects of ‘let*’ and ‘letrec’ that people commonly use,
  so most languages have at least one form with ‘letrec*’ semantics, or several that differ
  only in ways not having to do with scope and order of initialization.

 See also §4.2.2 of the R7RS Scheme standard:

   http://www.larcenists.org/Documentation/Documentation0.98/r7rs.pdf |#


#| Let: the canonical local naming construct.

 Summary:
   1. Evaluate all the initialization expressions.
   2. Create a new environment from the identifiers.
   3. Initialize the environment from the values.
   4. Evaluate within the new environment. |#

; Equivalence to a Lambda Calculus expression: let id = e1 in e2 ≡ ((λid.e2)e1).

; Racket's ‘let’, with equivalence as a syntax rule:
#;(define-syntax-rule
    (let ([id init-expr] ...)
      body-expr ... result-expr)
    ((λ (id ...) body-expr ... result-expr)
     init-expr ...))

; Expanding the multiple-arity shorthand in the binary case:
#;(((λ (id1)
      (λ (id2)
        body))
    init1)
   init2)

; Typical restriction: distinct identifiers. For example the following is then invalid:
#;(let ([x 1] [x 2]) x)

(define x 0)

; Hover the pointer over each ‘x’ to highlight which other ‘x’s correspond to it.

(let ([x (add1 x)])
  x)

(let ([x1 (+ 1 x)]
      [x 10]
      [x2 (+ 2 x)])
  (list x x1 x2))

; Detailed semantics, when derived from eager left-to-right by-value function call:
;   1. Evaluate the initialization expressions in order, producings values.
;   2. Make a new nested local environment: variables in it shadow any variables
;       outside the expression that have the same name.
;   3. For each id put a variable into the environment, with the value from #1.
;   4. In the new environment: evaluate the body expressions, in order, then evaluate
;       the result expression to produce the value of the whole expression.


#| Let* : “building up” named values.

 Summary:
   1.  Evaluate the first initialization expression.
   2.  Create a new environment from its identifier.
   3.  Initialize the environment from the value.
   3*. Repeat 1-3 for each subsequent binding clause.
   4.  Evaluate within the new environment.

 For a single binding clause it's equivalent to ‘let’.
 For multiple binding clauses, it's equivalent to a sequence of nested ‘let’s.
   In particular the identitifiers don't need to be distinct. |#

; Racket's ‘let*’, with equivalence as a syntax rule:
#;(define-syntax let*
    (syntax-rules ()
      ; Zero bindings is the simplest base case, and then nesting also handles a single binding.
      [(let* () body-expr ... result-expr)
       (let () body-expr ... result-expr)]
      ;
      [(let* ([id init-expr] binding ...)
         body-expr ... result-expr)
       (let ([id init-expr])
         (let* (binding ...)
           body-expr ... result-expr))]))

#;(let* ([id1 init1] [id2 init2]) body)
; Expanding the binary case, to the lambda calculus:
#;((λ (id1)
     ((λ (id2)
        body)
      init2))
   init1)

(let* ([x (add1 x)])
  x)

(let* ([x1 (+ 1 x)]
       [x 10]
       [x2 (+ 2 x)])
  (list x x1 x2))

(let* ([x (+ 1 x)]
       [x (+ 20 x)])
  x)

; Detailed semantics, when derived from eager by-value function call are similar to ‘let’,
;  but nesting a new environment for each identifier.


#| Letrec and letrec* : naming recursive values.

 With eager semantics this is only useful for creating recursive and mutually recursive functions.
 With lazy semantics it can create unbounded data.

 Summary of ‘letrec’: it switches the order of steps 1 and 2 from the ‘let’ summary.
   1. Create a new environment from the identifiers.
   2. In the new environment: evaluate all the initialization expressions.
   3. Initialize the environment from the values.
   4. Evaluate within the new environment.

 Summary of ‘letrec*’: it alternates steps 2 and 3 of ‘letrec’ once per binding, allowing
  later initialization expressions to use earlier values, not just refer to them. |#

; Racket's ‘letrec’ is more precisely ‘letrec*’, both are captured with the following syntax rule,
;  refined by the comment:
#;(define-syntax-rule
    (letrec ([id init-expr] ...)
            body-expr ... result-expr)
    (let ([id (void)] ...)
      ; letrec : it's an an error to use the value of an id in the ‘set!’ expressions
      ; letrec*: it's an error to use the value of a not-yet-‘set!’ id in the ‘set!’ expressions
      (set! id init-expr) ...
      body-expr ... result-expr))


#| Racket's ‘define’ and ‘local’.

 In a definition context [a block that can mix a sequence of definitions and expressions],
  the ‘define’s collectively behave like ‘letrec*’, and the sequenced initializations are
  intermixed with the evaluations of the non-definition expressions.

 Summary:
   1. Create a new environment from the identifiers.
   2. In the new environment, evaluate each initialization and non-definition expression, in order,
       initializing each variable after its initializaton expression is evaluated.
   3. Use the result of the final expression, which must be a non-definition, as the block's value.

 Racket's ‘local’ has the same outer form as the let forms, but with ‘letrec*’ semantics, and
  definition syntax for the binding clauses.

 (local (definition ...)
   body-expr ... result-expr) |#
