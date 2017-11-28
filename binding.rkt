#lang racket #| Binding Concepts : let, let*, rec |#

#| An expression in the scope of some named values is “parameterized” by those values,
    in other words is a function of those values. So local naming constructs tend to be
    shorthands for making and immediately calling a function.

 The ‘let’ form is a straightforward syntactic abbreviation of that pattern.
 Then ‘let*’ varies in how it sequences the initialization of multiple names,
  and ‘rec’ allows a value to recursively refer to its name.

 The ‘rec’ form combines with ‘let’ or ‘let*’, to form ‘letrec’ and ‘letrec*’.

 The four forms ‘let’, ‘let*’, ‘letrec’, and ‘letrec*’ combine:
   • a sequence of: identifier and associated initialization expression
   • a body to evaluate in the scope of those identifiers

 The semantic distinctions involve scope and order of initialization.

 The way people usually use ‘let*’ and ‘rec’ is covered well by ‘letrec*’, so most languages
  have at least one form with ‘letrec*’ semantics, or several that differ in ways not having
  to do with scope and order of initialization. |#

#| Let: the canonical local naming construct. |#

; Equivalence to a Lambda Calculus expression: let id = e1 in e2 ≡ ((λid.e2)e1).

; Racket:
#;(let ([id init-expr]
        ...)
    body-expr
    ...
    result-expr)
;  ≡
#;((λ (id ...)
     body-expr
     ...
     result-expr)
   init-expr ...)

; Expanding the multiple-arity shorthand:
#;(((λ (id1) (λ (id2) body)) init1) init2)

; Typical restriction: distinct identifiers.
; Invalid:
#;(let ([x 1]
        [x 2])
    x)

; Semantics [when derived from eager left-to-right by-value function call].
; 1. Evaluate initialization expressions in order, producings values.
; 2. Make new nested local scope: ids shadow any ids outside the expression
; 3. Put variables into scope, initialize with the values from #1.
; 4. In the new scope: evaluate body expressions, in order, then evaluate
;     the result expression to produce the value of the whole expression.

(define x 0)

(let ([x (add1 x)])
  x)

(let ([x1 (+ 1 x)]
      [x 10]
      [x2 (+ 2 x)])
  (list x x1 x2))


#| Let*

 For “building up” named values: using some of the names in later initializations. |#

; Racket: for a single binding, the same as ‘let’, otherwise a sequence of nestings:
#;(let* ([id init-expr]
         binding
         ...)
    body-expr
    ...
    result-expr)
;  ≡
#;(let ([id init-expr])
    (let* (binding
           ...)
      body-expr
      ...
      result-expr))

#;(let* ([id1 init1]
         [id2 init2])
    body)
; Expanding to core lambda calculus:
#;((λ (id1) ((λ (id2) body) init2)) init1)

; Semantics [when derived from eager by-value function call].
; 1. For each binding [id init-expr], in order:
;    A. Evaluate initialization expression, produce a value.
;    B. Make new nested local scope: id shadows any ids outside the expression
;        and any from previous bindings
;    C. Put variable into scope, initialize with the values from #1.
; 2. In the new most-nested scope: evaluate body expressions, in order, then evaluate
;     the result expression to produce the value of the whole expression.

(let* ([x (add1 x)])
  x)

(let* ([x1 (+ 1 x)]
       [x 10]
       [x2 (+ 2 x)])
  (list x x1 x2))

(let* ([x (+ 1 x)]
       [x (+ 20 x)])
  x)
