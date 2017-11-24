#lang racket #| The Stateless Benefit of Functional Programming |#

#| Functional programming is usually associated with one or more of:

  Immutable variables and data structures, i.e. statelessness.
  Functions as values.
  Recursion.

 The following contains a little GUI app, about a half dozen lines of code, that uses a loop
  to create two buttons along with their click callbacks. Programmers frequently code it
  incorrectly, because of state. Two implementations of a for loop are provided, to see the
  difference between the behaviour in a language whose loop *implementation* is stateful,
  versus in a language whose loop implementation is stateless. |#

; Make ‘For!’: a simple loop construct, that increments a loop variable starting at zero,
;  repeatedly evaluating a body while the variable is less than the given bound.
;
; See ★ below first, for an example usage.
;
; The only important information about the implementation is that it mutates the loop variable
;  on each iteration. Knowing that, you can skip examination of the implementation.
;
(define-syntax-rule (For! (id bound)
                          body ...)
  (local [(define id 0)
          (define (loop)
            (when (< id bound)
              body ...
              (set! id (add1 id))
              (loop)))]
    (loop)))

; ★
(define sum! 0)
(For! [n 10] ; n = 0, 1, 2, ..., n-1.
      ; Modify ‘sum!’.
      (set! sum! (+ sum! n)))
(println sum!) ; Prints 45.

; Make ‘For’: used exactly the same way as ‘For!’.
;
; See ★★, which is the same as ★, except with this choice of loop implementation.
;
; The only important information about this implementation is that it makes a fresh loop variable
;  on each iteration. Knowing that, you can skip examination of the implementation.
;
(define-syntax-rule (For (id bound)
                         body ...)
  (local [(define (loop id)
            (when (< id bound)
              body ...
              (loop (add1 id))))]
    (loop 0)))

; ★★
(define sum 0)
(For [n 10]
     (set! sum (+ sum n)))
(println sum) ; Also prints 45.

; So far, there is no difference.
; Now, let's look at a small GUI, with two buttons, where the reaction to a click is specified by
;  giving the button a “callback” - a function to call when the button is clicked - at construction.

(require racket/gui)
; Makes two window objects, details unimportant.
(define window  (new frame% [label "Functional"] [width 320]))
(define window! (new frame% [label "Stateful"]   [width 320]))

; Add two buttons to window:
(For [n 2]
     (new button%
          [label (number->string n)] ; Label the buttons "0" and "1".
          [parent window] ; Place it on ‘window’.
          [callback
           ; Click handler: a function accepting any number of arguments [with detail about the click,
           ;  which we'll ignore], and simply display the button's number [in the Interactions area].
           (λ args (displayln n))]))

; Add two buttons to ‘window!’, using identical code, except with a ‘For!’ loop:
(For! [n 2]
      (new button%
           [label (number->string n)]
           [parent window!]
           [callback (λ args (println n))]))

; Show the windows.
(send window  show #true)
(send window! show #true)
; Click the buttons: what did you expect, and what happened?

; Some people say: 0 and 1 for both windows.
; *Shared* state is difficult to reason about, but some people say: 1 and 1 for "Stateful".
; *Changing* state is difficult to keep track of, but some people say: 2 and 2 for "Stateful".

; Investigate the corresponding loop construct in languages you use, to determine their behaviour.


; If you are comfortable with continuations, they also highlight the difference:
(require (only-in racket/control prompt call/comp))
; Uncomment this after you've played with the GUI:
#;(begin
    (define K (void))
    (prompt (For [n 2]
                 (call/comp (λ (k) (set! K k)))
                 ; Saved this point in execution.
                 (println n)))
    (prompt (K))
    (prompt (K))
    (prompt (K))
    (prompt (For! [n 2]
                  (call/comp (λ (k) (set! K k)))
                  ; Saved this point in execution.
                  (println n)))
    (prompt (K))
    (prompt (K))
    (prompt (K)))
