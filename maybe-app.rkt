#lang racket #| Evaluation Model that Propagates False. |#

; Curried ‘#%app’ that short-circuits and propagates 'fail at first part of (f e0 e ...) being 'fail.
; Provides regular uncurried un-failing ‘#%app’ as ‘:’.
(module Maybe racket
  (provide (rename-out (maybe-call #%app) (#%app :)))
  (define-syntax maybe-call
    (syntax-rules ()
      [(maybe-call f e) (match f ['fail 'fail] [f′ (match e ['fail 'fail] [e′ (f′ e′)])])]
      [(maybe-call f e0 e ...) (maybe-call (maybe-call f e0) e ...)])))

(require 'Maybe)

; Produce result of e2 if e1 didn't fail.
; Can be a function, because ‘do’ won't even be called if the first argument produces ‘fail’.
(define ((do e1) e2) e2)

; Evaluate and turn #false into 'fail.
(define (? e) (or e 'fail))

; Safe log.
(define (ln x) (do (? (positive? x))
                 (log x)))

; Safe division.
(define ((÷ x) y) (do (? (not (zero? y)))
                    (: / x y)))

(local [] ; Handle printing ourselves.
  (println (sqr (ln 0)))
  (: println (sqr (÷ 1 0)))
  (println (sqr (÷ 1 0))))
