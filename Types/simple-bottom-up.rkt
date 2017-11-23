;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname simple-bottom-up) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; #lang racket #| Simple Bottom-Up Type Checking. |#

; Language of <type>s:
;   'String
;   'Number
;   '(<type> → <type>)

; To type the ‘→’ character:
;   \rightarrow followed immediately [no space] by ctrl-\ or alt-\ or esc-\ depending on platform.
; A unique prefix is sufficient, e.g. : \ri
; Uncomment and run the following to see the documentation for that in your browser:
#;(begin (require help/search)
(perform-search "latex keybindings" ""))

; Automatically prints explanations for some of the racket used in this module.
(require "explain.rkt")

(require "for-ISL.rkt")

(define type-environment
  ; #hash{(string-length . (String → Number))
  ;        (string-append . (String → (String → String)))
  ;        (sqr . (Number → Number))}
  '((string-length (String → Number))
    (string-append (String → (String → String)))
    (sqr (Number → Number))))

(define (type expr)
  (cond [(string? expr) 'String]
        [(number? expr) 'Number]
        [(symbol? expr) (dict-ref type-environment expr)]
        [else #;(function argument)
              ;
              ; Expect types:
              #;((<function-argument-type> → <function-result-type>) <argument-type>)
              ;
              ; If <function-argument-type> and <argument-type> are the same, the result type is:
              ;   <function-result-type>
              ;
              (local [(define function-type (type (first expr)))]
                (cond [(list? function-type)
                       (local [(define function-argument-type (first function-type))
                               (define function-result-type (third function-type))
                               (define argument-type (type (second expr)))]
                         (if (equal? argument-type function-argument-type)
                             function-result-type
                             (error "type mismatch")))]
                      [else (error "type mismatch")]))]))

(type '(string-length "hello"))
; string-length : (String → Number)
; "hello"       :  String

#;(string-length "hello")
#;((String → Number) String)
#;Number

#;((A → R) A) ; ≡ R, otherwise type mismatch

(type 'sqr)
(type 'string-length)
(type 'string-append)
(type "a")
(type '(string-append "a"))
(type "b")
(type '((string-append "a") "b"))
(type '(string-length ((string-append "a") "b")))
(type '(sqr (string-length ((string-append "a") "b"))))
