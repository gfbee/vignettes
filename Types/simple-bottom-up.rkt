#lang racket #| Simple Bottom-Up Type Checking. |#

; Language of <type>s:
;   'String
;   'Number
;   '(<type> → <type>)

(define type-environment #hash{(string-length . (String → Number))
                               (string-append . (String → (String → String)))
                               (sqr . (Number → Number))})

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
              (define function-type (type (first expr)))
              (unless (list? function-type) (error "type mismatch"))
              (define function-argument-type (first function-type))
              (define function-result-type (third function-type))
              ;
              (define argument-type (type (second expr)))
              ;
              (if (equal? argument-type function-argument-type)
                  function-result-type
                  (error "type mismatch"))]))

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

