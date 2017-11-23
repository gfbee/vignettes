#lang racket ; Print explanations of some aspects of racket code.

; When a module that require this one runs, it first prints out some information about the module.
; Current explanations are for:
;   • the #hash notation for literal hash tables
;   • quotation for symbols, empty, and non-empty lists

(provide (rename-out [datum #%datum]
                     [~quote quote]))
(require (for-syntax syntax/parse racket))

(begin-for-syntax
  (define hash-explained (make-parameter #false))
  (define quote-symbol-explained (make-parameter #false))
  (define quote-empty-explained (make-parameter #false))
  (define quote-list-explained (make-parameter #false)))

(define-syntax datum
  (syntax-parser
    #:datum-literals (→)
    [(_ . d) #:when (and (not (hash-explained)) (hash? (syntax-e #'d)))
             (hash-explained #true)
             (printf "~a\n  ~a\n  ~a\n  ~a\n  ~a\n  ~a\n"
                     "The literal notation #hash{(<key> . <value>) ...} makes a hash table."
                     "As usual for racket, balanced delimiters {}, (), or [] are interchangeable."
                     "The dot separating key and value is required, but otherwise meaningless here."
                     "The keys and values are treated as if quoted."
                     "The notation is thus equivalent to calling hash with quoted keys and values:"
                     "  (hash '<key> '<value> ...).\n")
             #''d]
    [(_ . d) #''d]))

(define-syntax ~quote
  (syntax-parser
    [(_ an-id:id) #:when (not (quote-symbol-explained))
                  (quote-symbol-explained #true)
                  (printf "The literal notation '~a makes a piece of data: a “symbol” named ‘~a’.\n\n"
                          (syntax-e #'an-id) (syntax-e #'an-id))
                  #'(quote an-id)]
    [(_ ()) #:when (not (quote-empty-explained))
            (quote-empty-explained #true)
            (printf "'() is a literal for the empty list: (list).\n")
            #'(quote ())]
    [(_ (part ...+)) #:when (not (quote-list-explained))
                     (quote-list-explained #true)
                     (display "The literal notation '(<part> ...) makes list, recursvely quoting:")
                     (displayln " (list '<part> ...).\n")
                     #'(quote (part ...))]
    [(_ . stuff) #'(quote . stuff)]))
                            
