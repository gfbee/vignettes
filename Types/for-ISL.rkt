#lang racket

(provide dict-ref)

(define (dict-ref a-dictionary key)
  (second (assoc key a-dictionary)))
