#lang racket/base
(require racket/match
         xml)

(provide xexpr*->string)

;; xexpr*
;; - add @ tag (splice, fragment).
;; - output empty string if a node is empty.
;;   empty node: empty string, or a node which only contains empty nodes.

;; TODO:
;; - better name for "xexpr*"
;; - more strict (only supports Telegram's HTML format)

(define (xexpr*->string xexpr*)
  (let ([output (open-output-string)])
    (write-xexpr* xexpr* output)
    (get-output-string output)))

(define (write-xexpr* xexpr* out)
  (define normalized (normalize-xexpr* xexpr*))
  (match normalized
    [(list '@ child ...)
     (for-each (lambda (node) (write-xexpr node out))
               child)]
    [_ (write-xexpr normalized out)]))

(define (empty-node? node)
  (match node
    ["" #t]
    [(list tag (list (? list? attr) ...) child ...)
     (and (null? attr)
          (andmap empty-node? child))]
    [(list tag child ...)
     (andmap empty-node? child)]
    [_ #f]))

(define (normalize-xexpr* xexpr*)
  (match xexpr*
    [(list tag children ...)
     (define node
       (cons tag (normalize-nodes children)))
     (cond
       [(empty-node? node) ""]
       [else node])]
    [_ xexpr*]))

(define (normalize-nodes nodes)
  (match nodes
    [(list) nodes]
    [(list head tail ...)
     (define head/norm (normalize-xexpr* head))
     (match head/norm
       [(list '@ child ...)
        (append child (normalize-nodes tail))]
       [(? empty-node?)
        (normalize-nodes tail)]
       [_
        (cons head/norm (normalize-nodes tail))])]))

(module+ test
  (require rackunit)

  (check-equal? (empty-node? "") #t)
  (check-equal? (empty-node? '(bold "")) #t)
  (check-equal? (empty-node? '(@)) #t)
  (check-equal? (empty-node? '(@ (@ (bold "") (code "")) (pre ""))) #t)
  (check-equal? (empty-node? '(em "non empty")) #f)
  (check-equal? (empty-node? '(a ((href "http://www.example.com/")))) #f)
  (check-equal? (empty-node? '(@ (bold "non empty") (em "too"))) #f)

  (check-equal? (xexpr*->string "string") "string")
  (check-equal? (xexpr*->string '(@)) "")
  (check-equal? (xexpr*->string '(@ (bold "") "text")) "text")
  (check-equal? (xexpr*->string '(@ (bold "") (@ (em "text") (code "(+ 1 2)"))))
                "<em>text</em><code>(+ 1 2)</code>"))
