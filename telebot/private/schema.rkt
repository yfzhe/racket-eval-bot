#lang racket/base
(require "bot.rkt"
         (for-syntax racket/base
                     syntax/parse/pre
                     racket/syntax
                     racket/string
                     syntax/parse/experimental/template
                     threading))

(provide define-schema
         optional
         define-api ->
         ref :)

;; TODO:
;; - add primitive schemas
;; - implement gen:custom-write
;; - provide transformer `schema-out`
;; - field converter?
;; - contracts?

(define json-undefined ;; though json doesn't have "undefined" type
  (let ()
    (struct undefined ())
    (undefined)))

(define (json-undefined? x) (eq? x json-undefined))

(define-syntax optional
  (lambda (stx)
    (raise-syntax-error #f "optional should be used in define-schema" stx)))

(define-syntax :
  (lambda (stx)
    (raise-syntax-error #f ": should be used in ref" stx)))

(define-syntax ->
  (lambda (stx)
    (raise-syntax-error #f "-> should be used in define-api" stx)))

(begin-for-syntax
  (define (kebab-case->snake-case/str str)
    ;; TODO: how to handle non-alphabetic characters?
    (string-replace str "-" "_"))

  (define (kebab-case->snake-case/id id)
    (~> (syntax-e id)
        symbol->string
        kebab-case->snake-case/str
        string->symbol
        (datum->syntax id _)))

  (struct schema-info (struct-id fields from-jsexpr to-jsexpr))

  (define-syntax-class schema
    #:attributes (struct-id fields from-jsexpr to-jsexpr)
    (pattern id:id
             #:with info-id (format-id #'id "schema:~a" #'id)
             #:do [(define local-value (syntax-local-value #'info-id (lambda () #f)))]
             #:when (schema-info? local-value)
             #:with struct-id (schema-info-struct-id local-value)
             #:attr fields (schema-info-fields local-value)
             #:with from-jsexpr (schema-info-from-jsexpr local-value)
             #:with to-jsexpr (schema-info-to-jsexpr local-value))
    (pattern id
             #:attr struct-id #f
             #:attr fields '()
             #:with from-jsexpr #'begin
             #:with to-jsexpr #'begin))

  (define-syntax-class schema/opt
    #:literals (optional)
    #:attributes (opt? type type.from-jsexpr type.to-jsexpr)
    (pattern (optional type:schema) #:attr opt? #t)
    (pattern type:schema #:attr opt? #f))

  (define-syntax-class field
    #:attributes (name schema opt? key from-jsexpr to-jsexpr)
    (pattern (name:id type:schema/opt (~optional key*:string))
             #:with key
             (if (attribute key*)
                 (datum->syntax #'key* (string->symbol (syntax-e #'key*)))
                 (kebab-case->snake-case/id #'name))
             #:with schema #'type.type
             #:attr opt? (attribute type.opt?)
             #:with from-jsexpr #'type.type.from-jsexpr
             #:with to-jsexpr #'type.type.to-jsexpr))

  (define-syntax-class ref-key
    #:attributes (trimed)
    (pattern id:id
             #:do [(define key/str (symbol->string (syntax-e #'id)))]
             #:fail-unless (string-prefix? key/str ".") "key should start with \".\""
             #:with trimed (datum->syntax #'id (string->symbol (substring key/str 1)))))

  (define-template-metafunction (make-field-failed stx)
    (syntax-parse stx
      [(_ converter-id fld:field)
       (if (attribute fld.opt?)
           #'json-undefined
           #'(lambda () (error 'converter-id "field \"~a\" is missed" 'fld.key)))]))

  (define-template-metafunction (make-field-kw-arg stx)
    (syntax-parse stx
      [(_ fld:field)
       #:with name #'fld.name
       #:with kw (datum->syntax #'name
                                (string->keyword (symbol->string (syntax-e #'name))))
       (if (attribute fld.opt?)
           #'(kw [name json-undefined])
           #'(kw name))]))

  (define-template-metafunction (make-accessor stx)
    (syntax-parse stx
      [(_ id fld:field) (format-id #'id "~a-~a" #'id #'fld.name)])))

(define-syntax (define-schema stx)
  (syntax-parse stx
    [(_ name:id fld:field ...)
     #:with schema-info-id (format-id #'name "schema:~a" #'name)
     #:with ctor-id (format-id #'name "make-~a" #'name)
     #:with jsexpr->name (format-id #'name "jsexpr->~a" #'name)
     #:with name->jsexpr (format-id #'name "~a->jsexpr" #'name)
     #'(begin
         (struct name (fld.name ...)
           ;; #:constructor-name struct-ctor-id
           #:transparent)

         (define (ctor-id (~@ . (make-field-kw-arg fld)) ...)
           (name fld.name ...))

         (define (jsexpr->name jsexpr)
           (name (fld.from-jsexpr
                  (hash-ref jsexpr 'fld.key (make-field-failed jsexpr->name fld)))
                 ...))

         (define (name->jsexpr data)
           (define jsexpr (make-hash))
           (let ([fld-v ((make-accessor name fld) data)])
             (unless (json-undefined? fld-v)
               (hash-set! jsexpr 'fld.key (fld.to-jsexpr fld-v)))) ...
           jsexpr)

         (define-syntax schema-info-id
           (schema-info #'name
                        (list #'fld ...)
                        #'jsexpr->name
                        #'name->jsexpr)))]))

(define-syntax (ref stx)
  (syntax-parse stx
    #:literals (:)
    [(_ (expr : schema:schema) key:ref-key ...+ (~optional failed))
     #'(%ref schema expr (key ...) (~? failed))]))

(define-syntax (%ref stx)
  (syntax-parse stx
    [(_ schema value () (~optional failed)) #'value]
    [(_ schema:schema expr (key:ref-key more ...) (~optional failed))
     #:with (field-name field-schema)
     (let loop ([fields (attribute schema.fields)])
       (cond
         [(null? fields)
          (raise-syntax-error 'ref (format "schema ~a don't have the field ~a"
                                           (syntax-e #'schema) (syntax-e #'key.trimed))
                              #f #'key)]
         [else
          (syntax-parse (car fields)
            [fld:field
             #:when (equal? (syntax-e #'fld.name) (syntax-e #'key.trimed))
             #'(fld.name fld.schema)]
            [_ (loop (cdr fields))])]))
     #:with struct-id #'schema.struct-id
     #:with accessor (format-id #'struct-id "~a-~a" #'struct-id #'key.trimed)
     #'(let ([val (accessor expr)])
         (if (json-undefined? val) ;; TODO: optimize this check out if the field is not optional
             (~? failed (raise-argument-error 'ref "the field ~a is undefined" 'key.trimed))
             (%ref field-schema val (more ...) (~? failed))))]))

(define-syntax (define-api stx)
  (syntax-parse stx
    #:literals (->)
    [(_ api-id endpoint (~optional arg:schema) -> ret:schema)
     #'(define (api-id bot (~? arg))
         (ret.from-jsexpr (bot-post bot endpoint (~? (arg.to-jsexpr arg)))))]))
