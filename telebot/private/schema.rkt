#lang racket/base
(require racket/contract/base
         "bot.rkt"
         (for-syntax racket/base
                     syntax/parse/pre
                     racket/syntax
                     racket/string
                     syntax/parse/experimental/template
                     threading))

(provide define-schema
         integer? string? boolean? true? listof
         optional
         define-api ->
         ref :)

;; TODO:
;; - implement gen:custom-write
;; - provide transformer `schema-out`
;; - field converter?
;; - add contracts
;; - schema union

(define json-undefined ;; though json doesn't have "undefined" type
  (let ()
    (struct undefined ())
    (undefined)))

(define (json-undefined? x) (eq? x json-undefined))

(define (true? x) (eq? x #t))

(define-syntax optional
  (lambda (stx)
    (raise-syntax-error #f "optional should be used in define-schema" stx)))

(define-syntax :
  (lambda (stx)
    (raise-syntax-error #f ": should be used in ref" stx)))

#;
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

  (define-syntax-class schema-id
    #:attributes (info)
    (pattern id:id
             #:do [(define info-id (format-id #'id "schema:~a" #'id))
                   (define local-value (syntax-local-value info-id (lambda () #f)))]
             #:fail-unless (schema-info? local-value)
             "expected an identifier for a schema name"
             #:attr info local-value))

  (define-syntax-class schema
    #:literals (integer? string? boolean? true? listof)
    #:attributes (struct-id fields from-jsexpr to-jsexpr)
    (pattern (~or integer? string? boolean? true?)
             #:attr struct-id #f
             #:attr fields '()
             #:with from-jsexpr #'values
             #:with to-jsexpr #'values)
    (pattern (listof s:schema)
             #:attr struct-id #f
             #:attr fields '()
             #:with from-jsexpr #'(lambda (jsexpr) (map s.from-jsexpr jsexpr))
             #:with to-jsexpr #'(lambda (jsexpr) (map s.to-jsexpr jsexpr)))
    (pattern id:schema-id
             #:do [(define local-value (attribute id.info))]
             #:with struct-id (schema-info-struct-id local-value)
             #:attr fields (schema-info-fields local-value)
             #:with from-jsexpr (schema-info-from-jsexpr local-value)
             #:with to-jsexpr (schema-info-to-jsexpr local-value))
    ;; this clause is a temporal fallback for recursive schema definition
    ;; TODO: distinguish the strict and the loose situation
    (pattern id:id
             #:attr struct-id #f
             #:attr fields '()
             #:attr from-jsexpr #f
             #:attr to-jsexpr #f))

  (define-syntax-class schema/opt
    #:literals (optional)
    #:attributes (opt? type type.from-jsexpr type.to-jsexpr)
    (pattern (optional type:schema) #:attr opt? #t)
    (pattern type:schema #:attr opt? #f))

  (define-syntax-class field
    #:attributes (name schema opt? key from-jsexpr to-jsexpr)
    (pattern (name:id type+:schema/opt (~optional key*:string))
             #:with key
             (if (attribute key*)
                 (datum->syntax #'key* (string->symbol (syntax-e #'key*)))
                 (kebab-case->snake-case/id #'name))
             #:with schema #'type+.type
             #:attr opt? (attribute type+.opt?)
             #:attr from-jsexpr (attribute type+.type.from-jsexpr)
             #:attr to-jsexpr (attribute type+.type.to-jsexpr)))

  (define-syntax-class ref-key
    #:attributes (trimed)
    (pattern id:id
             #:do [(define key/str (symbol->string (syntax-e #'id)))]
             #:fail-unless (string-prefix? key/str ".") "key should start with \".\""
             #:with trimed (datum->syntax #'id (string->symbol (substring key/str 1)))))

  (define-template-metafunction (make-field-kw-arg stx)
    (syntax-parse stx
      [(_ fld:field)
       #:with name #'fld.name
       #:with kw (datum->syntax #'name
                                (string->keyword (symbol->string (syntax-e #'name))))
       (if (attribute fld.opt?)
           #'(kw [name json-undefined])
           #'(kw name))])))

(define-syntax (define-schema stx)
  (syntax-parse stx
    [(_ name:id fld:field ...)
     #:with schema-info-id (format-id #'name "schema:~a" #'name)
     #:with ctor-id (format-id #'name "make-~a" #'name)
     #:with jsexpr->name (format-id #'name "jsexpr->~a" #'name)
     #:with name->jsexpr (format-id #'name "~a->jsexpr" #'name)

     (syntax-local-lift-module-end-declaration
      #'(begin
          (define-jsexpr->schema name jsexpr->name)
          (define-schema->jsexpr name name->jsexpr)))

     #'(begin
         (struct name (fld.name ...)
           ;; #:constructor-name struct-ctor-id
           #:transparent)

         (define (ctor-id (~@ . (make-field-kw-arg fld)) ...)
           (name fld.name ...))

         (define-syntax schema-info-id
           (schema-info #'name
                        (list #'fld ...)
                        #'jsexpr->name
                        #'name->jsexpr)))]))

(define-syntax (define-jsexpr->schema stx)
  (syntax-parse stx
    [(_ schema:schema-id jsexpr->schema:id)
     #:do [(define schema-info (attribute schema.info))
           (define fields (schema-info-fields schema-info))]
     #:with struct-id (schema-info-struct-id schema-info)
     #:with (field-value ...)
     (for/list ([fld (in-list fields)])
       (syntax-parse fld
         [fld:field
          #:with key #'fld.key
          (if (attribute fld.opt?)
              #'(let ([val (hash-ref jsexpr 'key json-undefined)])
                  (if (json-undefined? val) val (fld.from-jsexpr val)))
              #'(let ([val (hash-ref jsexpr 'key
                                     (lambda () (raise-argument-error
                                                 'jsexpr->schema
                                                 "field \"~a\" is missed" 'key)))])
                  (fld.from-jsexpr val)))]))
     #'(define (jsexpr->schema jsexpr)
         (struct-id field-value ...))]))

(define-syntax (define-schema->jsexpr stx)
  (syntax-parse stx
    [(_ schema:schema-id schema->jsexpr:id)
     #:do [(define schema-info (attribute schema.info))
           (define fields (schema-info-fields schema-info))]
     #:with struct-id (schema-info-struct-id schema-info)
     #:with (set-field-value ...)
     (for/list ([fld (in-list fields)])
       (syntax-parse fld
         [fld:field
          #:with accessor (format-id #'struct-id "~a-~a" #'struct-id #'fld.name)
          (if (attribute fld.opt?)
              #'(let ([fld-val (accessor data)])
                  (unless (json-undefined? fld-val)
                    (hash-set! jsexpr 'fld.key (fld.to-jsexpr fld-val))))
              #'(hash-set! jsexpr 'fld.key (fld.to-jsexpr (accessor data))))]))
     #'(define (schema->jsexpr data)
         (let ([jsexpr (make-hash)])
           set-field-value ...
           jsexpr))]))

(define-syntax (ref stx)
  (syntax-parse stx
    #:literals (:)
    [(_ (expr : schema:schema) key:ref-key ...+ (~optional failed))
     #'(%ref schema expr (key ...) (~? failed))]))

(define-syntax (%ref stx)
  (syntax-parse stx
    [(_ schema value () (~optional failed)) #'value]
    [(_ schema:schema expr (key:ref-key more ...) (~optional failed))
     #:with field:field
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
             #'fld]
            [_ (loop (cdr fields))])]))
     #:with struct-id #'schema.struct-id
     #:with accessor (format-id #'struct-id "~a-~a" #'struct-id #'key.trimed)
     (if (attribute field.opt?)
         #'(let ([val (accessor expr)])
             (if (json-undefined? val)
                 (~? failed (raise-argument-error 'ref "the field ~a is undefined" 'key.trimed))
                 (%ref field.schema val (more ...) (~? failed))))
         #'(let ([val (accessor expr)])
             (%ref field.schema val (more ...) (~? failed))))]))

(define-syntax (define-api stx)
  (syntax-parse stx
    #:literals (->)
    [(_ api-id:id endpoint:string arg:schema -> ret:schema)
     #'(define (api-id bot arg)
         (ret.from-jsexpr (bot-post bot endpoint (arg.to-jsexpr arg))))]
    [(_ api-id:id endpoint:string (fld:field ...) -> ret:schema)
     #'(define (api-id bot (~@ . (make-field-kw-arg fld)) ...)
         (ret.from-jsexpr
          (bot-post bot endpoint
                    (let ([json (make-hasheq)])
                      (unless (json-undefined? fld.name)
                        (hash-set! json 'fld.key (fld.to-jsexpr fld.name))) ...
                      json))))]))
