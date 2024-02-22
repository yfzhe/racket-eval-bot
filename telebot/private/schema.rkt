#lang racket/base
(require racket/contract/base
         "bot.rkt"
         (for-syntax racket/base
                     syntax/parse/pre
                     racket/provide-transform
                     racket/syntax
                     racket/string
                     syntax/parse/experimental/template
                     threading))

(provide define-schema schema-out
         integer? string? boolean? listof
         optional
         ref :
         define-api ->)

;; TODO:
;; - contracts
;; - recursive definition
;; - cooperate with check-syntax
;; - implement gen:custom-write
;; - schema union

(define none
  (let ()
    (struct none ())
    (none)))

(define (none? x) (eq? x none))

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

  (struct schema-info (struct-id contract fields from-jsexpr to-jsexpr))

  (define-syntax-class schema-id
    #:attributes (info)
    (pattern id:id
             #:do [(define info-id (format-id #'id "schema:~a" #'id))
                   (define local-value (syntax-local-value info-id (lambda () #f)))]
             #:fail-unless (schema-info? local-value)
             "expected an identifier for a schema name"
             #:attr info local-value
             #:with struct-id (schema-info-struct-id local-value)
             #:with contract (schema-info-contract local-value)
             #:attr fields (schema-info-fields local-value)))

  (define-syntax-class schema
    #:literals (integer? string? boolean? listof)
    #:attributes (contract from-jsexpr to-jsexpr)
    (pattern integer?
             #:with contract #'integer?
             #:attr from-jsexpr #'values
             #:with to-jsexpr #'values)
    (pattern string?
             #:with contract #'string?
             #:attr from-jsexpr #'values
             #:with to-jsexpr #'values)
    (pattern boolean?
             #:with contract #'boolean?
             #:attr from-jsexpr #'values
             #:with to-jsexpr #'values)
    (pattern (~or lit:integer lit:string lit:boolean)
             #:with contract #'(lambda (x) (equal? x lit)) ;; TODO: just use #'lit
             #:attr from-jsexpr #'values
             #:with to-jsexpr #'values)
    (pattern (listof s:schema)
             #:with contract #'(listof s.contract)
             #:with from-jsexpr #'(lambda (jsexpr) (map s.from-jsexpr jsexpr))
             #:with to-jsexpr #'(lambda (jsexpr) (map s.to-jsexpr jsexpr)))
    (pattern id:schema-id
             #:do [(define local-value (attribute id.info))]
             #:with contract (schema-info-contract local-value)
             #:with from-jsexpr (schema-info-from-jsexpr local-value)
             #:with to-jsexpr (schema-info-to-jsexpr local-value)))

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

  (define-template-metafunction (make-field-kw-arg stx)
    (syntax-parse stx
      [(_ fld:field)
       #:with name #'fld.name
       #:with kw (datum->syntax #'name
                                (string->keyword (symbol->string (syntax-e #'name))))
       (if (attribute fld.opt?)
           #'(kw [name none])
           #'(kw name))])))

(define-syntax (define-schema stx)
  (syntax-parse stx
    [(_ name:id fld:field ...)
     #:with schema-info-id (format-id #'name "schema:~a" #'name)
     #:with pred-id (format-id #'name "~a?" #'name)
     #:with ctor-id (format-id #'name "make-~a" #'name)
     #:with jsexpr->name (format-id #'name "jsexpr->~a" #'name)
     #:with name->jsexpr (format-id #'name "~a->jsexpr" #'name)

     #'(begin
         (struct name (fld.name ...)
           ;; #:constructor-name struct-ctor-id
           #:transparent)

         (define (ctor-id (~@ . (make-field-kw-arg fld)) ...)
           (name fld.name ...))

         (define (jsexpr->name value) (jsexpr->schema name value))
         (define (name->jsexpr value) (schema->jsexpr name value))

         (define-syntax schema-info-id
           (schema-info #'name
                        #'pred-id
                        (list #'fld ...)
                        #'jsexpr->name
                        #'name->jsexpr)))]))

(define-syntax (jsexpr->schema stx)
  (syntax-parse stx
    #:literals (listof)
    [(_ schema:schema-id e:expr)
     #:do [(define schema-info (attribute schema.info))
           (define fields (schema-info-fields schema-info))]
     #:with struct-id (schema-info-struct-id schema-info)
     #:with (field-value ...)
     (for/list ([fld (in-list fields)])
       (syntax-parse fld
         [fld:field
          #:with key #'fld.key
          (if (attribute fld.opt?)
              #'(let ([val (hash-ref value 'key none)])
                  (if (none? val) val (fld.from-jsexpr val)))
              #'(let ([val (hash-ref value 'key
                                     (lambda ()
                                       (error 'jsexpr->schema
                                              "field \"~a\" is missed" 'key)))])
                  (fld.from-jsexpr val)))]))
     #'(let ([value e])
         (struct-id field-value ...))]))

(define-syntax (schema->jsexpr stx)
  (syntax-parse stx
    [(_ schema:schema-id e:expr)
     #:do [(define schema-info (attribute schema.info))
           (define fields (schema-info-fields schema-info))]
     #:with struct-id (schema-info-struct-id schema-info)
     #:with (set-field-value ...)
     (for/list ([fld (in-list fields)])
       (syntax-parse fld
         [fld:field
          #:with accessor (format-id #'struct-id "~a-~a" #'struct-id #'fld.name)
          (if (attribute fld.opt?)
              #'(let ([fld-val (accessor value)])
                  (unless (none? fld-val)
                    (hash-set! jsexpr 'fld.key (fld.to-jsexpr fld-val))))
              #'(hash-set! jsexpr 'fld.key (fld.to-jsexpr (accessor value))))]))
     #'(let ([value e]
             [jsexpr (make-hash)])
         set-field-value ...
         jsexpr)]))

(define-syntax schema-out
  (make-provide-transformer
   (lambda (stx modes)
     (syntax-parse stx
       [(_ id:schema-id)
        (define export-ids
          (list #'id
                (format-id #'id "make-~a" #'id)
                (format-id #'id "schema:~a" #'id)
                (format-id #'id "jsexpr->~a" #'id)
                (format-id #'id "~a->jsexpr" #'id)))

        (for/list ([id (in-list export-ids)])
          (make-export (syntax-property id 'disappeared-use
                                        (syntax-local-introduce id))
                       id 0 #f id))]))))

(begin-for-syntax
  (define-syntax-class ref-key
    #:attributes (trimmed)
    (pattern id:id
             #:do [(define key/str (symbol->string (syntax-e #'id)))]
             #:fail-unless (string-prefix? key/str ".") "key should start with \".\""
             #:with trimmed (datum->syntax #'id (string->symbol (substring key/str 1))))))

(define-syntax (ref stx)
  (syntax-parse stx
    #:literals (:)
    [(_ (expr : schema:schema) key:ref-key ...+ (~optional failed))
     #'(%ref schema expr (key ...) (~? failed))]))

(define-syntax (%ref stx)
  (syntax-parse stx
    [(_ schema value () (~optional failed)) #'value]
    [(_ schema:schema-id expr (key:ref-key more ...) (~optional failed))
     #:do [(define schema-info (attribute schema.info))]
     #:with field:field
     (let loop ([fields (schema-info-fields schema-info)])
       (cond
         [(null? fields)
          (raise-syntax-error 'ref
                              (format "schema ~a don't have the field ~a"
                                      (syntax-e #'schema) (syntax-e #'key.trimmed))
                              #f #'key)]
         [else
          (syntax-parse (car fields)
            [fld:field
             #:when (equal? (syntax-e #'fld.name) (syntax-e #'key.trimmed))
             #'fld]
            [_ (loop (cdr fields))])]))
     #:with struct-id (schema-info-struct-id schema-info)
     #:with accessor (format-id #'struct-id "~a-~a" #'struct-id #'key.trimmed)
     (if (attribute field.opt?)
         #'(let ([val (accessor expr)])
             (if (none? val)
                 (~? failed (error 'ref "the field ~a has no value" 'key.trimmed))
                 (%ref field.schema val (more ...) (~? failed))))
         #'(let ([val (accessor expr)])
             (%ref field.schema val (more ...) (~? failed))))]
    [(_ schema expr (key more ...) (~optional failed))
     (raise-syntax-error 'ref
                         (format "schema ~a don't have fields"
                                 (syntax->datum #'schema)))]))

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
                      (unless (none? fld.name)
                        (hash-set! json 'fld.key (fld.to-jsexpr fld.name))) ...
                      json))))]))
