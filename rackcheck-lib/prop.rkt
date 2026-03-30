#lang racket/base

(require (for-syntax racket/base
                     syntax/parse/pre)
         racket/contract/base
         racket/match
         racket/random
         racket/format
         racket/stream
         json
         "gen/syntax.rkt"
         "gen/core.rkt"
         (submod "gen/shrink-tree.rkt" private))

;; property ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide
 (rename-out [prop? property?]
             [prop-name property-name])
 sample-with-time
 quick-sample
 property
 define-property)

(struct prop (name arg-ids g f))

(module+ private
  (provide (struct-out prop)))

(define-syntax (property stx)
  (syntax-parse stx
    [(_ (~optional (~or name-id:id (~seq #:name name-ex:expr)))
        ([id:id g:expr] ...)
        body ...+)
     #'(prop (~? 'name-id (~? name-ex 'unnamed))
             (list 'id ...)
             (gen:let ([id g] ...)
                      (list id ...))
             (lambda (id ...)
               body ...))]))

(define-syntax (define-property stx)
  (syntax-parse stx
    [(_ name:id binds body ...+)
     #'(define name
         (property name binds body ...))]))

(module+ test
  (require "gen/base.rkt")

  (define prop-addition-commutes ;; noqa
    (property ([a gen:natural]
               [b gen:natural])
              (= (+ a b)
                 (+ b a))))

  (define prop-addition-is-multiplication ;; noqa
    (property ([a gen:natural]
               [b gen:natural])
              (= (+ a b)
                 (* a b)))))


;; config ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define features-funcs? (->* () #:rest (listof any/c) any/c))
(define features? (listof (cons/c string? features-funcs?)))

(provide
 config?
 (contract-out
  [make-config (->* []
                    [#:seed (integer-in 0 (sub1 (expt 2 31)))
                     #:tests exact-positive-integer?
                     #:size (-> exact-positive-integer? exact-nonnegative-integer?)
                     #:deadline (>=/c 0)
                     #:prop-run-start real?
                     #:tyche boolean?
                     #:features features?
                     ]
                    config?)]))

(struct config (seed tests size deadline prop-run-start tyche features))

(define (make-config #:seed [seed (make-random-seed)]
                     #:tests [tests 100]
                     #:size [size (lambda (n)
                                    (inexact->exact (round (log n 2))))]
                     #:deadline [deadline (+ (current-inexact-milliseconds) (* 60 1000))]
                     #:prop-run-start [run-start (current-inexact-milliseconds)]
                     #:tyche [tyche #f]
                     #:features [features (list)]
                     )
  (config seed tests size deadline run-start tyche features))

(define tyche-port (open-output-file "tyche-log.jsonl" #:exists 'truncate))

(module+ private
  (provide (struct-out config)))


;; result ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(struct result (config prop labels tests-run status args args/smallest e time time/smallest)
  #:transparent)

(define (make-result config prop labels tests-run status [args #f] [args/smallest #f] [exception #f] [time #f] [time/shrink #f]) ;; noqa
  (result config prop labels tests-run status args args/smallest exception time time/shrink))

(module+ private
  (provide (struct-out result)))


;; check ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide
 (contract-out
  [label! (-> (or/c #f string?) void?)]))

(define current-labels
  (make-parameter #f))

(define (label! s)
  (define labels (current-labels))
  (when (and s labels)
    (hash-update! labels s add1 0)))

(define (print-features features rep)
  (for/hasheq ([pair features])
    (match pair
      [(cons name f)
       (values (string->symbol name) (apply f rep))])))

(define (tyche-log p-name prop-run-start run-total status rep features)
  (define tyche-args
    (hasheq
     'type "test_case"
     'property (~a p-name)
     'run_start prop-run-start
     'status (if status "passed" "failed")
     'status_reason ""
     'representation (~v (car rep))
     'arguments (hasheq)
     'how_generated ""
     'timing (hasheq 'time run-total)
     'metadata (hasheq)
     'coverage (hasheq)
     'features (print-features features rep))) 
  (write-json tyche-args tyche-port)
  (newline tyche-port))


(define (check c p)
  (define caller-rng (current-pseudo-random-generator))
  (define rng (make-pseudo-random-generator))
  (parameterize ([current-labels (make-hash)]
                 [current-pseudo-random-generator rng])
    (match-define (config seed tests size deadline prop-run-start tyche features) c)
    (match-define (prop name _arg-ids g f) p)
    (if not tyche (when (file-exists? "tyche-log.json") (delete-file "tyche-log.json")))

    (define exn? #f)
    (define (pass? args gen-start)
      (let-values 
        ([(run-start status run-end) 
            (values (current-inexact-monotonic-milliseconds)
                    (with-handlers ([(lambda (_) #t)
                       (lambda (the-exn)
                         (begin0 #f
                                 (set! exn? the-exn)))])
                      (parameterize ([current-pseudo-random-generator caller-rng])
                      (apply f args))) 
                    (current-inexact-monotonic-milliseconds))])
        (if tyche
          (begin (tyche-log name prop-run-start (/ (- run-end gen-start) 1000) status args features) status)
           status)))

    (define (descend-shrinks trees last-failing-value gen-start)
      (cond
        [(stream-empty? trees) last-failing-value]
        [else
         (define tree (stream-first trees))
         (define value (shrink-tree-val tree))
         (if (pass? value gen-start)
             (descend-shrinks (stream-rest trees) last-failing-value gen-start)
             (descend-shrinks (shrink-tree-shrinks (stream-first trees)) value gen-start))]))

    (random-seed seed)
    (define start (current-inexact-monotonic-milliseconds))
    (let loop ([test 0])
      (cond
        [(= test tests)
         (make-result c p (current-labels) test 'passed)]

        [(>= (current-inexact-monotonic-milliseconds) deadline)
         (make-result c p (current-labels) (add1 test) 'timed-out)]

        [else
         (define gen-start (current-inexact-monotonic-milliseconds))
         (define tree (g rng (size (add1 test))))
         (define value (shrink-tree-val tree))
         (cond
           [(pass? value gen-start)
            (loop (add1 test))]

           [else
            (define start/shrink (current-inexact-monotonic-milliseconds))
            (define shrunk?
              (parameterize ([current-labels #f])
                (descend-shrinks (shrink-tree-shrinks tree) #f gen-start)))
            (define end/shrink (current-inexact-monotonic-milliseconds))
            (make-result c p (current-labels) (add1 test) 'falsified value shrunk? exn? (- start/shrink start) (- end/shrink start/shrink))])]))))

         (module+ private
           (provide
            (contract-out
             [check (-> config? prop? result?)])))

         (module+ test
           (require (prefix-in ru: rackunit))

           (define-syntax-rule (check-status r s)
             (ru:check-equal? (result-status r) s))

           (check-status
            (check (make-config) prop-addition-commutes)
            'passed)

           (check-status
            (check (make-config) prop-addition-is-multiplication)
            'falsified))


         ;; common ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

         (define (make-random-seed)
           (modulo
            (for/fold ([n 0])
                      ([b (in-list (bytes->list (crypto-random-bytes 8)))])
              (arithmetic-shift (+ n b) 8))
            (expt 2 31)))