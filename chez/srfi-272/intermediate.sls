; SPDX-FileCopyrightText: 2026 Sergei Egorov
;
; SPDX-License-Identifier: MIT

; Intermediate Pretty Printing library

(library (srfi-272 intermediate)

  (export
    ; procedures
    pp pp* pprint pprint-shared pprint-simple pprint-file
    ; parameters
    pp-width pp-circle pp-graph pp-radix pp-length pp-level
    ; configuration
    pretty-style)

  (import
    (rnrs base) (rnrs lists) (rnrs control) (rnrs unicode)
    (rnrs mutable-pairs) (rnrs io simple) (rnrs io ports)
    (rnrs bytevectors) (rnrs exceptions) (rnrs conditions)
    (only (chezscheme)
      pretty-print pretty-file pretty-format
      pretty-initial-indent pretty-line-length
      pretty-maximum-lines pretty-one-line-limit
      pretty-standard-indent
      print-brackets print-graph print-length
      print-level print-radix
      make-parameter parameterize define-values))

  ; remap parameters to existing ones
  (define pp-width pretty-line-length)

  ; Chez has print-graph, that turns on graph printing, but no separate
  ; control of circular only printing. We make our own pp-circle to hold the flag,
  ; and catch pretty-print's warning condition manually in pp
  (define pp-graph print-graph)
  (define pp-circle (make-parameter #t))

  ; radix/length/level map directly
  (define pp-radix print-radix)
  (define pp-length print-length)
  (define pp-level print-level)

  ; map pretty-style to Chez formatting style registry accessor
  (define pretty-style pretty-format)

  (define (pp sexp . rest)

    (define-values (*port* kv*)
      (if (and (pair? rest) (output-port? (car rest)))
          (values (car rest) (cdr rest))
          ; if port is not given as optional, look for the kw
          (values (current-output-port) rest)))

    (define (print sexp)
      (guard
        (c [(warning? c)
            (if (and (not (pp-graph)) (pp-circle))
                ; silently retry with pp-graph on
                (parameterize ([pp-graph #t]) (print sexp))
                ; let it go
                (raise c))]
           [else (raise c)])
        (pretty-print sexp *port*)))

    ; the parameters this library knows how to take as keywords
    (define known (list pp-width pp-graph pp-circle pp-radix pp-length pp-level))

    ; Scanning left to right, the FIRST value given for a parameter is the one
    ; that counts, so a repeated key is skipped rather than allowed to shadow.
    ; The bindings nest, which is why the skip matters: without it the innermost
    ; -- that is, the last -- would win instead.
    (let loop ([kv* kv*] [seen '()])
      (cond [(null? kv*)
             (print sexp)]
            [(not (and (pair? (cdr kv*)) (memq (car kv*) known)))
             (error 'pp "unexpected keyword arguments" kv*)]
            [(memq (car kv*) seen)
             (loop (cddr kv*) seen)]
            [else
             (let ([param (car kv*)] [val (cadr kv*)])
               (parameterize ([param val])
                 (loop (cddr kv*) (cons param seen))))])))

  ; accepts a keyword-value list as last argument
  (define (pp* obj arg . args)
    (apply pp obj (apply cons* arg args)))

  ; The hardwired pp-graph/pp-circle come BEFORE kv*, which is what makes them
  ; win: pp takes the first value given for a parameter.
  ; overrides pp-graph/pp-circle params; will hang on cycles
  ; this one is the fastest of them all
  (define (pprint-simple obj . rest)
    (define-values (port kv*)
      (if (and (pair? rest) (output-port? (car rest)))
          (values (car rest) (cdr rest))
          (values (current-output-port) rest)))
    (pp* obj port pp-graph #f pp-circle #f kv*))

  ; overrides pp-graph/pp-circle params; only marks cycles
  ; spends time on detecting shared structures, and more on cycles
  (define (pprint obj . rest)
    (define-values (port kv*)
      (if (and (pair? rest) (output-port? (car rest)))
          (values (car rest) (cdr rest))
          (values (current-output-port) rest)))
    (pp* obj port pp-graph #f pp-circle #t kv*))

  ; overrides pp-graph/pp-circle param; marks all shared
  ; this one is actually faster than pprint
  (define (pprint-shared obj . rest)
    (define-values (port kv*)
      (if (and (pair? rest) (output-port? (car rest)))
          (values (car rest) (cdr rest))
          (values (current-output-port) rest)))
    (pp* obj port pp-graph #t pp-circle #t kv*))

  (define (pprint-file ifn . rest)
    (define-values (ofn kv*)
      (if (and (pair? rest) (string? (car rest)))
          (values (car rest) (cdr rest))
          (values #f rest)))
    (define (pf ip op)
      (let loop ((obj (read ip)))
        (unless (eof-object? obj)
          (pp* obj op kv*)
          (newline op)
          (loop (read ip)))))
    (call-with-input-file ifn
      (lambda (ip)
        (if (not ofn)
            (pf ip (current-output-port))
            (call-with-output-file ofn (lambda (op) (pf ip op)))))))

)
