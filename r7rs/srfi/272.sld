; SPDX-FileCopyrightText: 2026 Sergei Egorov
;
; SPDX-License-Identifier: MIT

; Bare-bones Pretty Printing library

(define-library (srfi 272)
  (import (scheme base) (scheme inexact) (scheme write))
  ; reenable box support by uncommenting #; comments
  
  ; procedures
  (export pp)
  
  (begin
    (define *width* 80)
    
    ; basic formatting operations
    
    ; guess print length of atoms -- better be fast than exact
    (define log10-of-2 2.302585092994046)
    (define (atom-width x)
      (cond ((or (null? x) (boolean? x)) 2)
            ((symbol? x) (string-length (symbol->string x)))
            ((string? x) (+ (string-length x) 2)) ; ignores escapes!
            ((and (char? x) (char<=? #\! x #\~)) 3)
            ((and (integer? x) (exact? x)) ; don't print bignums
             (cond ((<= 0 x 9) 1)
                   ((<= -9 x 99) 2)
                   ((<= -99 x 999) 3)
                   ((> x 0) (exact (ceiling (/ (log (+ x 0.1)) log10-of-2))))
                   (else
                    (+ 1 (exact (ceiling (/ (log (- 0.1 x)) log10-of-2)))))))
            ((and (rational? x) (exact? x))
             (+ (atom-width (numerator x)) 1 (atom-width (denominator x))))
            ((memv x '((#\tab . 5) (#\newline . 9) (#\space . 7))) => cdr)
            (else
             (let* ((p (open-output-string)) ; slow but exact
                    (s (begin (write x p) (get-output-string p))))
               (close-output-port p)
               (string-length s)))))
    
    ; indentation ind is either an exact nonnegative integer or #f meaning 
    ; "we are printing inline code, so it doesn't matter"
    
    ; safe increment for indentation (handles #f)
    (define (ind+ i n) (and i (+ i n)))
    
    ; inserts single space if ind is #f; otherwise prints newline and indents to ind
    (define (space ind p)
      (cond ((not ind) (write-char #\space p))
            (else
             (newline p)
             (do ((i 0 (+ i 1))) ((>= i ind)) (write-char #\space p)))))
    
    (define (abbrev? x)
      (and (pair? x) (pair? (cdr x)) (null? (cddr x))
           (memq (car x) '(quote quasiquote unquote unquote-splicing))))
    
    ; output primitives
    (define (lpar p) (write-char #\( p))
    (define (rpar p) (write-char #\) p))
    
    ; we calculate width on S-exps directly; this is far from exact,
    ; but should do for our purposes. Stops early if cap is reached,
    ; returning a value larger than cap. Doing it this way helps to
    ; keep width calculations linear by input size
    ; This version of fit-ind does not use call/cc
    (define (fit-ind x ind)
      (define (make-cnt ind) (list (- *width* ind)))
      (define cnt-val car)
      (define cnt-set! set-car!)
      (define (cnt-zero? cnt) (<= (cnt-val cnt) 0))
      (define (cnt-sub cnt val)
        (>= (begin (cnt-set! cnt (- (cnt-val cnt) val)) (cnt-val cnt)) 0))
      (define (fits? x cnt)
        (cond ((abbrev? x)
               (and (cnt-sub cnt (case (car x) ((unquote-splicing) 2) (else 1)))
                    (fits? (cadr x) cnt)))
              ((pair? x)
               (let ((h (car x)) (t (cdr x)))
                 (cond ((null? t) (and (cnt-sub cnt 2) (fits? h cnt)))
                       ((pair? t)
                        (and (cnt-sub cnt 1) (fits? h cnt) (fits? t cnt)))
                       (else (and (cnt-sub cnt 5) (fits? h cnt) (fits? t cnt))))))
              ((vector? x)
               (let ((vlen (- (vector-length x) 1)))
                 (let loop ((i 0))
                   (if (>= i vlen)
                       (cnt-sub cnt 3)
                       (and (fits? (vector-ref x i) cnt) (loop (+ i 1)))))))
              (else (cnt-sub cnt (atom-width x)))))
      (if (or (not ind) (fits? x (make-cnt ind))) #f ind))
    
    (define (print-abbrev x ind p)
      (let ((abr (car x)) (arg (cadr x)))
        (case abr
          ((quote) (write-char #\' p))
          ((quasiquote) (write-char #\` p))
          ((unquote) (write-char #\, p))
          ((unquote-splicing) (display ",@" p)))
        (let
          ((ind (ind+ ind (case abr ((unquote-splicing) 2) (else 1)))))
          (print-datum arg ind p))))
    
    (define (print-pair-datum x ind p)
      (let ((ind (fit-ind x ind)))
        (lpar p)
        (do ((ind (ind+ ind 1)) (first? #t #f) (x x (cdr x)))
          ((not (pair? x))
           (unless (null? x)
             (display " . " p)
             (print-datum x (ind+ ind 3) p)))
          (unless first? (space ind p))
          (print-datum (car x) ind p))
        (rpar p)))
    
    (define (print-vector-datum x ind p)
      (let ((ind (fit-ind x ind)) (vlen (vector-length x)))
        (write-char #\# p)
        (lpar p)
        (do ((ind (ind+ ind 2)) (idx 0 (+ idx 1))) ((= idx vlen))
          (unless (zero? idx) (space ind p))
          (print-datum (vector-ref x idx) ind p))
        (rpar p)))
    
    
    (define (print-datum x ind p)
      (cond ((abbrev? x) (print-abbrev x ind p))
            ((pair? x) (print-pair-datum x ind p))
            ((vector? x) (print-vector-datum x ind p))
            (else (write x p))))
    
    (define (pp x . rest)
      (define p
        (if (and (pair? rest) (output-port? (car rest)))
            (car rest)
            (current-output-port)))
      (print-datum x 0 p)
      (newline p))))
