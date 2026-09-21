; SPDX-FileCopyrightText: 2026 Sergei Egorov
;
; SPDX-License-Identifier: MIT

; Datum labels for Guile's reader -- test support, not part of the SRFI.
;
; Guile's write-shared emits #N= and #N#, but its reader cannot read them
; back: #0= is taken for the start of a rank-0 array.  The test files feed
; cyclic and shared data to the printer by reading it from a string, so they
; need a reader that understands labels.  install-datum-labels! teaches the
; one global reader about them via read-hash-extend; after the call, plain
; read accepts #N= and #N#.
;
; A reference inside its own label (#0=(1 . #0#)) cannot be resolved while the
; datum is still being read, so #N# yields a placeholder and the structure is
; patched in place once the labelled datum is complete.
;
; The label table is not cleared between top-level data: a #N# whose #N= was
; defined by an earlier read resolves to that earlier datum instead of
; failing.  Every test defines a label before using it, so this never shows.

(define-library (datum-labels)
  (import (scheme base) (scheme char)
          (only (guile) read-hash-extend read))
  (export install-datum-labels!)
  (begin

    (define labels '())

    (define (label-ref n)
      (let ((p (assv n labels)))
        (if p (cdr p) (error "undefined datum label" n))))

    (define (label-set! n v)
      (let ((p (assv n labels)))
        (if p
            (set-cdr! p v)
            (set! labels (cons (cons n v) labels)))))

    ; A placeholder is a fresh pair, so eq? tells it from any real datum.
    (define (make-placeholder n) (list 'datum-label n))

    (define (patch! obj ph val)
      (let ((seen '()))
        (let walk ((x obj))
          (cond
            ((pair? x)
             (if (memq x seen)
                 #f
                 (begin
                   (set! seen (cons x seen))
                   (if (eq? (car x) ph) (set-car! x val) (walk (car x)))
                   (if (eq? (cdr x) ph) (set-cdr! x val) (walk (cdr x))))))
            ((vector? x)
             (if (memq x seen)
                 #f
                 (begin
                   (set! seen (cons x seen))
                   (let loop ((i 0))
                     (if (< i (vector-length x))
                         (begin
                           (if (eq? (vector-ref x i) ph)
                               (vector-set! x i val)
                               (walk (vector-ref x i)))
                           (loop (+ i 1))))))))
            (else #f)))))

    (define (read-digits c port)
      (let loop ((n (- (char->integer c) 48)))
        (let ((k (peek-char port)))
          (if (and (char? k) (char-numeric? k))
              (loop (+ (* n 10) (- (char->integer (read-char port)) 48)))
              n))))

    (define (hash-digit-handler c port)
      (let* ((n (read-digits c port))
             (k (read-char port)))
        (cond
          ((eqv? k #\#) (label-ref n))
          ((eqv? k #\=)
           (let ((ph (make-placeholder n)))
             (label-set! n ph)
             (let ((datum (read port)))
               (label-set! n datum)
               (patch! datum ph datum)
               datum)))
          (else (error "malformed datum label" n k)))))

    (define (install-datum-labels!)
      (let loop ((i 0))
        (if (< i 10)
            (begin
              (read-hash-extend (integer->char (+ 48 i)) hash-digit-handler)
              (loop (+ i 1))))))))
