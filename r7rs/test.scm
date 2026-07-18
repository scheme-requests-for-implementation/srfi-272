; SPDX-FileCopyrightText: 2026 Sergei Egorov
;
; SPDX-License-Identifier: MIT

(import (scheme base) (scheme read) (scheme write))
(import (srfi 272))

(define (pp-test input expected)
  (let ((p (open-output-string)))
    (pp (read (open-input-string input)) p)
    (let ((actual (get-output-string p)))
      (if (string=? actual expected)
          (begin (display "OK: ") (display expected))
          (begin
            (display "FAILED: ")
            (write input)
            (newline)
            (display "EXPECTED:")
            (newline)
            (display expected)
            (newline)
            (display "ACTUAL:")
            (newline)
            (display actual)
            (newline))))))

(display "Running tests")
(newline)

(pp-test "1" "1\n")
(pp-test "'(a b)" "'(a b)\n")
(pp-test "'(a . b)" "'(a . b)\n")
(pp-test "`(,a ,@b)" "`(,a ,@b)\n")
(pp-test
  "(let ((x 1) (y 2) (zzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzz 3)) (display x) (display y))"
  "(let\n ((x 1) (y 2) (zzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzz 3))\n (display x)\n (display y))\n")

(display "Done.")
(newline)
(newline)

; example

(define sexp1
  '(define
    (ast-pretty a . opt-port)
    (define
     (width x)
     (cond
      ((string? x) (+ (string-length x) 2))
      ((char? x) 3)
      ((number? x) 5)
      ((symbol? x) (string-length (symbol->string x)))
      ((boolean? x) 2)
      ((null? x) 2)
      (else 10)))
    (define
     (ast-width a)
     (cond
      ((ast-atom? a) (width (ast-atom->val a)))
      ((ast-null? a) 2)
      ((ast-pair? a)
       (if
        (ast-list? a)
        (let
         loop
         ((a a) (sum 0) (count 0))
         (if
          (ast-null? a)
          (+ sum count 1)
          (loop
           (ast-cdr a)
           (+ sum (ast-width (ast-car a)))
           (+ count 1))))
        (+ 1 (ast-width (ast-car a)) 3 (ast-width (ast-cdr a)) 1)))
      (else 0)))
    (define
     (ast-width-hello a)
     (display "hello!\n")
     (cond
      ((ast-atom? a) (width (ast-atom->val a)))
      ((ast-null? a) 2)
      ((ast-pair? a)
       (if
        (ast-list? a)
        (let
         loop
         ((a a) (sum 0) (count 0))
         (if
          (ast-null? a)
          (+ sum count 1)
          (loop
           (ast-cdr a)
           (+ sum (ast-width (ast-car a)))
           (+ count 1))))
        (+ 1 (ast-width (ast-car a)) 3 (ast-width (ast-cdr a)) 1)))
      (else
       (cond
        ((foo) => (lambda (v) (bar v 0)))
        (else (begin (newline) (if a #t #f)))))))
    (define
     (break? cur-col width max-col)
     (> (+ cur-col width) max-col))
    (define
     (print-indent n port)
     (do ((i 0 (+ i 1))) ((= i n)) (display "  " port)))
    (define
     (pretty a col indent port max-col)
     (cond
      ((ast-atom? a)
       (let
        ((w (width (ast-atom->val a))))
        (write (ast-atom->val a) port)
        (+ col w)))
      ((ast-null? a) (display "()" port) (+ col 2))
      ((ast-pair? a)
       (if
        (ast-list? a)
        (let*
         ((total-width (ast-width a))
          (remaining (- max-col col))
          (inline? (<= total-width remaining)))
         (if
          inline?
          (begin
           (display "(" port)
           (let
            loop
            ((first? #t) (rest a) (cur-col (+ col 1)))
            (if
             (ast-null? rest)
             (begin (display ")" port) (+ cur-col 1))
             (let*
              ((elem-width (ast-width (ast-car rest)))
               (new-col
                (if
                 first?
                 (pretty (ast-car rest) cur-col indent port max-col)
                 (begin
                  (display " " port)
                  (pretty (ast-car rest) (+ cur-col 1) indent port max-col)))))
              (loop #f (ast-cdr rest) new-col)))))
          (begin
           (display "(" port)
           (let
            ((new-indent (+ indent 1)))
            (let
             loop
             ((rest a) (cur-col (* new-indent 2)))
             (if
              (ast-null? rest)
              (begin (display ")" port) (+ cur-col 1))
              (let*
               ((elem (ast-car rest))
                (next-col (pretty elem cur-col new-indent port max-col)))
               (unless
                (ast-null? (ast-cdr rest))
                (newline port)
                (print-indent new-indent port))
               (loop (ast-cdr rest) (* new-indent 2)))))))))
        (begin
         (display "(" port)
         (let
          ((car-col (pretty (ast-car a) (+ col 1) indent port max-col)))
          (display " . " port)
          (let
           ((cdr-col
             (pretty (ast-cdr a) (+ car-col 3) indent port max-col)))
           (display ")" port)
           (+ cdr-col 1))))))))
    (let
     ((port
       (if (null? opt-port) (current-output-port) (car opt-port))))
     (pretty a 0 0 port 80)
     (newline port))))


(pp sexp1)

