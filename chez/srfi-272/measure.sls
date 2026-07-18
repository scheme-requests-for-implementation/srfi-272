; SPDX-FileCopyrightText: 2026 Sergei Egorov
;
; SPDX-License-Identifier: MIT

; Char-width library: scheme analog of wcwidth()
; returns #f, 0, 1, 2

(library (srfi-272 measure)

  (export char-width-procedure)

  (import 
    (rnrs base)
    (only (chezscheme) make-parameter))
  
  (define (char-width ch) (if (char<=? #\space ch #\~) 1 #f))
    
  ; users can plug in their custom procedures
  (define char-width-procedure (make-parameter char-width)))
