; SPDX-FileCopyrightText: 2026 Sergei Egorov
;
; SPDX-License-Identifier: MIT

; Bare-bones Pretty Printing library

(library (srfi-272)

  (export
    ; procedures 
    pp)

  (import
    (rnrs base)
    (only (chezscheme) pretty-print))

  (define pp pretty-print)
)
