; SPDX-FileCopyrightText: 2026 Sergei Egorov
;
; SPDX-License-Identifier: MIT

; Bare-bones Pretty Printing library, for Gauche
;
; Gauche's own pretty printer is pprint, in the gauche module, and it meets
; this library's requirements as it stands: object plus destination, output
; laid out within a width, a terminating newline, quote and its relatives
; abbreviated, and output that reads back equal wherever write round-trips.
; Gauche binds no pp of its own, so nothing is shadowed.
;
; What pprint does not take is a positional port: it wants :port.  So this
; pp accepts the port either way, and passes every other Gauche keyword
; through untouched, which is what makes it an upgrade rather than a
; replacement -- code written against Gauche's pprint keeps working when it
; is called through this SRFI's name, and code written against this SRFI
; works on Gauche.  The richer libraries widen the same bargain; see
; (srfi 272 basic) and up.
;
; Gauche's printer does not terminate on circular structure.  This library
; asks for no more: it guarantees termination for nothing circular.
;
; A Gauche programmer who prefers use to import needs nothing extra: this
; library's define-library form makes the module srfi.272, so (use srfi.272)
; reaches it, and (use srfi.272.basic) reaches the next one up.  Only the
; older hyphenated spelling, (use srfi-272), would need a wrapper module.

(define-library (srfi 272)
  (import (scheme base)
          (only (gauche base) pprint keyword? :port))
  (export pp)
  (begin

    ; obj, then an optional positional port, then Gauche keyword arguments
    (define (pp obj . rest)
      (if (and (pair? rest) (not (keyword? (car rest))))
          (apply pprint obj :port (car rest) (cdr rest))
          (apply pprint obj rest)))))
