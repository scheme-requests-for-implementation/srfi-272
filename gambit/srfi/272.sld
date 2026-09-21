; SPDX-FileCopyrightText: 2026 Sergei Egorov
;
; SPDX-License-Identifier: MIT

; Bare-bones Pretty Printing library, for Gambit
;
; Gambit already binds pp, and it happens to satisfy this library as it stands:
; it takes the object and an optional port, lays the output out within the port's
; width, ends it with a newline, and its output reads back equal for anything
; that write could round-trip.  It prints circular structure with datum labels
; rather than diverging, which is more than this library asks for.
;
; Given a procedure it prints the source the procedure was compiled from.  This
; library says nothing about printing procedures -- the machine-readability rule
; is conditional on write and read round-tripping, which no procedure does -- so
; that behaviour is left exactly as Gambit's users already know it.
;
; The richer libraries cannot be had this way, since Gambit's pp takes no
; parameters; those come from the portable implementation, which recovers the
; decompiled source for itself.  See (srfi 272 basic) and up.

(define-library (srfi 272)
  (import (only (gambit) pp))
  (export pp))
