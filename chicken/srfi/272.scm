; SPDX-FileCopyrightText: 2026 Sergei Egorov
;
; SPDX-License-Identifier: MIT

; Bare-bones Pretty Printing library, for CHICKEN
;
; CHICKEN's own pretty printer lives in (chicken pretty-print), where pp is
; an alias for pretty-print, and it satisfies this library as it stands: it
; takes the object and an optional port, lays the output out within the
; width held by the pretty-print-width parameter, ends it with a newline,
; and its output reads back equal for anything that write could round-trip,
; abbreviating quote and its relatives on the way.  So this library is that
; procedure under this SRFI's name.  A program that has not imported
; (chicken pretty-print) has no pp of its own, so nothing is shadowed.
;
; CHICKEN's printer does not terminate on circular structure.  This library
; asks for no more: it guarantees termination for nothing circular.
;
; The richer libraries cannot be had this way, since CHICKEN's printer takes
; no parameter beyond its width and cannot mark shared or circular structure
; with datum labels; those come from the portable implementation.  They do
; keep the width, though: their pp-width is pretty-print-width itself, so
; setting either moves both.  See (srfi 272 basic) and up.

(define-library (srfi 272)
  (import (only (chicken pretty-print) pp))
  (export pp))
