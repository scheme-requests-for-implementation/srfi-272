; SPDX-FileCopyrightText: 2026 Sergei Egorov
;
; SPDX-License-Identifier: MIT

; Bare-bones Pretty Printing library, for Guile
;
; Guile's own pretty printer lives in (ice-9 pretty-print), and it satisfies
; this library as it stands: it takes the object and an optional port, lays
; the output out within a fixed width of 79 columns, ends it with a newline,
; and its output reads back equal for anything that write could round-trip.
; So this library is that procedure under this SRFI's name.  Guile does not
; bind pp itself, so nothing a Guile user relies on is shadowed by importing
; it, and (ice-9 pretty-print) keeps working alongside.
;
; Guile's printer does terminate on circular structure, but it marks the
; back reference as #-1#, which its own reader cannot read.  This library
; asks for no more: it guarantees termination for nothing circular, and
; machine readability only where write and read already round-trip, which
; for circular structure they do not on Guile.
;
; The richer libraries cannot be had this way, since Guile's printer takes
; no parameter beyond its width and cannot mark shared or circular structure
; with datum labels at all; those come from the portable implementation.
; See (srfi-272 basic) and up -- spelled with a hyphen, because Guile maps
; every (srfi N x ...) name onto the single module (srfi srfi-N), so the
; five libraries of this SRFI would otherwise collide.  This one keeps the
; name the SRFI gives it, and lives in srfi/srfi-272.sld to match the map.

(define-library (srfi 272)
  (import (only (ice-9 pretty-print) pretty-print))
  (export (rename (pretty-print pp))))
