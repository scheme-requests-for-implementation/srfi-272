; SPDX-FileCopyrightText: 2026 Sergei Egorov
;
; SPDX-License-Identifier: MIT

; Basic Pretty Printing library

(library (srfi-272 basic)

  (export
    ; procedures 
    pp pprint pprint-shared pprint-simple
    ; parameters
    pp-width pp-circle pp-graph)

  (import
    (srfi-272 intermediate))

)
