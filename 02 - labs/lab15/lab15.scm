; ex. 4.55

(supervisor ?who (Bitdiddle Ben))

(job ?x (accounting ?type))

(address ?who (Slumerville ?road ?number))

; note that it is up to the user to use "primary key" 
; to retrieve fields that don't distinguish a record

; ex. 4.62

(assert! (rule (last-pair (?x . ()) ?x)))
(assert! (rule (last-pair (?x . ?y) ?z)
  (last-pair ?y ?z)))

; last-pair only defined for non-empty lists
(assert! (1 2 3 4))
(assert! (2 3 4))
(assert! (4 3 2))

; note that assert! must be used in driver-loop 
; or rules and records must be included in 
; database variable

; for (last-pair ?x (3)), there are an infinite number of satisfying lists; 
; the query does not finish


