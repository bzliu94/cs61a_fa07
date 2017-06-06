; 1.

; ex. 2.74

; methods that are used with apply-generic 
; should have type that is a list, the reason for which 
; is tied to the idea that parameter args 
; is always a list

(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (error
            "No method for these types -- APPLY-GENERIC"
            (list op type-tags))))))

; everything we have a constructor for we need a tag

; first division is computer division
(define (make-d1-name first-name middle-initial last-name)
  ((get 'make-d1-name 'division1) first-name middle-initial last-name))
(define (make-d1-salary value)
  ((get 'make-d1-salary 'division1) value))
(define (make-d1-address city road number)
  ((get 'make-d1-address 'division1) city road number))
(define (make-d1-record name salary address)
  ((get 'make-d1-record 'division1) name salary address))
(define (make-d1-personnel-file)
  ((get 'make-d1-personnel-file 'division1)))
(define (add-d1-record personnel-file record)
  ((get 'add-d1-record 'division1) personnel-file record))

; second division is accounting division
(define (make-d2-name first-name middle-initial last-name)
  ((get 'make-d2-name 'division2) first-name middle-initial last-name))
(define (make-d2-salary value)
  ((get 'make-d2-salary 'division2) value))
(define (make-d2-address city road number)
  ((get 'make-d2-address 'division2) city road number))
(define (make-d2-record name salary address)
  ((get 'make-d2-record 'division2) name salary address))
(define (make-d2-personnel-file)
  ((get 'make-d2-personnel-file 'division2)))
(define (add-d2-record personnel-file record)
  ((get 'add-d2-record 'division2) personnel-file record))

(define (get-name record) (apply-generic 'get-name record))
(define (get-salary record) (apply-generic 'get-salary record))
(define (get-salary-value salary) (apply-generic 'get-salary-value salary))
(define (get-address record) (apply-generic 'get-address record))
(define (get-first-name name) (apply-generic 'get-first-name name))
(define (get-middle-initial name) (apply-generic 'get-middle-initial name))
(define (get-last-name name) (apply-generic 'get-last-name name))
(define (get-city address) (apply-generic 'get-city address))
(define (get-road address) (apply-generic 'get-road address))
(define (get-number address) (apply-generic 'get-number address))
(define (get-record personnel-file name)
  (apply-generic 'get-record personnel-file name))
(define (get-records personnel-file name)
  (apply-generic 'get-records personnel-file name))
(define (get-make-name personnel-file)
  (apply-generic 'get-make-name personnel-file))

; we make explicitly clear that when looking for a record 
; given a name and a list of personnel files 
; that we look through all given personnel files, 
; as opposed to spending O(1) time to find a record

(define (install-division1-package)
  ;; internal procedures
  ; division-specific constructors for deep record
  (define (make-d1-name first-name middle-initial last-name)
    (list first-name middle-initial last-name))
  (define (make-d1-salary value)
    (list value))
  (define (make-d1-address city road number)
    (list city road number))
  (define (make-d1-record name salary address)
    (list name salary address))
  ; division-specific constructor/mutator for personnel file
  (define (make-d1-personnel-file) nil)
  ; as we are a mutator, we have already called constructor; 
  ; we don't need to tag again; 
  ; as we are post-constructor, we also assume we have tag at top-level
  (define (add-d1-record personnel-file record)
    ; we assume tags are present; 
    ; the prize is that adding takes constant time
    (cons 'division1 (cons record (cdr personnel-file))))
  ; getters for deep record
  (define (get-name record) (list-ref record 0))
  (define (get-salary record) (list-ref record 1))
  (define (get-salary-value salary) (list-ref salary 0))
  (define (get-address record) (list-ref record 2))
  (define (get-first-name name) (list-ref name 0))
  (define (get-middle-initial name) (list-ref name 1))
  (define (get-last-name name) (list-ref name 2))
  (define (get-city address) (list-ref address 0))
  (define (get-road address) (list-ref address 1))
  (define (get-number address) (list-ref address 2))
  ; getter for personnel file
  ; for list-implemented version of personnel record; 
  ; nil is considered a special value for key
  (define (key record) 
    (if (equal? (list? record) #f)
      nil
      ; need to trim twice - for before and for after
      (let ((result (lookup-trim-record (get-name (lookup-trim-record record)))))
        result)))
  ; note that removal of tags is shallow
  (define (lookup given-key set-of-records do-stop-at-first-match)
    (lookup-helper given-key set-of-records do-stop-at-first-match))
  (define (lookup-trim-record record)
    (if (equal? (car record) 'division1)
      (cdr record)
      record))
  ; this is modified; 
  ; also, we return a list
  (define (lookup-helper given-key set-of-records do-stop-at-first-match)
    (if (equal? set-of-records nil)
      nil
      (let ((curr-record (car set-of-records))
            (rest (cdr set-of-records)))
        (let ((keys-do-match (equal? given-key (key curr-record))))
          (if (equal? keys-do-match #t)
            (if (equal? do-stop-at-first-match #t)
              (list curr-record)
              (cons curr-record (lookup-helper given-key rest do-stop-at-first-match)))
            (lookup-helper given-key rest do-stop-at-first-match))))))
  ; getter/setter for personnel file
  ; for list-implemented version of personnel record
  (define (get-record personnel-file name)
    (let ((result (lookup name personnel-file #t)))
      (if (equal? result nil)
        #f
        (car result))))
  (define (get-records personnel-file name)
    (let ((result (lookup name personnel-file #f)))
      (if (equal? result nil)
        #f
        result)))

  ;; interface to the rest of the system
  (define (tag x) (attach-tag 'division1 x))
  ; division-specific constructors for deep record
  (put 'make-d1-name 'division1
    (lambda (first-name middle-initial last-name)
      (tag (make-d1-name first-name middle-initial last-name))))
  (put 'make-d1-salary 'division1
    (lambda (value)
      (tag (make-d1-salary value))))
  (put 'make-d1-address 'division1
    (lambda (city road number)
      (tag (make-d1-address city road number))))
  (put 'make-d1-record 'division1
    (lambda (name salary address)
      (tag (make-d1-record name salary address))))
  ; division-specific constructor/mutator for personnel file
  (put 'make-d1-personnel-file 'division1
    (lambda () 
      (tag (make-d1-personnel-file))))
  (put 'add-d1-record 'division1
    (lambda (personnel-file record)
      (add-d1-record personnel-file record)))
  ; setter for personnel file
  ; getters for deep record
  (put 'get-name '(division1) get-name)
  (put 'get-salary '(division1) get-salary)
  (put 'get-salary-value '(division1) get-salary-value)
  (put 'get-address '(division1) get-address)
  (put 'get-first-name '(division1) get-first-name)
  (put 'get-middle-initial '(division1) get-middle-initial)
  (put 'get-last-name '(division1) get-last-name)
  (put 'get-city '(division1) get-city)
  (put 'get-road '(division1) get-road)
  (put 'get-number '(division1) get-number)
  ; getter/setter for personnel file
  ; for list-implemented version of personnel record
  (put 'get-record '(division1 division1) get-record)
  (put 'get-records '(division1 division1) get-records)
  ; to make looking through multiple divisions for a name easier
  (put 'get-make-name '(division1) (lambda (pf) 
    (lambda (a b c) (tag (make-d1-name a b c)))))

  ;; install records
  (define our-personnel-file (install-division1-package-records))

  our-personnel-file)

; called as part of main install
(define (install-division1-package-records)

  ;; install records
  (define our-personnel-file (make-d1-personnel-file))
  ; first record for alyssa
  (define name1 (make-d1-name "Alyssa" "P" "Hacker"))
  (define salary1 (make-d1-salary 40000))
  (define address1 (make-d1-address "Cambridge" "Mass Ave" "78"))
  (define record1 (make-d1-record name1 salary1 address1))
  (set! our-personnel-file (add-d1-record our-personnel-file record1))
  ; second record for ben
  (define name2 (make-d1-name "Ben" "" "Bitdiddle"))
  (define salary2 (make-d1-salary 60000))
  (define address2 (make-d1-address "Slumerville" "Ridge Road" "10"))
  (define record2 (make-d1-record name2 salary2 address2))
  (set! our-personnel-file (add-d1-record our-personnel-file record2))

  our-personnel-file)

(define (install-division2-package)
  ;; internal procedures
  ; division-specific constructors for deep record
  (define (make-d2-name first-name middle-initial last-name)
    (list middle-initial last-name first-name))
  (define (make-d2-salary value)
    (list value))
  (define (make-d2-address city road number)
    (list road number city))
  (define (make-d2-record name salary address)
    (list name address salary))
  ; division-specific constructor/mutator for personnel file
  (define (make-d2-personnel-file)
    (make-dict))
  ; as we are a mutator, we have already called constructor; 
  ; we don't need to tag again; 
  ; as we are post-constructor, we also assume we have tag at top-level
  (define (add-d2-record personnel-file record)
    ; we assume tags are present; 
    ; the prize is that adding takes constant time
    (begin
      (dict-put! (cdr personnel-file) (key record) record)
      personnel-file))
  ; getters for deep record
  (define (get-name record) (list-ref record 0))
  (define (get-salary record) (list-ref record 2))
  (define (get-salary-value salary) (list-ref salary 0))
  (define (get-address record) (list-ref record 1))
  (define (get-first-name name) (list-ref name 2))
  (define (get-middle-initial name) (list-ref name 0))
  (define (get-last-name name) (list-ref name 1))
  (define (get-city address) (list-ref address 2))
  (define (get-road address) (list-ref address 0))
  (define (get-number address) (list-ref address 1))
  ; getter for personnel file
  ; for list-implemented version of personnel record; 
  ; nil is considered a special value for key
  (define (key record) 
    (if (equal? (list? record) #f)
      nil
      ; need to trim twice - for before and for after
      (let ((result (lookup-trim-record (get-name (lookup-trim-record record)))))
        result)))
  ; note that removal of tags is shallow
  (define (lookup given-key set-of-records)
    (dict-get set-of-records given-key #f))
  (define (lookup-trim-record record)
    (if (equal? (car record) 'division2)
      (cdr record)
      record))
  ; getter/setter for personnel file
  ; for list-implemented version of personnel record
  (define (get-record personnel-file name)
    (let ((result (lookup name personnel-file)))
      (if (equal? result #f)
        #f
        (car result))))
  (define (get-records personnel-file name)
    (lookup name personnel-file))

  ;; interface to the rest of the system
  (define (tag x) (attach-tag 'division2 x))
  ; division-specific constructors for deep record
  (put 'make-d2-name 'division2
    (lambda (first-name middle-initial last-name)
      (tag (make-d2-name first-name middle-initial last-name))))
  (put 'make-d2-salary 'division2
    (lambda (value)
      (tag (make-d2-salary value))))
  (put 'make-d2-address 'division2
    (lambda (city road number)
      (tag (make-d2-address city road number))))
  (put 'make-d2-record 'division2
    (lambda (name salary address)
      (tag (make-d2-record name salary address))))
  ; division-specific constructor/mutator for personnel file
  (put 'make-d2-personnel-file 'division2
    (lambda () 
      (tag (make-d2-personnel-file))))
  (put 'add-d2-record 'division2
    (lambda (personnel-file record)
      (add-d2-record personnel-file record)))
  ; setter for personnel file
  ; getters for deep record
  (put 'get-name '(division2) get-name)
  (put 'get-salary '(division2) get-salary)
  (put 'get-salary-value '(division2) get-salary-value)
  (put 'get-address '(division2) get-address)
  (put 'get-first-name '(division2) get-first-name)
  (put 'get-middle-initial '(division2) get-middle-initial)
  (put 'get-last-name '(division2) get-last-name)
  (put 'get-city '(division2) get-city)
  (put 'get-road '(division2) get-road)
  (put 'get-number '(division2) get-number)
  ; getter/setter for personnel file
  ; for list-implemented version of personnel record
  (put 'get-record '(division2 division2) get-record)
  (put 'get-records '(division2 division2) get-records)
  ; to make looking through multiple divisions for a name easier
  (put 'get-make-name '(division2) (lambda (pf) 
    (lambda (a b c) (tag (make-d2-name a b c)))))

  ;; install records
  (define our-personnel-file (install-division2-package-records))

  our-personnel-file)

; called as part of main install
(define (install-division2-package-records)

  ;; install records
  (define our-personnel-file (make-d2-personnel-file))
  ; first record for eben
  (define name1 (make-d2-name "Eben" "" "Scrooge"))
  (define salary1 (make-d2-salary 75000))
  (define address1 (make-d2-address "Weston" "Shady Lane" "10"))
  (define record1 (make-d2-record name1 salary1 address1))
  (set! our-personnel-file (add-d2-record our-personnel-file record1))
  ; second record for robert
  (define name2 (make-d2-name "Robert" "" "Cratchet"))
  (define salary2 (make-d2-salary 18000))
  (define address2 (make-d2-address "Allston" "N Harvard Street" "16"))
  (define record2 (make-d2-record name2 salary2 address2))
  (set! our-personnel-file (add-d2-record our-personnel-file record2))
  ; third record for alyssa
  (define name3 (make-d2-name "Alyssa" "P" "Hacker"))
  (define salary3 (make-d2-salary 40000))
  (define address3 (make-d2-address "Cambridge" "Mass Ave" "78"))
  (define record3 (make-d2-record name3 salary3 address3))
  (set! our-personnel-file (add-d2-record our-personnel-file record3))

  our-personnel-file)

; dictionary methods

(define (make-dict)
  (make-hash-table equal?))

(define (dict-put! dict key value)
  (let ((result (hash-table-get dict key #f)))
    (let ((key-is-present (not (equal? result #f))))
      (let ((next-value
        (if (equal? key-is-present #t)
          (cons value result)
          (list value))))
        (hash-table-put! dict key next-value)))))

; returns a list
(define (dict-get dict key default)
  (hash-table-get dict key default))

; note that we haven't dealt with dict-remove, 
; which would have to know to clean up lists 
; if number of values for a key becomes zero

(define (find-employee-record first-name middle-initial last-name personnel-files)
  (let ((result (find-employee-records first-name middle-initial 
          last-name personnel-files #t)))
    (if (equal? result nil)
      #f
      result)))

; we assume that there can be many matches for a name for a given personnel file

(define (find-employee-records first-name middle-initial last-name 
			       personnel-files do-stop-at-first-match)
  (if (equal? personnel-files nil)
      nil
      (let ((pf (car personnel-files))
            (rest (cdr personnel-files)))
        (let ((make-name (get-make-name pf)))
          (let ((name (make-name first-name middle-initial last-name)))
            (let ((result (get-records pf name)))
              (let ((next-result 
		     (if (equal? result #f)
			 nil
			 result)))
                (if (and (equal? do-stop-at-first-match #t)
			 (not (equal? next-result nil)))
		    (car next-result)
		    (append next-result (find-employee-record first-name middle-initial last-name rest))))))))))

; a.

; see above for implementation of get-record given name and division personnel file.

; personnel files can be structured in any way desired.

; our type information is a two-tuple for type of personnel file and name, 
; as each of these components has structure that is division-specific.

; b.

; see above for implementation of get-salary given record.

; for us, we just implement a record as a list, 
; so retrieval of salary container is a matter 
; of looking at the right offset for the list.

; c.

; see above for implementation of find-employee-record given name parts and a collection of division personnel files.

; d.

; implement constructors, setters, getters, record formats, component formats, personnel file format, populate using existing employee records; make sure we are using method look-up based on operation and type correctly; make sure that we tag for structures that have custom format w.r.t. current division; make sure that non-getters (i.e. mutators) have specially-named methods visible to outside

; ex. 2.75

; ex. 2.76

; ex. 2.77

; ex. 2.79

; ex. 2.80

; ex. 2.81

; ex. 2.83

; 2.

; scheme-1-related problem - map-1

; 3.

; modify scheme-1 to have let special form

; extra

; 1.

; type inference problem

; this week is to do with generic operators and how to support many types cleanly using a table; 
; extra credit is to do with deducing types for compilation


