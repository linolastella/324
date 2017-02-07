;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname E1) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
#| CSC324 2017 Winter. Exercise 1.

 1. Change the implementation of ‘fix-1st’ below to not name the returned function, but
     instead make an anonymous function.

 2. Read the summary of mapping and applying below.

 3. Complete the ‘check-expect’s below, and the functions ‘columns→database’ and ‘π’.
    Do not use recursion. 
    Add at least two partial design ‘check-expect’s for ‘π’ after the last one,
     showing the process of generalization that ‘check-expect’. |#


; fix-1st : binary-function any → unary-function
(define (fix-1st binary-function v0)
  (λ (v1) (binary-function v0 v1)))

(check-expect ((fix-1st list* 324) (list 1 2 3))
              (list 324 1 2 3))

(check-expect ((fix-1st + 324) 1000)
              1324)

; ∘ : unary-function unary-function → unary-function
(define (∘ unary-f unary-g)
  (λ (v) (unary-f (unary-g v))))

(check-expect ((∘ - sqrt) 324) -18)
(check-expect ((∘ sqrt -) 324) 0+18i)


#| Calling a function multiple times, on elements of a list or lists. |#

#| Do you want to:
     Transform a list of elements.
     Transform each element independently / in parallel.
     Produce a list of the results, which has the same length. |#
#;(map <f> (list     <a>       <b>       <c>  ...))
#;(list         (<f> <a>) (<f> <b>) (<f> <c>) ...)

#| Do you want to:
     Transform two equal-length lists of elements.
     Transform each corresponding pair of elements from the two lists.
     Produce a list of the results, which has the same length. |#
#;(map <f>
       (list     <a1>            <b1>            <c1>       ...)
       (list          <a2>            <b2>            <c2>  ...))
#;(list     (<f> <a1> <a2>) (<f> <b1> <b2>) (<f> <c1> <c2>) ...)

#| More generally: |#
#;(map <f>
       (list <a1> <b1> <c1> ...)
       (list <a2> <b2> <c2> ...)
       (list <a3> <b3> <c3> ...)
       ...)
#;(list (<f> <a1> <a2> <a3> ...) (<f> <b1> <b2> <b3> ...) (<f> <c1> <c2> <c3> ...) ...)


#| Calling a function once, with a computed *list* of arguments. |#

#| Do you have a list of [most of] the arguments for a function call,
  instead of the arguments, and want to use the list as the arguments. |#

#;(apply <f> (list <a0> <a1> ...))
#;      (<f>       <a0> <a1> ...)

#;(apply <f> <a> <b> ... (list <a0> <a1> ...))
#;      (<f> <a> <b> ...       <a0> <a1> ...)


#| columns→database |#

; columns→database : list of equal-length lists → list of lists
; Produce a database of rows, from a list of the columns.

; Test case:
(check-expect (columns→database (list (list 1 2 3)))
              (list (list 1)
                    (list 2)
                    (list 3)))
; Test case:
(check-expect (columns→database (list (list 1 2 3)
                                      (list 4 5 6)))
              (list (list 1 4)
                    (list 2 5)
                    (list 3 6)))

; Partial partial design. Complete this:
#;(check-expect (map list (list 1 2 3) (list 4 5 6))
                (list (list 1 4)
                      (list 2 5)
                      (list 3 6)))

; Partial partial design. Complete this:
#;(check-expect
   ; Use the same ‘<?>’ and ‘<??>’ as above.
   (map list (list 1 2 3) (list 4 5 6))
   (apply map list (list (list 1 2 3) (list 4 5 6))))

; Partial partial design. Complete this:
#;(check-expect (apply map list (list (list 1 2 3) (list 4 5 6)))
                (list (list 1 4)
                      (list 2 5)
                      (list 3 6)))

; Partially completed design. Complete this:
#;(check-expect (columns→database (list (list 1 2 3) (list 4 5 6)))
                (apply map list (list (list 1 2 3) (list 4 5 6))))

(define (columns→database columns)
  (apply map list columns))


(define database
  (list (list "Ada" "Lovelace" 1815)
        (list "Alan" "Turing" 1912)
        (list "Alonzo" "Church" 1903)
        (list "Grace" "Hopper" 1906)
        (list "Kurt" "Goedel" 1906)))

(define first-name first)
(define last-name second)
(define birth-year third)

; π : list-of-unary-functions list-of-lists → list-of-lists
; ‘(π (list column-selector ...) R)’ produces a database containing the columns from ‘R’ that
;  the ‘column-selector’s produce.

(check-expect (π (list last-name) database)
              (list
               (list "Lovelace")
               (list "Turing")
               (list "Church")
               (list "Hopper")
               (list "Goedel")))

(check-expect (π (list first-name last-name birth-year) database)
              database)

(check-expect (π (list first-name birth-year)
                 (filter (∘ (fix-1st <= 1905) birth-year) database))
              (list (list "Alan" 1912)
                    (list "Grace" 1906)
                    (list "Kurt" 1906)))

(check-expect (π (list first-name last-name) database)
              (columns→database
               (list (list (first-name (list "Ada" "Lovelace" 1815))
                           (first-name (list "Alan" "Turing" 1912))
                           (first-name (list "Alonzo" "Church" 1903))
                           (first-name (list "Grace" "Hopper" 1906))
                           (first-name (list "Kurt" "Goedel" 1906)))
                     (list (last-name (list "Ada" "Lovelace" 1815))
                           (last-name (list "Alan" "Turing" 1912))
                           (last-name (list "Alonzo" "Church" 1903))
                           (last-name (list "Grace" "Hopper" 1906))
                           (last-name (list "Kurt" "Goedel" 1906))))))

;Partial design:
#;(check-expect (π (list first-name last-name) database)
                (columns→database
                 (list (map first-name (list (list "Ada" "Lovelace" 1815)
                                             (list "Alan" "Turing" 1912)
                                             (list "Alonzo" "Church" 1903)
                                             (list "Grace" "Hopper" 1906)
                                             (list "Kurt" "Goedel" 1906)))
                       (map last-name (list (list "Ada" "Lovelace" 1815)
                                            (list "Alan" "Turing" 1912)
                                            (list "Alonzo" "Church" 1906)
                                            (list "Grace" "Hopper" 1906)
                                            (list "Kurt" "Goedel" 1906))))))

;Partial design:
#;(check-expect (π (list first-name last-name) database)
                (columns→database
                 (list (map first-name database) (map last-name database))))

(define (π column-selectors R)
  (columns→database
   (map (λ (f) (map f R)) column-selectors)))
