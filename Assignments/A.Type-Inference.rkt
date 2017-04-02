#lang racket

#| CSC324 2017 Winter. Assignment: Type Inference. |#

(provide B R
         (struct-out Γ) (struct-out ≡)
         α-type-generator)

(provide some-terms some-types some-type-information
         type-info)

#| The Language of Terms.

 A term is one of:
   • <boolean-literal> - represented by a boolean
   • <numeric-literal> - represented by a number
   • <identifier> - represented by a symbol that:
     ∘ is not one of 'if, 'λ, 'local, or 'define
     ∘ uniquely determines the variable under the usual semantics, i.e. no variables with
        different scopes have the same name
     ∘ is not one of '𝔹oolean, 'ℝeal, or '→
     ∘ does not start with a greek letter
   • (if <condition-term> <consequent-term> <alternative-term>)
     - represented by a list with the symbol 'if and three terms
   • (λ (<identifier> ...) <body-term>)
     - represented by a list with the symbol 'λ, a list of symbols, and a term
   • (local [(define <identifier> <initializer-term>)]
       <body-term>)
     - represented by a list with the symbol 'local,
        a singleton list containing a list with the symbol 'define, a symbol, and a term,
        and a term
   • (<function-term> <argument-term> ...) - represented by a non-empty list of terms

 These representations should be familiar, and the prose descriptions are just reminders. |#

#| ★ Make some example terms.

 Define ‘some-terms’ to contain one of each kind of:
   • base case term
   • recursive term, using only your base case terms as component terms, and
     ∘ for each of your base case terms, use it in at least one of these terms
   • recursive term, using only your previous recursive terms as component terms
     ∘ for each of your previous recursive terms, use it in at least one of these terms

 For each kind of term whose definition includes any number of identifiers or any number
  of component terms, have at least one of each of that kind of term where:
    • zero identifiers are used
    • two identifiers are used
    • zero component terms are used
    • two component terms are used
 A term can be used to cover more than one of these cases. |#

(define some-terms
  '(;base case terms
    #true 0 x 
    ;recursive terms
    (if #true #true 0) (λ () #true) (λ (a b) 0)
    (local [(define y #true)] y) (x) (+ 0 0)
    ;nested recursive terms
    (if (if #true #true 0) (+ 0 0) (x)) (λ () (local [(define y #true)] y))
    (local [(define z (λ () #true))] (λ (a b) 0)) ((λ () #true))))


#| The Semantics of Terms.

 The terms have the familiar semantics, except the conditional requires that its condition
  produces a boolean value.

 Recall that the <identifier> bound by ‘local’ is in the scope of its initializing term. |#

#| The Language of Types.

 A type is one of:

   • boolean - represented by the symbol '𝔹oolean

   • number - represented by the symbol 'ℝeal

   • function from argument type(s) to result type
     - represented by a list of the argument type(s), the symbol '→, then the result type:
        (<argument-type> ... → <result-type>)

   • type variable - represented by a symbol that is not one of:
     ∘ 'if, 'λ, 'local, or 'define
     ∘ '𝔹oolean, 'ℝeal, or '→ |#

(define R 'ℝeal)
(define B '𝔹oolean)

#| ★ Make some example types.

 Define ‘some-types’ to contain one of each kind of:
   • base case type
   • recursive type, using only your base case types as component types
   • recursive type, using only your previous recursive types as component types

 Have at least one function type for a thunk, and one for a binary function. |#

(define some-types
  (list
    ;base case types
    B R 'x
    ; recursive types
    `(→ ,R) `(,B → x) `(,R ,R → ,B)
    ; nested recursive types
    `((,B → x) → (→ ,R)) `((,R ,R → ,B) (,R ,R → ,B) → (→ ,R))))


#| Type Information for Terms.

 During type inference, information about the type of a term will be:

   • a type for the term
   • type constraints: a list of pairs of types that must be “compatible”
      for the term to be properly typed

 The type information is defined by structural induction/recursion:

   • <boolean-literal>
     type: boolean
     constraints: empty

   • <numeric-literal>
     type: number
     constraints: empty

   • <identifier>
     type: a type variable whose name is the identifier's name
     constraints: empty
     
   • (if <condition-term> <consequent-term> <alternative-term>)
     type: the type of the consequent
     constraints:
       ∘ the condition's type is compatible with boolean
       ∘ the consequent's type is compatible with the alternative's type
       ∘ all the constraints from the condition, consequent, and alternative

   • (λ (<identifier> ...) <body-term>)
     type: a function type
       ∘ the argument types are the types of the parameter identifiers [as if those were terms]
       ∘ the result type is the body's type
     constraints: the body term's constraints

   • (<function-term> <argument-term> ...)
     type: a type variable whose name is 'α<index> for a natural number <index> that has
            not yet been used
     constraints:
       ∘ the function term's type is compatible with a function type whose:
          - argument types are the the argument terms' types
          - result type is the type of this function call term
       ∘ all the constraints from the function term and argument terms

   • (local [(define <identifier> <initializer-term>)]
       <body-term>)
     type: the body's type
     constraints:
       ∘ the initializer's type is compatible with the identifier's type [as if it were a term]
       ∘ all the constraints from the initializer and body |#

#| Struct for the type information of a term. [The latex name is \Gamma]. |#
(struct Γ (type constraints) #:transparent)

#| Struct for a single compatibility constraint. [The latex name is \equiv]. |#
(struct ≡ (type₀ type₁) #:transparent)

; Example of type information, from one of the ‘some-example-terms’ I made for myself:
#;(Γ 'α0 (list (≡ 'b B)
               (≡ 'a B)
               (≡ B R)
               (≡ 'a '(→ α0))))
#;(Γ 'α0 (list (≡ 'b '𝔹oolean)
               (≡ 'a '𝔹oolean)
               (≡ '𝔹oolean 'ℝeal)
               (≡ 'a '(→ α0))))

#| ★ Determine the type information for each of your example terms.

 For each term in your ‘some-example-terms’, determine its type information by applying the
  above algorithm, and put the result in ‘some-type-information’: |#

(define some-type-information
  (list
   ;base case terms
   (Γ B '()) (Γ R '()) (Γ 'x '())
   ;recursive terms
   (Γ B (list (≡ B B) (≡ B R))) (Γ `(→ ,B) '()) (Γ `(a b → ,R) '())
   (Γ 'y (list (≡ B 'y))) (Γ 'α0 (list (≡ 'x '(→ α0))))
   (Γ 'α0 (list (≡ '+ `(,R ,R → α0))))
   ;nested recursive terms
   (Γ 'α0 (list (≡ B B) (≡ B R) (≡ 'α0 'α1) (≡ '+ `(,R ,R → α0)) (≡ 'x '(→ α1))))
   (Γ 'α0 (list (≡ `(→ ,B) '(→ α0))))
   (Γ `(a b → ,R) (list (≡ 'z `(→ ,B))))
   (Γ '(→ y) (list (≡ B 'y)))))


#| ★ Implement the algorithm you traced manually above.

 Implement ‘type-info’ to take a term and an α type generator, producing its type information. |#


#| (α-type-generator index) produces a stateful thunk that produces types of the form 'α<index>.

 The index defaults to starting at 0, but depending on how you make your test cases you might
  find it useful to set the initial index to something other than 0. |#

(define ((α-type-generator [index 0]))
  (local-require (only-in racket/syntax format-symbol))
  (set! index (add1 index))
  (format-symbol "α~a" (sub1 index)))


(define (type-info a-term [αs (α-type-generator)])

  #| ‘↓’ is a new kind of pattern, meant to be used as a sub-pattern, that:
      • automatically recurses on a component term where it occurs
      • binds the type and constraints of the resulting Γ instance to the two supplied names

     For example, instead of matching

       component-term

      inside a pattern and then extracting the type and constraints from (t-i component-term)
       in the result expression, you can instead use

       (↓ component-type component-constraints)

      and then in the result the names ‘component-type’ and ‘component-constraints’ will be
       bound for you to the type and constraints of (t-i component-term).

     But first start writing your implementation without ‘↓’, until you notice the repetition
      in the code you are writing. That repetition is what ‘↓’ can eliminate. |#
  
  (define-match-expander ↓
    (syntax-rules ()
      [(↓ <name-for-a-component-type> <name-for-a-component-constraints>)
       (app t-i (Γ <name-for-a-component-type> <name-for-a-component-constraints>))]))

  (define (t-i a-term)
    (match a-term
      [(? boolean?) (Γ B '())]
      [(? number?) (Γ R '())]
      [(? symbol?) (Γ a-term '())]
      [`(if ,(↓ cond-t cond-cs) ,(↓ cons-t cons-cs) ,(↓ alt-t alt-cs))
       (Γ cons-t (append (list (≡ cond-t B) (≡ cons-t alt-t))
                         cond-cs cons-cs alt-cs))]
      [`(λ (,ids ...) ,(↓ body-t body-cs))
       (Γ (append (map (λ (a) (Γ-type (t-i a))) ids) (list '→ body-t)) body-cs)]
      [`(local [(define ,id ,(↓ init-t init-cs))] ,(↓ body-t body-cs))
       (Γ body-t (append (list (≡ (Γ-type (t-i id)) init-t)) init-cs body-cs))]
      [`(,(↓ func-t func-cs) ,(↓ args-t args-cs) ...)
       (define index (αs))
       (Γ index (apply append (list (≡ func-t (append args-t `(→ ,index))))
                       func-cs args-cs))]))

  (t-i a-term))
