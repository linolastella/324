#lang racket

#| CSC324 2017 Winter. Exercise 6. |#

#| 1. Write three statically-typed polymorphic HOFs, and explicitly declare their types.

   2. Write a function to rename the variables in a term from a core language, producing an
       “alpha-equivalent” term where there is no variable shadowing. |#

#| A standard reminder for courses that involve programming.

 It's assumed that you are interested in and practicing the industry best practices of TDD
  and Unit Testing, as taught and practiced in your previous courses. In particular, you are
  preparing for job interviews, and the jobs themselves. To save yourself time when asking
  for help: be prepared with appropriate test cases and their corresponding implementation,
  up to the point at which your question or problem arises. |#


#| For Task #1: the functions are in the sub-module ‘static-helpers’, and exported here. |#
(provide count fix-1st fix-2nd)

#| Task #1 Testing Sub-module, for any tests you would like to add. |#
(module+ test (require rackunit)
  
  (check-equal? (count even? (list 1 2))
                1)
  
  (check-equal? (count procedure? '())
                0)
  
  (check-equal? ((fix-1st equal? 10) 10)
                #true)
  
  (check-equal? ((fix-1st equal? 10) 0)
                #false)
  
  (check-equal? ((fix-2nd equal? 10) 10)
                #true)
  
  (check-equal? ((fix-2nd equal? 10) 0)
                #false)
  
  (check-equal? ((fix-2nd + 12) 80) 92)
  
  (check-equal? (count (fix-2nd member '(207 263))
                       '(108 148 207 209 236 258))
                1))

#| Task #1 Implementation Sub-module. |#
(module static-helpers typed/racket
  
  (provide count fix-1st fix-2nd)
  
  #| (count p l) : for a unary “truthy” predicate ‘p’ and list ‘ℓ’, produce a count of the
      number of elements of ‘ℓ’ for which ‘p’ produces a non-‘#false’ value.

     (fix-1st f 1st) : for a binary function ‘f’ and value ‘1st’, produce a unary function
      that behaves like ‘f’ with the first argument fixed as ‘1st’.

     (fix-2nd f 2nd) : for a binary function ‘f’ and value ‘2nd’, produce a unary function
      that behaves like ‘f’ with the second argument fixed as ‘2nd’. |#
  
  #| ★ Fix the non-polymorphic static type declarations to be more specific, but no more than
        necessary, using a polymorphic type. And implement the functions. |#
  
  (: count : (∀ (α) (α → Any) (Listof α) → Nonnegative-Integer))
  
  (define (count p l)
    (length (filter (λ (x) (not (false? x))) (map p l))))
  
  
  (: fix-1st : (∀ (α β γ) ((α β → γ) α → (β → γ))))
  
  (define ((fix-1st f 1st) 2nd)
    (f 1st 2nd))
  
  
  (: fix-2nd : (∀ (α β γ) ((α β → γ) β → (α → γ))))
  
  (define ((fix-2nd f 2nd) 1st)
    (f 1st 2nd)))

(require 'static-helpers)

#| Task #2.

 ★ Implement ‘rename’ below.

 For a term t₀, produce a term t₁ such that:
   • t₁ has the same meaning as t₀
     - in particular, free variables are not renamed, because they would be given meaning
        by using the term in some context that binds those names
   • bound variables are subscripted by how many times a variable with that name is bound
      in the sequence of enclosing scopes, so that there is no variable shadowing in t₁

 For subscripting you can use the following library. |#

#| Task #2 Subscripting library. |#
(module subscript racket
  (provide subscripted)
  #| subscripted : any subscriptable/list-of-subscriptable → symbol
     A symbol version of ‘id’, with a subscript formed from ‘index’.
     The ‘index’ is either a value who representation consists of characters that can be
      subscripts, or a list of such values in which case they are separated by ‘₋’ when
      producing the subscript. |#
  (define (subscripted id index)
    (define (subscript c)
      (if (list? c)
          (apply string-append (map subscript c))
          (let ([c (~a c)]
                [mapping (apply map cons
                                (map (curry map ~a)
                                     (map string->list
                                          '("0123456789+-=()" "₀₁₂₃₄₅₆₇₈₉₊₋₌₍₎"))))])
            (if (= (string-length c) 1)
                (dict-ref mapping c)
                (subscript (string->list c))))))
    (local-require (only-in racket/syntax format-symbol))
    (format-symbol "~a~a"
                   id
                   (subscript (add-between (if (list? index) index (list index)) '-)))))

(require 'subscript)

#| Task #2 Testing Sub-module, for any tests you would like to add. |#
(module+ test (require rackunit)
  
  ; Illustration of the provided ‘subscripted’ function.
  (check-equal? (subscripted 'x '12) 'x₁₂)
  
  ; Unary usage of ‘rename’:
  (check-equal? (rename '(f ((λ (f x)
                               ((λ (x) (f (f x)))
                                x))
                             x)))
                '(f ((λ (f₁ x₁)
                       ((λ (x₂) (f₁ (f₁ x₂)))
                        x₁))
                     x)))
  
  (check-equal? (rename '(local [(define x 10)] x))
                '(local [(define x₁ 10)] x₁))

  (check-equal? (rename '(if (λ (x) x) (λ (x) x) (λ (x) x)))
                '(if (λ (x₁) x₁) (λ (x₁) x₁) (λ (x₁) x₁)))
  
  ; Binary usage of ‘rename’, which could occur as a recursive step:
  (check-equal? (rename '(f (f x)) '(x f x))
                '(f₁ (f₁ x₂))))

#| (rename term bound) : ‘term’ with variables renamed to avoid shadowing as described above.

 Study the example test case to confirm your understanding.
 [If you are unsure what “scope”, “bound”, or “shadowing” are in PLs, please ask about those
  before asking about the exercise].

 ‘bound’ is a list of names of variables bound in the enclosing scopes of ‘term’, and in the
  header of ‘rename’ it has a default value of empty. It is useful for the recursive step(s).

 A term is one of:
   • <boolean-literal> - represented by a boolean
   • <numeric-literal> - represented by a number
   • <identifier> - represented by a symbol that is not one of 'if, 'λ, 'local, 'define.
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
 These representations should be familiar, and the prose descriptions are just reminders.

 Pre-condition: ‘term’ does not have any of its free variables already subscripted. |#
(define (rename term [bound '()])
  (define renamer (fix-2nd rename bound))
  (match term
    [(? boolean?) term]
    [(? number?) term]
    [(? symbol?) (if (member term bound)
                     (subscripted term (count (fix-2nd equal? term) bound))
                     term)]
    [`(if ,cond ,cons ,alt) `(if ,(renamer cond) ,(renamer cons) ,(renamer alt))]
    [`(λ (,ids ...) ,body)
     `(λ ,(map (fix-2nd rename (append ids bound)) ids)
        ,((fix-2nd rename (append ids bound)) body))]
    [`(local [(define ,id ,init)] ,body)
     `(local [(define ,((fix-2nd rename (list* id bound)) id)
                ,((fix-2nd rename (list* id bound)) init))]
        ,((fix-2nd rename (list* id bound)) body))]
    [`(,term-f ,term-a ...) (append (list (renamer term-f)) (map renamer term-a))]))
