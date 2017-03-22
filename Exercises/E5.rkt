#lang racket

#| CSC324 2017W Exercise 5. |#

#| Pattern Matching and Templating.

 We've seen:

   Compile-time pattern matching on code, which is called “macro by example”.
     E.g. Racket's ‘syntax-rules’ macros, or Rust's macros.

   Run-time pattern matching on data.
     Many languages support this [although it might seem rare to you at the moment since
      Python, Java, and C do not support this, except for tuple-assignment in Python].
     There are also many domain-specific languages whose main purpose is pattern matching,
      e.g. XQuery for HTML/XML, and regular expressions for strings.

 In some languages [e.g. Haskell and Prolog] pattern matching is the main mechanism for defining
  variables, with the binding of a single variable to an entire value being a special case.

 In this exercise you'll implement a run-time pattern matcher, that “binds” variables by
  creating a table of their values [as opposed to defining racket variables].
 You'll also implement a “templating” system. Racket's macro transformation contains one,
  and run-time quasi-quotation is another. It's a common concept in many languages: a simple
  familiar example is string formatting in Python [which has analogues in C, Racket, and
  most other languages]. |#


#| Pattern Variable Environments.

 Pattern matching binds “pattern variables” to parts of the data, and so it essentially
  manages an environment. Racket has hash tables [and other dictionary implementations],
  but we'll continue a white-box approach, practicing functional programming by building
  on the familiar lists of 2-tuples of name and value.

 It is a pre-condition and post-condition on environments that:
   • An environment never contains more than one binding for a name.
   • When combining environments, no name is bound in more than one of the environments.

 ★ Fix the stub implementations of ‘binding’ and ‘combine’. |#

(provide empty-environment simple-environment
         combine
         binding)

(module Environment racket

  (provide empty-environment simple-environment
           combine
           binding)
  
  (define empty-environment '())
  
  (define (simple-environment id value)
    `([,id ,value]))

  #| Produce the value of ‘id’ in ‘environment’, or ‘id’ if it does not have a binding. |#
  (define (binding environment id)
    (if (empty? environment)
        id
        (if (equal? id (first (first environment)))
            (second (first environment))
            (binding (rest environment) id))))

  #| Produce a single environment from a list of environments, that contains all the bindings
      from the given list of environments. |#
  (define (combine environments)
    (apply append environments)))

(require 'Environment)

#| Passing these tests is not sufficient for correctness!

   If you are unsure about the expected behaviour in a certain case, be sure to formulate
    a tentative simplest test for it before asking about the behaviour. |#
  
(module+ test (require rackunit)
  (check-equal? (binding (simple-environment 'a 324) 'a)
                324)
  (check-equal? (binding (simple-environment 'a 324) 'b)
                'b)
  (check-equal? (combine '(([a 324] [b 207])
                           ([c 148] [d 108])))
                '([a 324] [b 207] [c 148] [d 108]))
  (check-equal? (combine '())
                '())
  (check-equal? (combine '(()
                           ([c 148] [d 108])))
                '([c 148] [d 108]))
  (check-equal? (combine '(([a 324] [b 207])
                           ()))
                '([a 324] [b 207])))

#| Fix the stub implementations of these two functions: |#
(provide match fill)

#| Match ‘pattern’ with ‘v’, treating the symbols in ‘literals’ as pattern literals, and
    any other symbols that occur somewhere in ‘pattern’ as pattern variables.

 Pre-condition:
   • ‘literals’ is a list of symbols.
   • ‘pattern’ and ‘v’ are recursively built from numbers, booleans, symbols, and lists.
   • Any symbol appearing somewhere in ‘pattern’ is either also in ‘literals’, or occurs
      exactly once in ‘pattern’.

 The result is either an environment of bindings for the pattern variables to parts of ‘v’,
  or #false if ‘v’ doesn't match the ‘pattern’. |#

(define (check-lists? x y)
  (and (list? x) (list? y)))

(define (check-same-length? x y)
  (equal? (length x) (length y)))

(define (check-literal? x literals)
  (member x literals))


(define (match literals pattern v)
  ; This will probably be convenient to use:
  (define (matcher pattern v) (match literals pattern v))
  (if (check-lists? pattern v)
      (if (not (check-same-length? pattern v))
          #false
          (if (empty? pattern)
              '()
              (local [(define f (matcher (first pattern) (first v)))
                      (define r (matcher (rest pattern) (rest v)))]
                (if (or (boolean? f) (boolean? r))
                    #false
                    (combine (list f r))))))
      (if (list? pattern)
          #false
          (if (check-literal? pattern literals)
              (if (equal? pattern v)
                  '()
                  #false)
              (simple-environment pattern v)))))
     

(module+ test (require rackunit)

  #| Passing these tests is not sufficient for correctness!

  If you are finding the exercises or assignments long, we suggest following the standard
   Software Engineering Practices reviewed here, which are not only meant to produce correct
   code but to put a bound on the time spent implementing and debugging.

  • Make a complete set of test cases, starting with the simplest possibilities, and then
     increase the complexity [we can't review the entire testing curriculum from CSC108/148/207
     here, but that's what we're referencing].

  • Write the tests *in parallel* with implementing the functions, extending them every time
     you add a test [unless they handle the test automatically]. In particular, don't write
     any recursive code until you've written code that handles the base case(s) and *passes
     your corresponding tests*.

  • To the extent it's possible/reasonable: implement the functions in parallel. Don't increase
     the complexity until all the functions have been implemented up that level of complexity.

  • We are doing stateless programming, so making partial and full design test cases is always
     an available approach any time you have any trouble or doubt about the implementation.
    [A closer look at UBC's curriculum suggests that this, and the support it provides for doing
     the other bullet points, and the commitment to the other bullet points in their curriculum,
     suggests that by 3rd year their students are well-practiced in a disciplined proper
     software design process. This probably explains why their PL course seems ambitious.]

  If you follow the above approach then you should rarely, if ever, be in a situation where more
   than one aspect of the code is giving you difficulty [producing it, or debugging it].
  And you'll always have something working to submit [which most of you already know will be
   expected throughout your professional career, i.e. “no broken commits”.]

  Making and passing simple test cases, then working up will produce a series of function
   requirements simpler than the final one, culminating in the final one. This technique
   generalizes, and it's often taken for granted that you were taught it at some point:

    If you are having trouble with a problem, solve a simpler version of the problem.

  Let's see that in this situation.

  For ‘match’ we could focus first on implementing just a predicate, that just compares
   whether two values have the same “shape”, i.e. are the same if considered as trees
   but ignoring the values of the labels. Write predicates that:

    1. Check if two values are both lists, or both not lists.
    2. Check if two values are both lists of the same length, or both not lists.
    3. Check #2 recursively.

  You pick the hardest one of those that you find easy and start with that. Then you repeat
   by looking at all the ones harder than the one you started with. If at any time you have
   trouble and skipped an intermediate problem, back up. If the gap between two problems is
   too big to isolate one difficulty for you, then invent a problem inbetween.

  To continue working up to ‘match’, we could now write a predicate that checks if the shape
   of a value is an extension of the shape of another value. In terms of an underlying tree:
   is a value's tree a “top” part of another value's tree.

   4. Extend #3 with: if the first value is not a list, then the second value can be anything.

  Ask us if you'd like suggestions for other intermediate problems to solve first, for this
   exercise or anything else you implement in this course. And if you do this in other courses
   you are very likely to elicit a lot of help and support from those course's instructors. |#

  (check-equal? (match '() '() '())
                '())

  (check-equal? (match '() '$ 3)
                '(($ 3)))

  (check-equal? (match '() '($) '(S))
                '(($ S)))

  (check-equal? (match '($) '$ '$)
                '())

  (check-equal? (match '($) '($ x) '($ #true))
                '((x #true)))

  (check-equal? (match '($) '$ 'x)
                #false)

  (check-equal? (match '($) 'x '$)
                '((x $)))

  (check-equal? (match '() '(a) 'x)
                #false)

  (check-equal? (match '() '(a b) 'x)
                #false)

  (check-equal? (match '($ &) '($ &) '($ &))
                '())
  
  (check-equal? (match '($ &) '($ &) '($ E))
                #false)

  (check-equal? (match '() '$ '(324))
                '(($ (324))))
  
  (check-equal? (match '(ℒ : ∈)
                  '(ℒ element-expression : id ∈ list-expression)
                  '(ℒ (sqr x) : x ∈ (list 3 2 4)))
                '([element-expression (sqr x)]
                  [id x]
                  [list-expression (list 3 2 4)]))

  (check-equal? (match '(: ∈)
                  '(ℒ element-expression : id ∈ list-expression)
                  '(L (sqr x) : x ∈ (list 3 2 4)))
                '([ℒ L]
                  [element-expression (sqr x)]
                  [id x]
                  [list-expression (list 3 2 4)]))

  (check-equal? (match '(: ∈)
                  '(ℒ element-expression : id ∈ list-expression)
                  '(ℒ (sqr x) : x ∈ (list 3 2 4)))
                '([ℒ ℒ]
                  [element-expression (sqr x)]
                  [id x]
                  [list-expression (list 3 2 4)]))
  
  (check-equal? (match '(: ∈)
                  '(ℒ element-expression : id ∈ list-expression)
                  '(ℒ (sqr x) : x ∈ 3 2 4))
                #false)

  (check-equal? (match '()
                  '((a))
                  '((b)))
                '([a b]))

  (check-equal? (match '(: ∈)
                  '(ℒ element-expression : id ∈ list-expression)
                  '(ℒ (sqr x) : x in (list 3 2 4)))
                #false))


#| Fill in ‘template’, substituting symbols in it that have a binding in ‘environment’
    with their values. |#
(define (fill template environment)
  ; This will probably be convenient to use:
  (define (filler template) (fill template environment))
  (if (symbol? template)
      (binding environment template)
      (if (empty? template)
          '()
          (apply list (filler (first template)) (filler (rest template))))))


(module+ test

  (check-equal? (fill '$ '())
                '$)

  (check-equal? (fill '$ '([$ 324]))
                324)

  (check-equal? (fill '() '())
                '())

  (check-equal? (fill '(a) '())
                '(a))

  (check-equal? (fill '() '([a 2]))
                '())

  (check-equal? (fill 'a '([a b] [b c]))
                'b)

  (check-equal? (fill '(x) '([x #false]))
                '(#false))
  
  (check-equal? (fill '(a (b)) '([a (a)]))
                '((a) (b)))
  
  (check-equal? (fill '(a (b)) '([b (a)]))
                '(a ((a)))))

#| This puts ‘match’ and ‘fill’ together to do macro-like recursive expansion.
 There is nothing for you to implement here, but having implemented ‘match’ and ‘fill’
  this should help solidify your understanding of the macro expansion process. |#
(module+ test

  (define ((transformer literals pattern template) v)
    (define environment (match literals pattern v))
    (if environment
        (fill template environment)
        v))
  
  (define cond-pattern '(cond [<condition> <consequent>]
                              [else <alternative>]))
  (define if-template '(if <condition>
                           <consequent>
                           <alternative>))

  #| From a lab. |#
  
  (define ((deep-mapper f) s)
    (if (and (list? s) (equal? s (f s)))
        (map (deep-mapper f) s)
        (f s)))

  (define (fixed-point f v₀)
    (if (equal? v₀ (f v₀))
        v₀
        (fixed-point f (f v₀))))

  (check-equal? (fixed-point (deep-mapper (transformer '(cond else) cond-pattern if-template))
                             '(define (f)
                                (cond [(cond [(zero? 0) 1]
                                             [else 2])
                                       3]
                                      [else 4])))
                '(define (f) (if (if (zero? 0)
                                     1
                                     2)
                                 3
                                 4))))
