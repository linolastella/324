#lang racket

(provide eva)

(require "LC.Interpreter.lib.rkt")

#| ★ Complete the requirements in the test module below.
      Write the prose description of your algorithm.
      Then implement ‘eva’ to evaluate a term [from the language of Exercise 2]
       in the scope of an environment.

 The semantics is the familiar eager by-value evaluation of variables and function calls,
  and creation of closures that know the environment they were created in.

 The only values that can appear in environments are closures.

 Try your example test cases with our memory modeler, and study the diagrams.
 Try your example test cases with the Python Visualizer, and study the diagrams.

 Follow the understanding you developed from the required test cases, other tests you made
  to understand the algorithm on specific examples, the partial design check-expects you made,
  and your study of the memory model diagrams.

 ★ Write a description, in prose, of your algorithm:

 In "eva" first checks if the term is of the form "(λ (<parameter>) <body>)", i.e. a lambda-term.
 If so, it returns the closure of that term in the global environment. If not, it checks
 whether the term is a simple <identifier>. If so, it returns the values of that variable in
 the environment. Finally, the last possibility is that "t" represents a function call, i. e.
 an expression of the form (<term-0>, <term-1>). The procedure is then to evaluate the body of
 <term-0> (function called) in an environment where the parameter of <term-0> is bound to the
 value of <term-1> (argument of this function call).

|#

(module+ test
  (require rackunit)
  (require test-engine/racket-tests)

  #| ★ For each kind of term, where meaningful, write tests for ‘eva’ covering at least: |#

  #| • A simplest version of the term, in the scope of an empty environment. |#

  (check-equal? (eva '(λ (x) x) '())
                '(closure: (λ (x) x) ()))

  (check-equal? (eva '((λ (y) (y y)) (λ (x) x)) '())
                '(closure: (λ (x) x) ()))
  
  #| • A simplest version of the term, in the scope of the simplest environment,
        where the environment affects the value of the term. |#

  (check-equal? (eva 'a '([a (closure: (λ (b) b) ()) ]))
                '(closure: (λ (b) b) ()))
  
  (check-equal? (eva '(λ (_) x) '([x (closure: (λ (y) y) ()) ]))
                '(closure: (λ (_) x) ([x (closure: (λ (y) y) ()) ])))

  (check-equal? (eva '((λ (_) x) (λ (z) z)) '([x (closure: (λ (i) i) ())]))
                '(closure: (λ (i) i) ()))
                
  #| • A simplest version of the term, in the scope of the simplest environment,
        where the environment has a binding for a variable,
        and a variable with the same name also occurs in some closure's environment,
        and the two bindings have different values,
        and the two bindings affect the value of the term. |#

  (check-equal? (eva 'x '([x (closure: (λ (_) y) ([y (closure: (λ (x) x) ())]))]))
                '(closure: (λ (_) y) ((y (closure: (λ (x) x) ())))))
  
  #| ★ For each of the above test cases, where meaningful, write at least one partial design.|#

  ; partial design for the first two cases are actually full design.
  (check-expect (eva 'a '([a (closure: (λ (b) b) ()) ]))
                (binding '([a (closure: (λ (b) b) ()) ]) 'a))

  (check-expect (eva '(λ (_) x) '([x (closure: (λ (y) y) ()) ]))
                (closure '(λ (_) x) '([x (closure: (λ (y) y) ()) ])))

  (check-expect (eva '((λ (_) x) (λ (z) z)) '([x (closure: (λ (i) i) ())]))
                (eva 'x '([_ (closure: (λ (z) z) ([x (closure: (λ (i) i) ())]))]
                          [x (closure: (λ (i) i) ())])))
  
  #| ★ For each of the above test cases write a full design [except do not include the conditional
        that checks for which kind of term is being handled]. |#

  (check-expect (eva 'a '([a (closure: (λ (b) b) ()) ]))
                (binding '([a (closure: (λ (b) b) ()) ]) 'a))

  (check-expect (eva '(λ (_) x) '([x (closure: (λ (y) y) ()) ]))
                (closure '(λ (_) x) '([x (closure: (λ (y) y) ()) ])))

  (check-expect (eva '((λ (_) x) (λ (z) z)) '([x (closure: (λ (i) i) ())]))
                (eva (closure-body (eva (function '((λ (_) x) (λ (z) z)))
                                        '([x (closure: (λ (i) i) ())])))
                     (extend '([x (closure: (λ (i) i) ())])
                             (closure-parameter (eva (function '((λ (_) x) (λ (z) z)))
                                                     '([x (closure: (λ (i) i) ())])))
                             (eva (argument '((λ (_) x) (λ (z) z)))
                                  '([x (closure: (λ (i) i) ())])))))
  
  )

(define (eva t env)
  (if (λ-term? t)
      (closure t env)
      (if (symbol? t)
          (binding env t)
          (eva (closure-body (eva (function t) env))
               (extend (closure-environment (eva (function t) env))
                       (closure-parameter (eva (function t) env))
                       (eva (argument t) env))))))
