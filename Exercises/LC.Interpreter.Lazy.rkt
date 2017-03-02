#lang racket

#| Exercise 4 : Lazy Evaluation. |#

(provide eva)

#| Delaying Evaluation.

 The current implementation of ‘eva’ binds an identifier in exactly one
  situation: binding the parameter to the argument value when calling
  a function.

 Change function call to “delay” the evaluation of the argument expression,
  by wrapping the argument expression in a no-parameter lambda term, also
  called a “thunk”, along with the current environment. So, the argument's
  “value” you will produce should be of the form:

    (closure: (λ () <argument-expression>) <current-environment>)

 Then, when a binding is looked up: match its value to that form, and then
  evaluate the stored argument expression in the scope of the stored
  environment.

 Also, extend ‘eva’ to accept numeric literals and produce the corresponding
  numeric value, so we can simplify some of the examples. |#

(require "LC.Interpreter.lib.rkt")

(module+ test

  (require rackunit)

  (check-equal? (eva 324 '()) 324)

  (check-equal? (eva '((λ (x) 324) 123) '()) 324)
  
  (check-equal? (eva '((λ (x) y) 123)
                     ; Note: environments can only contain delayed values now.
                     '([y (closure: (λ () 324) ())]))
                324)

  (check-equal? (eva '((λ (y) 324)
                       ; An argument that's an infinite loop!
                       ((λ (x) (x x)) (λ (x) (x x))))
                     '())
                324)

  (local [(define if '(λ (condition)
                        (λ (consequent)
                          (λ (alternative)
                            ((condition consequent) alternative)))))
          (define true '(λ (b0) (λ (b1) b0)))
          (define false '(λ (b0) (λ (b1) b1)))
          (define ω '((λ (x) (x x)) (λ (x) (x x))))]
    
    (check-equal? (eva `(((,if ,true) 324) ,ω) '()) 324)
    (check-equal? (eva `(((,if ,false) ,ω) 324) '()) 324))

  )


(define (eva t env)
  (cond [(number? t) t]
        [(λ-term? t) (closure t env)]
        [(list? t) (local [(define f (eva (function t) env))
                           (define delayed-arg (closure `(λ () ,(argument t)) env))]
                     (eva (closure-body f)
                          (extend (closure-environment f)
                                  (closure-parameter f)
                                  delayed-arg)))]
        [else (local [(define bind (binding env t))]
                (match bind [`(closure: (λ () ,argument) ,env)
                             (eva argument env)]))]))
