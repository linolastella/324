#lang racket

#| In this exercise you prepare to write an interpreter for the Lambda Calculus.

 Complete all the parts marked by a “★” [some of them, indicated by “___”, require
  you to define the meaning of a PL term.]

 ★ Read all the comments in this file, and follow all the requirements they include.

 ★ Read and understand the lecture notes about symbols, quasiquotation, and ‘match’.

 The set of terms in our Lambda Calculus language will be:

   • <identifier> - a variable access term represented by a symbol

   • (<term-0> <term-1>) - a function call term represented as a list of two terms

   • (λ (<identifier>) <term>) - a λ term represented by a list with the symbol ‘λ’,
                                  followed by a list containing a single symbol,
                                  followed by a term for the body

 ★ The form of set definition above [recall CSC236/CSC240] is called: structural/inductive set definition 


 In this file you define functions to support an interpreter, which you will
  complete in Exercise 3.

 ★ Define the functions whose names are listed in this ‘provide’.
   You may assume they always receive valid arguments.

 Start by writing a stub version of each of them [a “stub” function is a
  simple function having the correct number of parameters and returning
  a simple constant] so that you can run this file and its tests.

 Use quasiquotation in patterns and building results, whenever appropriate.
 Although you can assume valid input, for patterns that match values that
  should contain a particular symbol somewhere, enforce that. Also, for
  values that should contain a particular form of sub-list, enforce that.
  But you do not need to enforce that ‘<identifier>’s are symbols. |#

(provide λ-term?
         function argument
         closure
         closure-parameter closure-body closure-environment
         binding extend)




#| Shorthands for defining unary functions expecting, or testing for, a particular
    form of argument.

 Use ‘Define’ or ‘Define-predicate’, defined below, for the unary functions you implement.

 ★ A unary function is: a funciton that takes in one argument
 ★ A predicate is: a relation or function that returns a boolean

 These new forms take a ‘match’ pattern instead of a parameter name.
 For ‘Define’, the body can use any names bound by the pattern.
 For ‘Define-predicate’ there is no body: it returns a boolean for whether the
  pattern matches. |#

(define-syntax-rule
  (Define (<function-id> <pattern>) <result-expr>)
  (define (<function-id> v)
    (match v [<pattern> <result-expr>])))

(define-syntax-rule
  (Define-predicate (<function-id> <pattern>))
  (define (<function-id> v)
    (match v [<pattern> #true]
      [_ #false])))

#| Some test cases, and examples of ‘Define’ and ‘Define-predicate’.

 Passing all the tests does not guarantee your implementation is correct.

 ★ Add a new example using ‘Define-predicate’, significantly different from the
    provided example, to make sure you have explored its behaviour before using it
    to implement the required functions.|#

; return true if the argument is made of only nested lists.
(Define-predicate (nested-list? `((,_ ...) ...)))


#| ★ For each function that does not have a test case with both a literal argument and
    literal result value, add one that does have both a literal argument and literal
    result value. [In PL a “literal” is an expression that doesn't contain a computation.
    A variable access is considered a computation.]

   You are encouraged, but not required, to add more tests where that helps you. |#

(module+ test

  ; Import a test framework for full racket.
  ; It shows only failing tests in the Interactions, remaining silent if all the tests pass.
  (require rackunit)

  ; tests for our Define-predicate example
  (check-false (nested-list? (list (list) 0)))
  (check-true (nested-list? (list (list 3) (list 0) (list 9))))
  (check-false (nested-list? (list 3)))
  

  ; Example: extracting the function term from a function call term.
  (check-equal? (function '(f g)) 'f)

  (define (double a)
    (* 2 a))
  (check-equal? (function `(,double 324)) double)
  
  (check-equal? (function `(,sqrt ,5)) sqrt)
  
  ; Example: extracting the argument term from a function call term.
  (check-equal? (argument '(f g)) 'g)
  (check-equal? (argument `(,procedure? ,double)) double)
  
  (define a-λ-term '(λ (x) (y x)))

  ; ★ An environment [recall CSC108] is: a list of variables and their values in a scope
  ; ★ A variable shadows another variable [recall CSC207] iff: it is already defined in the same environment

  ; Example environment.
  ; The first ‘y’ represents a variable named “y” with value 3, shadowing
  ;  another variable.
  (define an-environment '([y 3] [x 2] [z 4] [y 1]))
  
  ; A closure will be a combination of a λ term and an environment for the
  ;  scope of the λ term.
  
  (check-equal? (closure a-λ-term an-environment)
                '(closure: (λ (x) (y x)) ([y 3] [x 2] [z 4] [y 1])))
  
  (check-equal? (closure '(λ (a b c) #true) '([a 3] [a 4] [c 0] [b #true]))
                '(closure: (λ (a b c) #true) ([a 3] [a 4] [c 0] [b #true])))
  
  (check-equal? (closure-environment (closure a-λ-term an-environment))
                an-environment)
  
  (check-equal? (closure-environment (closure '(λ (a b c) #true) '([a 3] [a 4] [c 0] [b #true])))
                '([a 3] [a 4] [c 0] [b #true]))
  
  (check-equal? (closure-parameter (closure a-λ-term an-environment))
                'x)
  
  (check-equal? (closure-parameter (closure '(λ (a) #true) '([a 3] [a 4] [c 0] [b #true])))
                'a)
  
  (check-equal? (closure-body (closure a-λ-term an-environment))
                '(y x))

  (check-equal? (closure-body (closure '(λ (a) #true) '([a 3] [a 4] [c 0] [b #true])))
                #true)

  (check-false (λ-term? "Hello"))
  
  (check-true (λ-term? a-λ-term))
  
  (check-false (λ-term? (closure-body (closure a-λ-term an-environment))))
  
  ; Examples: getting the local value of a variable from an environment.
  ; Use higher-order functions ‘map’, ‘apply’, and/or ‘filter’ appropriately,
  ;  to implement ‘binding’. Do not recurse.
  (check-equal? (binding an-environment 'y) 3)
  (check-equal? (binding an-environment 'z) 4)
  (check-equal? (binding '([a 3] [a 4] [c 0] [b #true]) 'c) 0)

  ; Example: putting a variable and its value in the most local scope of
  ;  an environment.
  (check-equal? (extend an-environment 'z 5)
                '([z 5] [y 3] [x 2] [z 4] [y 1]))

  (check-equal? (extend '([a 3] [a 4] [c 0] [b #true]) 'hi 324)
                '([hi 324] [a 3] [a 4] [c 0] [b #true]))


  ; Design for our binding function definition.
  (check-equal? (equal? 'y (first '(z 5)))
                #false)
  (check-equal? (equal? 'y (first '(y 3)))
                #true)
  
  (check-equal? (filter (λ (x) (equal? 'y (first x))) '([z 5] [y 3] [x 2] [z 4] [y 1]))
                '((y 3) (y 1)))
  
  )
; ★ Put your function definitions here.

(Define-predicate (λ-term? `(λ (,_) ,_)))

(Define (function `(,f ,_))
        f)

(Define (argument `(,_ ,g))
        g)

(define (closure a-λ-term an-environment)
  `(closure: ,a-λ-term ,an-environment))

(Define (closure-environment `(,closure ,a-λ-term ,an-environment))
        an-environment)

(Define (closure-body `(,closure (λ (,_) ,body) ,an-environment))
        body) 

(Define (closure-parameter `(,closure (λ (,parameter) ,_) ,an-environment))
        parameter)

(define (binding an-environment variable)
  (first (map second
              (filter (λ (x) (equal? variable (first x)))
                      an-environment))))
                    
(define (extend an-environment variable value)
  (append `((,variable ,value)) an-environment))
