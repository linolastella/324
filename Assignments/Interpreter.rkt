#lang racket

#| In this assignment you write an interpreter for the following subset of racket:

 • <literal> - a boolean or numeric constant
 • <identifier>
 • (λ (<identifier> ...)
     <body-term>
     ...)
 • (<term-f> <term-a> ...)
 • (local [(define <identifier> <term>)
           ...]
     <body-term>
     ...)
 • (if <condition-term>
       <consequent-term>
       <alternative-term>)
 • (set! <identifier> <term>)
 • (zero? <term>)
 • (+ <term> ...)
 • (* <term> ...)

 The <identifier>s never have one of the names: λ local define if set! zero? + * |#

; The functions to implement:
(provide parse index-binders unparse environment-tree simplify-closure eva)


#| The parser turns the representation that uses lists, symbols, and constants, into
    an “Abstract Syntax Tree” [AST]. The nodes of the tree will be of these types: |#

(struct Literal (value) #:transparent)
(struct Var (identifier) #:transparent)
(struct Lambda (index parameter-ids body) #:transparent)
(struct App (function arguments) #:transparent)
(struct Local (index defined-identifiers initializers body) #:transparent)
(struct If (condition consequent alternative) #:transparent)
(struct Set! (identifier expression) #:transparent)

(provide (struct-out Literal)
         (struct-out Var)
         (struct-out Lambda)
         (struct-out App)
         (struct-out Local)
         (struct-out If)
         (struct-out Set!))

; parse : term → AST
; Parse a term represented with lists, symbols, and constants, into one of the above
;  struct instances [which recursively will usually contain other struct instances].
; Set the index for Lambdas and Locals to '_ .
(define (parse t)
  (match t
    [(or (? number?) (? boolean?)) (Literal t)]
    [(? symbol?) (Var t)]
    [`(λ (,id ...) ,body ...) (Lambda '_ (map parse id) (map parse body))]
    [`(if ,cond ,cons ,alt) (If (parse cond) (parse cons) (parse alt))]
    [`(set! ,id ,term) (Set! (parse id) (parse term))]
    [`(zero? ,term) (App 'zero? (list (parse term)))]
    [`(+ ,term ...) (App '+ (map parse term))]
    [`(* ,term ...) (App '* (map parse term))]
    [`(local [(define ,id ,term) ...] ,body-term ...)
     (Local '_ (map parse id) (map parse term) (map parse body-term))]
    [`(,term-f ,term-a ...) (App (parse term-f) (map parse term-a))]))


#| Indexing Lambdas and Locals.

 To help show closures and environments, it will be helpful to have an indexing for
  the two operations that create new environments.

 They are indexed according to where they are in the tree of code, relative to each other. |#

; An example to illustrate the indexing.
#;(local [(define f (λ (x y)
                      (λ ()
                        (+ x y))))]
    ((λ (g z)
       ((g (z) (z))))
     f
     (local [(define a 324)]
       (λ ()
         a))))
; The outermost Local would have index '(0), shown below as local₀.
; The other Local would have index '(0 2), shown below as local₀₋₂.
; The λ₀₋₂₋₀ is the 0th local or λ, under the 2nd local or λ, under the 0th local or λ.
#;(local₀ [(define f (λ₀₋₀ (x y)
                           (λ₀₋₀₋₀ ()
                                   (+ x y))))]
          ((λ₀₋₁ (g z)
                 ((g (z) (z))))
           f
           (local₀₋₂ [(define a 324)]
                     (λ₀₋₂₋₀ ()
                             a))))

; Here's a little Counter class to help.
; It has a single parameter count, but the bracketing defaults it to -1.
; Calling Counter with no arguments creates a no-argument function that produces
;  0, 1, 2, ...: incrementing each time it's called.
(define ((Counter [count -1]))
  (set! count (+ 1 count))
  count)

; I added this for testing purposes
(provide Counter)

#;; Here's how you would use it with index-binders:
((index-binders '() (Counter))
 (parse '(λ (x) x)))

; index-binders : [list-of-numbers Counter] → [AST → AST]
(define ((index-binders parents sibling) t)
  (define indexer (index-binders parents sibling))
  (cond
    [(Lambda? t) (local [(define new-parent (append parents (list (sibling))))]
                   (Lambda new-parent (Lambda-parameter-ids t)
                           (map (index-binders new-parent (Counter)) (Lambda-body t))))]
    [(Local? t) (local [(define new-parent (append parents (list (sibling))))
                        (define new-counter (Counter))]
                  (Local new-parent (Local-defined-identifiers t)
                         (map (index-binders new-parent new-counter) (Local-initializers t))
                         (map (index-binders new-parent new-counter) (Local-body t))))]
    [(App? t) (App (indexer (App-function t)) (indexer (App-arguments t)))]
    [(list? t) (map indexer t)]
    [else t]))


#| subscripted : any list-of-subscriptable → symbol
 A symbol version id, with a subscript formed from the list elements turned into subscripts
  and separated by  ₋  . |#
(define (subscripted id index)
  (define (subscript c)
    (if (list? c)
        (apply string-append (map subscript c))
        (let ([c (~a c)]
              [mapping (apply map cons
                              (map (curry map ~a)
                                   (map string->list '("0123456789+-=()" "₀₁₂₃₄₅₆₇₈₉₊₋₌₍₎"))))])
          (if (= (string-length c) 1)
              (dict-ref mapping c)
              (subscript (string->list c))))))
  (local-require (only-in racket/syntax format-symbol))
  (format-symbol "~a~a" id (subscript (add-between index '-))))


#| unparse : AST → list/symbol/constant representation

 Turn an indexed AST into the original representation, but with the Lambdas and
 Locals subscripted.

 See the example above where indexing was described.
 Do not try to reproduce any square brackets, just produce normal lists.
|#
(define (unparse t)
  (match t
    [(? symbol?) t]  ; t is a symbol iff t is 'zero? '+ or '*
    [(Literal val) val]
    [(Var id) id]
    [(App func args) (append (list (unparse func)) (map unparse args))]
    [(If cond cons alt) (list 'if (unparse cond) (unparse cons) (unparse alt))]
    [(Set! id expr) (list 'set! (unparse id) (unparse expr))]
    [(Lambda ind par body)
     (append (list (subscripted 'λ ind)) (list (map unparse par)) (map unparse body))]
    [(Local ind ids inits body)
     (append (list (subscripted 'local ind))
             (list (map (λ (id init) (list 'define (unparse id) (unparse init))) ids inits))
             (map unparse body))]))


#| For interpretation use the following structs for closures and environments.

 A Closure contains a Lambda and an Environment.

 An Environment contains:
   index : a number
   bindings : a list of 2-tuples of a symbol and a box with the current value
   parent : the environment of the enclosing scope, or #false for the initial environment
   children : a growing list of enviroments that this is a parent of

 Notice that this makes a tree of environments without the closure layers intertwined.
 Instead the closures point into the tree, and their level can also be seen in the code's
  indexed Lambdas.

 Indexing of environments simply counts: 0, 1, 2, .... |#

(struct Closure (lambda environment) #:transparent)
(struct Environment (index bindings parent [children #:mutable]) #:transparent)

(provide (struct-out Closure)
         (struct-out Environment))

; Examples of mutating boxes, and the children field of Environments.
(module+ test
  (require rackunit)
  
  (define b (box 123)) ; Constructor.
  (check-equal? (unbox b) 123) ; Accessor.
  (check-equal? b #&123) ; Literal notation for a constant box, but shows how they display.
  (set-box! b 324) ; Mutator.
  (check-equal? (unbox b) 324)

  (define env-0 (Environment 0 '() #false '()))
  (define env-1 (Environment 1 `((x ,(box 324))) env-0 '()))
  (set-Environment-children! env-0 (append (Environment-children env-0) (list env-1)))
  (check-equal? (match env-0 [(Environment _ _ _ envs) envs])
                (list (Environment 1 `((x ,(box 324))) env-0 '())))
  )


#| simplify-closure : Closure → 2-tuple

 A simplified representation of a closure: the symbol λ subscripted with the index of
 the Lambda, and the symbol E subscripted with the index of the Environment.

|#
(define (simplify-closure closure)
  `(,(subscripted 'λ (Lambda-index (Closure-lambda closure)))
    ,(subscripted 'E (list (Environment-index (Closure-environment closure))))))


#| environment-tree : Environment → list

 A simplified representation of the tree of environments starting from some root environment.
 The boxes in the bindings are unwrapped, and closure values are simplified.

   (E_n ([identifier number/boolean/simplified-closure] ...) ; bindings
        <child-environment>
        ...)
|#
(define (environment-tree env)
  (append (list (subscripted 'E (list (Environment-index env))))
          (list (Environment-bindings env))
          (map environment-tree (Environment-children env))))

#| Parameters [not to be confused with λ parameters] are a disciplined way to handle
   nested state.

 They could have been used effectively for the counters in index-binders,
 but I didn't want to go into the details. Here they're used minimally to manage just
 a bit of state.

 (E0 body ...) evaluates its body statements with a fresh environment index counter.

 (new-environment-index) is for use in the implementation of eva to get a next index. |#

(provide E0 global-env)

(define (new-environment-index) ((environment-index)))
(define (global-env) (Environment (new-environment-index) '() #false '()))
(define environment-index (make-parameter (void)))
(define-syntax-rule (E0 body ...)
  (parameterize ([environment-index (Counter)])
    body
    ...))

(module+ test
  (E0 (define G (global-env))
      (define closure ((eva G)
                       ((index-binders '() (Counter)) (parse '(λ (x) 324)))))
      (println closure) ; Notice this prints 1 at the moment.
      (println (simplify-closure closure))
      (println (environment-tree G))))


; eva : Environment → [AST → boolean/number/Closure]
; The value of AST t in the Environment env.
(define ((eva env) t)
  (match t
    [(Literal value) value]
    [(Var id) (unbox (get-box env id))]
    [(If cond cons alt) (if ((eva env) cond) ((eva env) cons) ((eva env) alt))]
    [(App func args) (define f ((eva env) t))
                     (define arg-list (map (eva env) args))
                     (#false)]
    [(Local ind ids inits body) (#f)]
    [(Lambda ind par body) (Closure t env)]
    [(Set! id expr)
     (set-box! (get-box env (Var-identifier id)) ((eva env) expr))]
    ['zero? zero?]
    ['+ +]
    ['* *]))

; get-box : Environment identifier → box
; the box <identifier> is bound to in <Environment>
(define (get-box env id)
  (first (map second (filter (λ (x) (equal? id (first x)))
                             (Environment-bindings env)))))
