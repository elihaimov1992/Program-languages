#lang pl

;;;; PL_EX_3:
;;;; Part A:
;; About: Implementation a “proper” implementation of sqrt: one that returns two results (one positive and one negative).
;; **Note: This will have a great impact on the language (and hence on your implementation), since it will now allow
;;         several returned values, and hence, also several arguments for operations.
;; 2 Steps: 1. Add a plain sqrt expression to the language.
;;          2. Extend it so it can deal with multiple values so the sqrt can be made into the proper one.


;;;; Part A: Step 1.0: Adding sqrt to the Language (Multiple Values + Fixing sqrt):
#| BNF for the MUWAE language:
     <MUWAE> ::= <num>
             | { + <MUWAE> <MUWAE> }
             | { - <MUWAE> <MUWAE> }
             | { * <MUWAE> <MUWAE> }
             | { / <MUWAE> <MUWAE> }
             | { sqrt <MUWAE> }  ;; 1.1: Adding a new rule for a unary (i.e., that takes a single input) in the BNF definition.
             | { with { <id> <MUWAE> } <MUWAE> }
             | <id>
|#


;; MUWAE abstract syntax trees
(define-type MUWAE
    [Num  (Listof Number)]  ;; Fixing from Number to Listof Number- to the final result that builded from list and multiple values.
    [Add  MUWAE MUWAE]
    [Sub  MUWAE MUWAE]
    [Mul  MUWAE MUWAE]
    [Div  MUWAE MUWAE]
    [Sqrt  MUWAE]  ;; 1.2: Adding a new variant to the AST type definition
    [Id   Symbol]
    [With Symbol MUWAE MUWAE])


(: parse-sexpr : Sexpr -> MUWAE)
;; to convert s-expressions into MUWAEs
(define (parse-sexpr sexpr)
  (match sexpr
    [(number: n)    (Num (list n))] ;; Fixing from "n" to "list n"
    [(symbol: name) (Id name)]
    [(cons 'with more)
     (match sexpr
       [(list 'with (list (symbol: name) named) body)
        (With name (parse-sexpr named) (parse-sexpr body))]
       [else (error 'parse-sexpr "bad `with' syntax in ~s" sexpr)])]
    [(list '+ lhs rhs) (Add (parse-sexpr lhs) (parse-sexpr rhs))]
    [(list '- lhs rhs) (Sub (parse-sexpr lhs) (parse-sexpr rhs))]
    [(list '* lhs rhs) (Mul (parse-sexpr lhs) (parse-sexpr rhs))]
    [(list '/ lhs rhs) (Div (parse-sexpr lhs) (parse-sexpr rhs))]
    [(list 'sqrt exp) (Sqrt (parse-sexpr exp))]  ;; 1.3: Adding a line to parse these expressions.
    [else (error 'parse-sexpr "bad syntax in ~s" sexpr)]))


(: parse : String -> MUWAE)
;; parses a string containing a MUWAE expression to a MUWAE AST
(define (parse str)
  (parse-sexpr (string->sexpr str)))

#| Formal specs for `subst':
   (`N' is a <num>, `E1', `E2' are <MUWAE>s, `x' is some <id>, `y' is a
   *different* <id>)
      N[v/x]                = N
      {+ E1 E2}[v/x]        = {+ E1[v/x] E2[v/x]}
      {- E1 E2}[v/x]        = {- E1[v/x] E2[v/x]}
      {* E1 E2}[v/x]        = {* E1[v/x] E2[v/x]}
      {/ E1 E2}[v/x]        = {/ E1[v/x] E2[v/x]}
      {sqrt E}[v/x]         = {sqrt E[v/x]}  ;; 1.4: Adding a line to the formal definition of subst for these expressions.
      y[v/x]                = y
      x[v/x]                = v
      {with {y E1} E2}[v/x] = {with {y E1[v/x]} E2[v/x]}
      {with {x E1} E2}[v/x] = {with {x E1[v/x]} E2}
|#


(: subst : MUWAE Symbol MUWAE -> MUWAE)
;; substitutes the second argument with the third argument in the
;; first argument, as per the rules of substitution; the resulting
;; expression contains no free instances of the second argument
(define (subst expr from to)
  (cases expr
    [(Num n) expr]
    [(Add l r) (Add (subst l from to) (subst r from to))]
    [(Sub l r) (Sub (subst l from to) (subst r from to))]
    [(Mul l r) (Mul (subst l from to) (subst r from to))]
    [(Div l r) (Div (subst l from to) (subst r from to))]
    [(Sqrt exp)  (Sqrt (subst exp from to))]  ;; 1.5: Adding the corresponding case in the subst implementation.
    [(Id name) (if (eq? name from) to expr)]
    [(With bound-id named-expr bound-body)
     (With bound-id
           (subst named-expr from to)
           (if (eq? bound-id from)
             bound-body
             (subst bound-body from to)))]))


 #| Formal specs for `eval':
     eval(N)         = N
     eval({+ E1 E2}) = eval(E1) + eval(E2)
     eval({- E1 E2}) = eval(E1) - eval(E2)
     eval({* E1 E2}) = eval(E1) * eval(E2)
     eval({/ E1 E2}) = eval(E1) / eval(E2)
     eval({sqrt E})  = (sqrt eval(E))  ;; 1.6: Adding a line in the formal definition of eval.
     eval(id)        = error!
     eval({with {x E1} E2}) = eval(E2[eval(E1)/x])
|#


;; Adding and Finishing the sqrt+ method:
(: sqrt+ : (Listof Number) -> (Listof Number))
#|
;; Exp:
;; A version of `sqrt' that takes a list of numbers, and return a list
;; with twice the elements, holding the two roots of each of the inputs;
;; throws an error if any input is negative.
;; Otherwise, In the last condition we use a recursion on sqrt+, to activate and include all the roots of the list values
;;            (and holding the two roots of each inputs) and then, if necessary, to do some arithmetic action between them.
;; Difficulties: Understand how the sqrt+ function will work and write the body/content code of the third condition, which is more difficult.
;; Source:
;; I helped with the WhatsApp group of the course.
;; About "let": https://docs.racket-lang.org/reference/let.html?q=let#%28form._%28%28lib._racket%2Fprivate%2Fletstx-scheme..rkt%29._let%29%29
;; Time: It took about 2.5 hours (All the first part of sqrt (Part A) and including reading the instructions- understanding them and
;;       fixing all parts of the codes).
|#
(define (sqrt+ ns)
  (cond [(null? ns) null]
        [(< (first ns) 0) (error 'sqrt+ "`sqrt' requires a non-negative input")]
        [else (let ([root-val (sqrt (first ns))])
                (cons root-val (cons (- root-val) (sqrt+ (rest ns)))))]))


#|
;; tests for sqrt+ :
(test (sqrt+ '()) => '())
(test (sqrt+ '(0)) => '(0 0))
(test (sqrt+ '(-0)) => '(0 0))
(test (sqrt+ '(9)) => '(3 -3))
(test (sqrt+ '(1 9)) => '(1 -1 3 -3))
(test (sqrt+ '(0 -1)) =error> "`sqrt' requires a non-negative input")
(test (sqrt+ '(-1 0)) =error> "`sqrt' requires a non-negative input")
(test (sqrt+ '(-1 -1)) =error> "`sqrt' requires a non-negative input")
|#


;;;; Part A: Step 2.0: Fixing the Arithmetic Operators (Fixing the With):
(: bin-op : (Number Number -> Number) (Listof Number) (Listof Number) -> (Listof Number))
;; applies a binary numeric function on all combinations of numbers from
;; the two input lists, and return the list of all of the results 
(define (bin-op op ls rs)
  (: helper : Number (Listof Number) -> (Listof Number))
  (define (helper l rs)
    (: f : Number -> Number)
    (define (f num)
      (op l num))
    (map f rs))
  (if (null? ls) null
      (append (helper (first ls) rs) (bin-op op (rest ls) rs))))


;; Fixing and updating the eval after Fixing the Arithmetic Operators:
(: eval : MUWAE -> (Listof Number))
;; evaluates MUWAE expressions by reducing them to numbers
;; Fixing Output: from Number to Listof Number
;; **Note: Details of the difficulties and time regarding step 2 are listed above in step 1 which also includes step 2.
(define (eval expr)
  (cases expr
    [(Num n) n]
    [(Add l r) (bin-op + (eval l) (eval r))]
    [(Sub l r) (bin-op - (eval l) (eval r))]
    [(Mul l r) (bin-op * (eval l) (eval r))]
    [(Div l r) (bin-op / (eval l) (eval r))]
    [(Sqrt e) (sqrt+ (eval e))]  ;; 1.7: Adding and Fixing the corresponding case in the eval definition.
    [(With bound-id named-expr bound-body)
     (eval (subst bound-body
                  bound-id
                  (Num (eval named-expr))))]
    [(Id name) (error 'eval "free identifier: ~s" name)]))


(: run : String -> (Listof Number))
;; evaluate a MUWAE program contained in a string
;; Fixing Output: from Number to Listof Number
(define (run str)
  (eval (parse str)))


#|
;; tests for bin-op:
(test (bin-op + '(0) '(1)) => '(1))
(test (bin-op + '(1 2) '(3 4)) => '(4 5 5 6))
(test (bin-op * '(0) '(1)) => '(0))
(test (bin-op / '(0) '(1)) => '(0))
(test (bin-op - '(1 2) '(3 4)) => '(-2 -3 -1 -2))
(test (run "{+ 3 {sqrt 9}}") => '(6 0))
(test (run "{+ {sqrt 1} {sqrt 9}}") => '(4 -2 2 -4))
(test (run "{+ {sqrt 1} 3}") => '(4 2))
(test (run "{+ {/ {+ {sqrt 1} 3} 2} {sqrt 100}}")=> '(12 -8 11 -9))
(test (run "{sqrt {+ 16 {* {+ 1 {sqrt 1}} {/ 9 2}}}}") => '(5 -5 4 -4))
|#


#|
;; General tests (From WAE file with fixing after changes):
(test (run "5") => '(5))
(test (run "{+ 5 5}") => '(10))
(test (run "{with {x {+ 5 5}} {+ x x}}") => '(20))
(test (run "{with {x 5} {+ x x}}") => '(10))
(test (run "{with {x {+ 5 5}} {with {y {- x 3}} {+ y y}}}") => '(14))
(test (run "{with {x 5} {with {y {- x 3}} {+ y y}}}") => '(4))
(test (run "{with {x 5} {+ x {with {x 3} 10}}}") => '(15))
(test (run "{with {x 5} {+ x {with {x 3} x}}}") => '(8))
(test (run "{with {x 5} {+ x {with {y 3} x}}}") => '(10))
(test (run "{with {x 5} {with {y x} y}}") => '(5))
(test (run "{with {x 5} {with {x x} x}}") => '(5))
(test (run "{with {x 5} {* x 5}}") => '(25))
(test (run "{with {x 5} {/ 10 x}}") => '(2))
(test (run "{with {x 1} y}") =error> "free identifier")
(test (run "{5 {x y}}") =error> "bad syntax in")
(test (run "{x {y w}}") =error> "bad syntax in")
(test (run"{with {x y}}") =error> "bad `with' syntax in")

(test (run "{sqrt 0}") => '(0 0))
(test (run "{sqrt -0}") => '(0 0))
(test (run "{sqrt 1}") => '(1 -1))
(test (run "{sqrt 9}") => '(3 -3))

(test (run "{with {x 1} {sqrt x}}") => '(1 -1))
(test (run "{with {x {sqrt 25}} {+ x 5}}") => '(10 0))
|#


;;;; Part B: Detecting free instances in a WAE program:
;; About: A code that contains free instances of an identifier is a bad code, and should not be evaluated into anything but an error message.
;; Part B: 1. Identifying free instances before evaluation:


#|

<WAE> ::= <num> 
   | {+ <WAE> <WAE>}
   | {-  <WAE> <WAE>}
   | {* <WAE> <WAE>}
   | {/ <WAE> <WAE>}
   | {with {<id> <WAE>} <WAE>}
   | <id>

|#



(define-type WAE
  [NumW Number]
  [AddW WAE WAE]
  [SubW WAE WAE]
  [MulW WAE WAE]
  [DivW WAE WAE]
  [IdW Symbol]
  [WithW Symbol WAE WAE])



(: parse-sexprW : Sexpr -> WAE) 
(define (parse-sexprW sexpr)
  (match sexpr
    [(number: n) (NumW n)]
    [(symbol: name) (IdW name)]
    [(cons 'with more)
     (match sexpr
       [(list 'with (list (symbol: name) named) body)
        (WithW name (parse-sexprW named) (parse-sexprW body))]
       [else (error 'parse-sexprW "bad `with' syntax in ~s" sexpr)])]
    [(list '+ lhs rhs) (AddW (parse-sexprW lhs) (parse-sexprW rhs))]
    [(list '- lhs rhs) (SubW (parse-sexprW lhs) (parse-sexprW rhs))]
    [(list '* lhs rhs) (MulW (parse-sexprW lhs) (parse-sexprW rhs))]
    [(list '/ lhs rhs) (DivW (parse-sexprW lhs) (parse-sexprW rhs))]
    [else (error 'parse-sexprW "bad syntax in ~s" sexpr)]))

(: parseW : String -> WAE)
(define (parseW str)
  (parse-sexprW (string->sexpr str)))



#| Formal specs for `subst':
   (`N' is a <num>, `E1', `E2' are <WAE>s, `x' is some <id>,
   `y' is a *different* <id>)
      N[v/x]                = N
      {+ E1 E2}[v/x]        = {+ E1[v/x] E2[v/x]}
      {- E1 E2}[v/x]        = {- E1[v/x] E2[v/x]}
      {* E1 E2}[v/x]        = {* E1[v/x] E2[v/x]}
      {/ E1 E2}[v/x]        = {/ E1[v/x] E2[v/x]}
      y[v/x]                = y
      x[v/x]                = v
      {with {y E1} E2}[v/x] = {with {y E1[v/x]} E2[v/x]}
      {with {x E1} E2}[v/x] = {with {x E1[v/x]} E2}
|#



(: substW : WAE Symbol WAE -> WAE)
(define (substW expr from to)
  (cases expr
    [(NumW n) expr]
    [(AddW l r) (AddW (substW l from to) (substW r from to))]
    [(SubW l r) (SubW (substW l from to) (substW r from to))]
    [(MulW l r) (MulW (substW l from to) (substW r from to))]
    [(DivW l r) (DivW (substW l from to) (substW r from to))]
    [(IdW name) (if (eq? name from) to expr)]
    [(WithW bound-id named-expr bound-body)
     (WithW bound-id
           (substW named-expr from to)
           (if (eq? bound-id from)
               bound-body
               (substW bound-body from to)))]))


;; Exp:
;; FreeInstanceList function consumes an abstract syntax tree (WAE) and returns null if there are no free instance, and a list of all
;; the free instances otherwise.
;; *freeInstanceList Method helped by the substW method- that returns back to freeInstanceList (that operates recursively) WAE expression and
;; so until the conditions/cases of the stop.
;; *In the rest of the arithmetic cases, we use the "append" that combines and links the 2 lists that recursively recur from
;; expressions from the 2 wings.
;; **Note: Multiple occurrences of the same identifier should all appear in the list.
;; Difficulties: Understand the intent of the exercise instructions and apply the case of WithW.
;; Source:
;; I helped with the WhatsApp group and the Moodlearn forum of the course.
;; About Using in Append: https://docs.racket-lang.org/reference/pairs.html?q=append#%28def._%28%28quote._~23~25kernel%29._append%29%29 
;; Time: It took about 1.5 hours.
(: freeInstanceList : WAE -> (Listof Symbol))
(define (freeInstanceList exp)
  (cases exp
    [(NumW n) (list)]
    [(AddW l r) (append (freeInstanceList l) (freeInstanceList r))]
    [(SubW l r) (append (freeInstanceList l) (freeInstanceList r))]
    [(MulW l r) (append (freeInstanceList l) (freeInstanceList r))]
    [(DivW l r) (append (freeInstanceList l) (freeInstanceList r))]
    [(IdW name) (list name)]
    [(WithW bound-id named-expr bound-body)
     (freeInstanceList (substW bound-body bound-id named-expr))]))


#|
;; tests for freeInstanceList:
(test (freeInstanceList (parseW "5")) => '())
(test (freeInstanceList (IdW 'x)) => '(x))
(test (freeInstanceList (WithW 'x (NumW 2) (AddW (IdW 'y) (NumW 3)))) => '(y))
(test (freeInstanceList (WithW 'x (NumW 2) (AddW (IdW 'y) (IdW 'x)))) => '(y))
(test (freeInstanceList (WithW 'x (NumW 2) (MulW (IdW 'y) (AddW (IdW 'x) (NumW 3))))) => '(y))
(test (freeInstanceList (WithW 'x (NumW 2) (DivW (IdW 'y) (SubW (IdW 'x) (NumW 3))))) => '(y))
(test (freeInstanceList (parseW "{with {x 1} y}")) => '(y))
(test (freeInstanceList (parseW "{+ x 1}")) => '(x))
(test (freeInstanceList (parseW "w")) => '(w))
(test (freeInstanceList (parseW "{with {xxx 2} {with {yyy 3} {+ {- xx y} z}}}")) => '(xx y z))
(test (freeInstanceList (parseW "{with {xxx 2} {with {yyy 3} {* {/ xx y} z}}}")) => '(xx y z))
(test (freeInstanceList (WithW 'x (NumW 2) (AddW (IdW 'x) (NumW 3)))) => '())
(test (freeInstanceList (WithW 'x (NumW 2) (SubW (IdW 'x) (NumW 3)))) => '())
(test (freeInstanceList (WithW 'x (NumW 2) (MulW (IdW 'x) (NumW 3)))) => '())
(test (freeInstanceList (WithW 'x (NumW 2) (DivW (IdW 'x) (NumW 3)))) => '())
(test (freeInstanceList (parseW "{+ z {+ x z}}")) => '(z x z))
|#
