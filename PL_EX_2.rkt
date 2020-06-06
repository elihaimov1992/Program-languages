#lang pl


;; Ex_2:
;; Ex_2: Q_1:

; The ROL BNF and Parsing code:

;; Defining two new types
(define-type BIT = (U 0 1))
(define-type Bit-List = (Listof BIT))

;; The actual interpreter
#| BNF for the RegE language:
;; Q_1.1 (a+b):

<ROL> ::= {reg-len = <num> <RegE>}    ;(1)
 
<RegE> ::= <Bits>                     ;(2)
          |{and <RegE> <RegE>}        ;(3)   
          |{or <RegE> <RegE>}         ;(4)    
          |{shl <RegE>}               ;(5) 

<Bits> ::= <bit>                      ;(6)                
          | <bit> <Bits>              ;(7)        

<bit> ::= 1                           ;(8)               
         | 0                          ;(9)                   

write a BNF and parser for a new language “ROL”: a similarly simple language of “Register Operation Expressions”.

in this language should correspond to Racket expressions that evaluate to S-expressions holding lists of zero and ones, and symbols

*** Explanation: We use this to derive expressions in new language "ROL", a similarly simple language of
    “Register Operation Expressions”, that evaluate to S-expressions holding lists of zero and ones, and symbols,
    and we start with <ROL>, which should be from a form: {reg-len = <num> <RegE>} that contain:
    * a natural number <num> (length of all registers)
    * a sequence of register operations <RegE>, which should be one of these:
      * a sequence of bits <Bits>, which should be one of these:
        * a number zero or one <bit>
        * <bit> and <Bits>
      * the valid operation name(which refer to binary operation) "and", <RegE>, and another <RegE>
      * the valid operation name(which refer to binary operation) "or", <RegE>, and another <RegE>
      * the valid operation name(which refer to unary operation) "shl" and <RegE>


;; Q_1.1 (c):
;; 3 examples of derived trees:
;; First Example:
;; Input: "{ reg-len = 4 {1 0 0 0}}"
;; <ROL>                                    ;           ==>
;;   <num> <RegE>                           ;(1)        ==>
;;   4      <Bits>                          ;(2)        ==>
;;            <bit> <Bits>                  ;(7)        ==>
;;            1       <bit> <Bits>          ;(7)+(8)    ==>
;;                    0       <bit> <Bits>  ;(7)+(9)    ==>
;;                            0       <bit> ;(6)+(9)    ==>
;;                                    0     ;(9)        ==>
;; Output: (Reg '(1 0 0 0))

;; Second Example:
;; Input: "{ reg-len = 4 {and {shl {1 0 1 0}} {shl {1 0 1 0}}}}"
;; <ROL>                                                                        ;           ==>
;;   <num> <RegE>                                                               ;(2)        ==>
;;   4       {and {shl <RegE>} {shl <RegE>}}                                    ;(3)        ==>
;;                 {shl <RegE>} {shl <RegE>}                                    ;(5)        ==>          
;;                  <RegE>       <RegE>                                         ;           ==>
;;                    <Bits>       <Bits>                                       ;(2)        ==>
;;                     <bit> <Bits>                <bit> <Bits>                 ;(7)        ==>
;;                     1      <bit> <Bits>         1      <bit> <Bits>          ;(7)+(8)    ==>
;;                            0      <bit> <Bits>         0      <bit> <Bits>   ;(7)+(9)    ==>
;;                                   1      <bit>                1      <bit>   ;(7)+(8)    ==>
;;                                          0                           0       ;(9)        ==>
;; Output: (And (Shl (Reg '(1 0 1 0))) (Shl (Reg '(1 0 1 0))))

;; Third Example:
;; Input: "{ reg-len = 4 {or {shl {1 0 1 0}} {shl {1 0 1 0}}}}"
;; <ROL>                                                                        ;           ==>
;;   <num> <RegE>                                                               ;(1)        ==>
;;   4       {or {shl <RegE>} {shl <RegE>}}                                     ;(4)        ==>
;;                {shl <RegE>} {shl <RegE>}                                     ;(5)        ==>          
;;                  <RegE>       <RegE>                                         ;           ==>
;;                    <Bits>       <Bits>                                       ;(2)        ==>
;;                     <bit> <Bits>                <bit> <Bits>                 ;(7)        ==>
;;                     1      <bit> <Bits>         1      <bit> <Bits>          ;(7)+(8)    ==>
;;                            0      <bit> <Bits>         0      <bit> <Bits>   ;(7)+(9)    ==>
;;                                   1      <bit>                1      <bit>   ;(7)+(8)    ==>
;;                                          0                           0       ;(9)        ==>
;; Output: (Or (Shl (Reg '(1 0 1 0))) (Shl (Reg '(1 0 1 0))))

;; Source: I was assisted by this course's whatsapp group and most of all from the course presentations.
;; Difficulties: Understanding the question- what is required to do
;; and understanding this topic in depth, including understanding the tree derivative.
;; Time: It took about 2.5 hours.

|#
                   



;; Ex_2: Q_1.2(a): Parser for the Register Operation Language ROL:
;; Difficulties: After the question from the previous section, it was relatively easy and clear to answer this section.
;; Time: It took about 15 minutes.
;; RegE abstract syntax trees
(define-type RegE
  [Reg Bit-List] 
  [And RegE RegE]
  [Or RegE RegE]
  [Shl RegE])

;; Ex_2: Q_1.2(b):
;; Next is a technical function that converts (casts)
;; (any) list into a bit-list. We use it in parse-sexpr.
(: list->bit-list : (Listof Any) -> Bit-List)
;; to cast a list of bits as a bit-list
(define (list->bit-list lst)
  (cond [(null? lst) null]
        [(eq? (first lst) 1)(cons 1 (list->bit-list (rest lst)))]
        [else (cons 0 (list->bit-list (rest lst)))]))


(: parse-sexpr : Sexpr -> RegE)        
;; to convert the main s-expression into ROL
(define (parse-sexpr sexpr)
  (match sexpr
    ;;< --fill in-- > ;; remember to make sure specified register length is at least 1
    ;; Explanation - Accept list and checks:
    ;; If the list length is equal or less than 0 (the length must be at least 1) -> send an error message.
    ;; Otherwise, enter a function called "parse-sexpr-RegL" and send the parameters len and seq.
    ;; Parameters:
    ;; len = Natural number (Number).
    ;; seq = Sequence of Register operations to be performed.
    ;; Source: I was assisted by this course's whatsapp group.
    ;; Time: It took about half an hour.
    [(list 'reg-len '= (number: len) seq) (if (<= len 0)
                                              (error 'parse-sexpr "Register length must be at least 1 ~s" sexpr)
                                              (parse-sexpr-RegL seq len))]
    [else (error 'parse-sexpr "bad syntax in ~s" sexpr)]))


#|
;; Explanation: "parse-sexpr-RegL" convert s-expressions into RegEs by checking:
   1. If the number of bits is the same, otherwise, send error message. 
   2. If does not holds one of the variants then "parse-sexpr-RegL" sends error massage
   The "parse-sexpr-RegL" calls each list recursively.
;; Difficulties: Understand that the function must be called recursively.
;; Time: It took about a 1.5 hour.
|#

(: parse-sexpr-RegL : Sexpr Number -> RegE)
;; to convert s-expressions into RegEs
(define (parse-sexpr-RegL sexpr reg-len)
  (match sexpr
    [(list (and a (or 1 0)) ... ) (if (eq? (length a) reg-len)
                                      (Reg(list->bit-list a)) 
                                     (error 'parse-sexpr "wrong number of bits in ~s" a))]
    [(list 'and lst1 lst2) (And (parse-sexpr-RegL lst1 reg-len) (parse-sexpr-RegL lst2 reg-len))]
    [(list 'or lst1 lst2) (Or (parse-sexpr-RegL lst1 reg-len) (parse-sexpr-RegL lst2 reg-len))]
    [(list 'shl lst) (Shl (parse-sexpr-RegL lst reg-len))]
    [else (error 'parse-sexpr "bad syntax in ~s" sexpr)]))


(: parse : String -> RegE)
;; parses a string containing a RegE expression to a RegE AST
(define (parse str)
  (parse-sexpr (string->sexpr str)))


;; tests :
;(test (parse "{ reg-len = 4 {1 0 0 0}}") => (Reg '(1 0 0 0)))
;(test (parse "{ reg-len = 4 {shl {1 0 0 0}}}") => (Shl (Reg '(1 0 0 0))))
;(test (parse "{ reg-len = 4 {and {shl {1 0 1 0}} {shl {1 0 1 0}}}}") => (And (Shl (Reg '(1 0 1 0))) (Shl (Reg '(1 0 1 0)))))
;(test (parse "{ reg-len = 4 { or {and {shl {1 0 1 0}} {shl {1 0 0 1}}} {1 0 1 0}}}") => (Or (And (Shl (Reg '(1 0 1 0))) (Shl (Reg '(1 0 0 1)))) (Reg '(1 0 1 0))))
;(test (parse "{ reg-len = 2 { or {and {shl {1 0}} {1 0}} {1 0}}}") => (Or (And (Shl (Reg '(1 0))) (Reg '(1 0))) (Reg '(1 0))))
;(test (parse "{ reg-len = 4 {or {1 1 1 1} {0 1 1}}}") =error> "wrong number of bits in")
    

;; Ex_2: Q_2.1: Accommodating Memory Operations
 #|
‘MAE’ - Syntax for language has the functionality that use a ‘memory’ cell to get more “power” by adding a 'set' operator,
that sets the current memory value to some expression result and a 'get' operator to retrieve the current value.

<MAE> ::= <num>
         | { + <MAE> <MAE> }
         | { - <MAE> <MAE> }
         | { * <MAE> <MAE> }
         | { / <MAE> <MAE> }
         | { set <MAE> }
         | get

Describe the problem that is demonstrated by:  {* {+ {set 1} {set 2}} get}
and suggest a feature to add to the MAE semantics that will solve it.
**Note: This feature is already present in our AE implementation, but it is only implicit there.

;; The problem is "ambiguity", because there is no knowing who is the last number (1 or 2) to enter a memory cell
   and thus the expression can be derived in multiple ways.
   a  {* {+ {set 1} {set 2}} get}  can be derived in two ways that result in tree that look different, for example: 
   Result 1 (read {set 1} before {set 2}):  {* {+ {set 1} {set 2}} get} =>  {* {+ 1 {set 2}} get} =>  {* {+ 1 2} 2} => 6.
   Result 2 (read {set 2} before {set 1}):  {* {+ {set 1} {set 2}} get} =>  {* {+ {set 1} 2} get} =>  {* {+ 1 2} 1} => 3.
;; The First solution is define the order in the derivation- to start from left or right that help us to
   decide which number (1 or 2) to read before.
   Another solution is define the order in the derivation by "restricting derivation" or rather by "Left association" or
   "Right association" which will lead to single way to derive our expression in our case (or problem) by
   forcing the left side (or right, respectively) to be a number <num> (instead <MAE> that in left side).
;; Source: Course Presentations.
;; Difficulties: Understanding the question and choosing the right solution for the problem.
;; Time: It took about 20 minutes.

|#




#|
;; Ex2_Q2.2 :

*** Explanation: We use this to derive expressions and we start with <MAE>, for a sequence of computations,
    which should be from a form: {seq <SAE>}.
    <SAE> - Responsible for expression AE or expression form set (i.e. initializes the expression with a operator "set"),
    which should be one of these:
    * <AE> - Presented in lecture, simple AE Language, which should be one of these:
      ** A number <num>
      ** The operator "+", <AE> and another <AE>
      ** The operator "-", <AE> and another <AE>
      ** The operator "*", <AE> and another <AE>
      ** The operator "/", <AE> and another <AE>
    * The form: {set <AE>} <NAE>  ; <NAE> - New kind of <AE> that includes a get operator.
    * The form: {set <AE>} <CAE> <NAE>   ; <CAE> - Countinue of <NAE>.
    **Note: In this expression (second form) we make sure that the number of operator "get" does not exceed
            the number of operators "set", as required.
    <CAE> should be one of these:
    * The form: {set <NAE>}
    * The form: {set <NAE>} <CAE>
;; Source: Course Presentations.
;; Difficulties: Think about covering all the options required in this exercise manual.
;; Time: It took about 1 hour.

<MAE> ::= {seq <SAE>}                   ;(1)


<SAE> ::= <AE>                          ;(2)
        | {set <AE>} <NAE>              ;(3)
        | {set <AE>} <CAE> <NAE>        ;(4)


<CAE> ::= {set <NAE>}                   ;(5)
        | {set <NAE>} <CAE>             ;(6)


<AE> ::= <num>                          ;(7)
       | {+ <AE> <AE>}                  ;(8)
       | {- <AE> <AE>}                  ;(9)
       | {* <AE> <AE>}                  ;(10)
       | {/ <AE> <AE>}                  ;(11)

 
<NAE> ::= <num>                         ;(12)
         | {+ <NAE> <NAE>}              ;(13)
         | {- <NAE> <NAE>}              ;(14)
         | {* <NAE> <NAE>}              ;(15)
         | {/ <NAE> <NAE>}              ;(16)
         | get                          ;(17)

;; ID: 308019306

;; *First "MAE" Expression: Default Case
;; {seq {- 3 1}}
<MAE>                           ;           ==>     
   {seq <SAE>}                  ;(1)        ==>     {seq <SAE>}
   <SAE>                        ;           ==>
      <AE>                      ;           ==>     {seq <AE>}
         {- <AE> <AE>}          ;(9)        ==>     {seq {- <AE> <AE>}}
         {- <num> <AE>}         ;(7)        ==>     {seq {- <num> <AE>}}
         {- 3     <AE>}         ;           ==>     {seq {- 3     <AE>}}
         {- 3     <num>}        ;(7)        ==>     {seq {- 3     <num>}}
         {- 3     1}            ;           ==>     {seq {- 3     1}}


;; *Second "MAE" Expression:
;; {seq {set {- 9 8}}
;;      {/ get get}}
<MAE>                           ;           ==>
   {seq <SAE>}                  ;(1)        ==>     {seq <SAE>}
   <SAE>                        ;           ==>
      {set <AE>} <NAE>          ;(3)        ==>     {seq {set <AE>} <NAE>}
      <AE>                      ;           ==>
         {- <AE> <AE>}          ;(9)        ==>     {seq {- <AE> <AE>} <NAE>}
         {- <num> <AE>}         ;(7)        ==>     {seq {- <num> <AE>} <NAE>}
         {- 9     <AE>}         ;           ==>     {seq {- 9     <AE>} <NAE>}
         {- 9     <num>}        ;(7)        ==>     {seq {- 9     <num>} <NAE>}
         {- 9     8}            ;           ==>     {seq {- 9     8} <NAE>}
      <NAE>                     ;           ==>
         {/ <NAE> <NAE>}        ;(16)       ==>     {seq {- 9     8} {/ <NAE> <NAE>}}
         {/ get <NAE>}          ;(17)       ==>     {seq {- 9     8} {/ get   <NAE>}}
         {/ get get}            ;(17)       ==>     {seq {- 9     8} {/ get   get}}


;; *Third "MAE" Expression: Classic Case
;; {seq {set {+ 30 193}}
;;      {set {* get get}}
;;      {/ get 80}}
<MAE>                           ;           ==>
   {seq <SAE>}                  ;(1)        ==>     {seq <SAE>}
   <SAE>                        ;           ==>
      {set <AE>} <CAE> <NAE>    ;(4)        ==>     {seq {set <AE>} <CAE> <NAE>}
      <AE>                      ;           ==>
         {+ <AE> <AE>}          ;(8)        ==>     {seq {set {+ <AE> <AE>}} <CAE> <NAE>}
         {+ <num> <AE>}         ;(7)        ==>     {seq {set {+ <num> <AE>}} <CAE> <NAE>}
         {+ 30 <AE>}            ;           ==>     {seq {set {+ 30 <AE>}} <CAE> <NAE>}
         {+ 30 <num>}           ;(7)        ==>     {seq {set {+ 30 <num>}} <CAE> <NAE>}
         {+ 30 193}             ;           ==>     {seq {set {+ 30 193}} <CAE> <NAE>}
      <CAE>                     ;           ==>
         {set <NAE>}            ;(5)        ==>     {seq {set {+ 30 193}} {set <NAE>} <NAE>}
         <NAE>                  ;           ==>
            {* <NAE> <NAE>}     ;(15)       ==>     {seq {set {+ 30 193}} {set {* <NAE> <NAE>}} <NAE>}
            {* get   <NAE>}     ;(17)       ==>     {seq {set {+ 30 193}} {set {* get <NAE>}} <NAE>}
            {* get   get}       ;(17)       ==>     {seq {set {+ 30 193}} {set {* get get}} <NAE>}
      <NAE>                     ;           ==>
         {/ <NAE> <NAE>}        ;(16)       ==>     {seq {set {+ 30 193}} {set {* get get}} {/ <NAE> <NAE>}}
         {/ get   <NAE>}        ;(17)       ==>     {seq {set {+ 30 193}} {set {* get get}} {/ get <NAE>}}
         {/ get   <num>}        ;(12)       ==>     {seq {set {+ 30 193}} {set {* get get}} {/ get <num>}}
         {/ get   80}           ;           ==>     {seq {set {+ 30 193}} {set {* get get}} {/ get 80}}

|#


#|
;; Ex2_Q3 :

;; Sources:
;  Specifying Types (Conversion type variable from Any to Number): https://docs.racket-lang.org/ts-guide/more.html
;  Pairs and Lists (About foldl and lambda):
;  https://docs.racket-lang.org/reference/pairs.html?q=map#%28def._%28%28lib._racket%2Fprivate%2Fmap..rkt%29._map%29%29
;; Difficulties: Knowing the foldl, map and incorporating the Lambda, and learning to convert type variable in Lambda's variables,
;  which gets Any types and for this exercise, we were supposed to convert to a Number.
;; Time: It took about 40 minutes.

|#



;; The purpose of function "square": Return the number multiplied by itself (square).
;; The function "square" takes a number called num (Number).
;; The function "square" return a number (Number) which is the square number (num*num).
;; **Note: Tests and the example present later in this file.
(: square : Number -> Number)
(define (square num)
  (* num num)
  )

;; tests:
;(test (square 0) => 0)
;(test (square 1) => 1)
;(test (square 2) => 4)
;(test (square 3) => 9)


;; The purpose of function "sum-of-squares": Return the square sum of all the numbers in the list.
;; The function "sum-of-squares" takes a list of numbers (Listof Number).
;; The function "sum-of-squares" produces a number (Number) which is the sum of the squares of all of the numbers (With the help of Lambda)
;; in the list.
;; The "sum-of-squares" function is helped by the "square" function, which calculates the number multiplied by itself (square).
;; **Note: Tests and the example present later in this file.
(: sum-of-squares : (Listof Number) -> Number)
(define (sum-of-squares lst)
   (foldl (lambda ([a : Number] [result : Number])
           (+ result (square a)))
         0
         lst))


;; tests:
;(test (sum-of-squares '()) => 0)
;(test (sum-of-squares '(0)) => 0)
;(test (sum-of-squares '(0 0)) => 0)
;(test (sum-of-squares '(-0)) => 0)
;(test (sum-of-squares '(1 2 3)) => 14)
;(test (sum-of-squares '(1 -2 3)) => 14)
;(test (sum-of-squares '(-1 2 -3)) => 14)
;(test (sum-of-squares '(-1 -2 -3)) => 14)

#|
;; Example of "sum-of-squares" :
(sum-of-squares '(1 2 3)) ==> (sum-of-squares (lambda ('(1 2 3) , result=0) (0 + square(1))) ==>
(sum-of-squares (lambda ('(1 2 3) , result=0) (0 + square(1*1))) ==> (sum-of-squares (lambda ('(1 2 3) , result=0) (0 + 1)) ==>
(sum-of-squares (lambda ('(1 2 3) , result=1) (1 + square(2))) ==> (sum-of-squares (lambda ('(1 2 3) , result=1) (1 + (2*2))) ==>
(sum-of-squares (lambda ('(1 2 3) , result=1) (1 + 4)) ==> (sum-of-squares (lambda ('(1 2 3) , result=5) (5 + square(3))) ==>
(sum-of-squares (lambda ('(1 2 3) , result=5) (5 + (3*3))) ==> (sum-of-squares (lambda ('(1 2 3) , result=5) (5 + 9)) ==>
(sum-of-squares (lambda ('(1 2 3) , result=14) (14)) ==> result = 14
|#



;; Ex2_Q4 : Typed Racket (and more H.O. functions)


;; Ex2_Q4 (a) :
;; Defenition to binary tree (BINTREE) with two variants called :
;; Leaf holding a number (Number)
;; A Node that contains a binary tree (BINTREE) on the left and one on the right
;; Time: It took about 5 minutes.
(define-type BINTREE
  [Leaf Number]
  [Node BINTREE BINTREE])

;; tests of Ex2_Q4 (a):
;(test (Leaf 0) => (Leaf 0))
;(test (Node (Leaf 0) (Leaf 1)) => (Node (Leaf 0) (Leaf 1)))
;(test (Node (Leaf 0) (Node (Leaf 0) (Leaf 1) )  ) => (Node (Leaf 0) (Node (Leaf 0) (Leaf 1) )  ))
;(test (Node (Node (Leaf 0) (Leaf 1) ) (Leaf 1))   => (Node (Node (Leaf 0) (Leaf 1) ) (Leaf 1)) )
;(test (Node (Node (Leaf 0) (Leaf 1) ) (Node (Leaf 0) (Leaf 1) )  ) => (Node (Node (Leaf 0) (Leaf 1) ) (Node (Leaf 0) (Leaf 1) )  ))

#|
;; Ex2_Q4 (b)+(c)+(d)+(e)+(f) :
;; Sources:
;  Kinds of numeric functions:
;  https://docs.racket-lang.org/reference/generic-numbers.html?q=add1#%28def._%28%28quote._~23~25kernel%29._add1%29%29
;  Info about high-order functions.
;  https://docs.racket-lang.org/htdp-langs/advanced.html?q=higher-order#%28part._htdp-advanced._.Higher-.Order_.Functions%29
;; Difficulties: Understand the requirements of the question and the beginning of its implementation (mainly through the calculation at first).
;; Time: It took about 2 hours.
|#

;; The purpose of function "tree-map": Get a binary tree (Node, or Leaf), and return it in the same shape only with changing
;; the values of the leaves (Leaf left and Leaf right) with the numerical function f (which we also get).
;; The function "tree-map" takes in a numeric function f called f (Number -> Number) and a binary tree called b-tree (BINTREE).
;; The function "tree-map" returns :
;; case #1: If b-tree is a Leaf -> return Leaf with using f(n) in his value.
;; case #2: If b-tree is a Node with left and right binary tree (or leaf of binary tree) -> return a tree with the same shape of the
;; but with using function f for values in its leaves recursively (on the function tree-map, see the examples below for this file)
;; **Note: tree-map is a (higher-order) function and tests and the examples are shown later in this file.
(: tree-map : (Number -> Number) BINTREE -> BINTREE)
(define (tree-map f b-tree)
  (cases b-tree
    [(Leaf num) (Leaf (f num))]
    [(Node l r) (Node (tree-map f l) (tree-map f r)) ]
  ))

;; tests :
;  Add1 :
;(test (tree-map add1 (Leaf 1) ) => (Leaf 2))
;(test (tree-map add1 (Node (Leaf 0) (Leaf 1))) => (Node (Leaf 1) (Leaf 2)))
;(test (tree-map add1 (Node (Leaf 1) (Node (Leaf 2) (Leaf 3)))) => (Node (Leaf 2) (Node (Leaf 3) (Leaf 4))))
;(test (tree-map add1 (Node (Node (Leaf 0) (Leaf 1) ) (Leaf 1))) => (Node (Node (Leaf 1) (Leaf 2) ) (Leaf 2)))
;(test (tree-map add1 (Node (Node (Leaf 0) (Leaf 1) ) (Node (Leaf 0) (Leaf 1))) ) => (Node (Node (Leaf 1) (Leaf 2) ) (Node (Leaf 1) (Leaf 2) ) ))
;  Sub1 :
;(test (tree-map sub1 (Leaf 1) ) => (Leaf 0))
;(test (tree-map sub1 (Node (Leaf 0) (Leaf 1))) => (Node (Leaf -1) (Leaf 0)))
;(test (tree-map sub1 (Node (Leaf 1) (Node (Leaf 2) (Leaf 3)))) => (Node (Leaf 0) (Node (Leaf 1) (Leaf 2))))
;(test (tree-map sub1 (Node (Node (Leaf 0) (Leaf 1) ) (Leaf 1))) => (Node (Node (Leaf -1) (Leaf 0) ) (Leaf 0)))
;(test (tree-map sub1 (Node (Node (Leaf 0) (Leaf 1) ) (Node (Leaf 0) (Leaf 1))) ) => (Node (Node (Leaf -1) (Leaf 0) ) (Node (Leaf -1) (Leaf 0) ) ))
;  Abs :
;(test (tree-map abs (Leaf -1) ) => (Leaf 1))
;(test (tree-map abs (Node (Leaf -2) (Leaf -1))) => (Node (Leaf 2) (Leaf 1)))
;(test (tree-map abs (Node (Leaf -1) (Node (Leaf -2) (Leaf -3)))) => (Node (Leaf 1) (Node (Leaf 2) (Leaf 3))))
;(test (tree-map abs (Node (Node (Leaf -0) (Leaf -1) ) (Leaf 1))) => (Node (Node (Leaf 0) (Leaf 1) ) (Leaf 1)))
;(test (tree-map abs (Node (Node (Leaf 0) (Leaf -1) ) (Node (Leaf -0) (Leaf -1))) ) => (Node (Node (Leaf 0) (Leaf 1) ) (Node (Leaf 0) (Leaf 1) ) ))
;  Round and Floor :
;(test (tree-map round (Leaf 17/4) ) => (Leaf 4))
;(test (tree-map round (Node (Leaf -17/4) (Leaf 1/2))) => (Node (Leaf -4) (Leaf 0)))
;(test (tree-map floor (Node (Leaf -1.5) (Leaf 0.5))) => (Node (Leaf -2.0) (Leaf 0.0)))

#|
Example of Ex2_Q4 (b)+(c) :
tree-map(f = add1, (Node (Leaf 1) (Leaf 2)) ) ==> tree-map(f = add1, (Leaf 1)) && tree-map(f = add1, (Leaf 2))
==> tree-map(f = add1, (Leaf (f(1)= 2))) && tree-map(f = add1, (Leaf (f(2)= 3))) ==> tree-map(f = add1, (Node (Leaf 2) (Leaf 3)) )
|#



;; Ex2_Q4 (b)+(c)+(d) :
;; The purpose of function "tree-fold": To activate the numeric function (leaf-f) on the binary tree leaves (or leaf, case 1) and combine the 2
;; sub-trees (case 2) to one tree by activating the combiner function (comb-f), and of course return a type A value of "tree-fold".
;; The function "tree-fold" takes three values: The combiner function called comb-f (a function of two arguments, two results for the
;; two subtrees (A A -> A), some input of type A), the leaf function called leaf-f (a numeric function of a single number argument)
;; and the BINTREE value to process.
;; The function "tree-fold" returns (type A value of "tree-fold"):
;; case #1: If binary tree called b-tree (BINTREE) is a Leaf => return leaf-f(value of Leaf, num)
;; case #2: If b-tree is a Node with left and right leaves => return comb-f( leaf-f(value of left leaf), leaf-f(value of right leaf))
;; Additional: The function "tree-fold" exerts a numeric function on the leaves and combines the 2 sub-trees recursively.
;; **Note: "tree-fold" is a polymorphic function, so its type should be parameterized over “some input type A”.
;;         Tests and the examples are shown later in this file.


(: tree-fold : (All (A) (A A -> A) (Number -> A) BINTREE -> A))
(define (tree-fold comb-f leaf-f b-tree)
  (cases b-tree
    [(Leaf num) (leaf-f num)]
    [(Node l r) (comb-f (tree-fold comb-f leaf-f l) (tree-fold comb-f leaf-f r))]
  ))

;; tests of Ex2_Q4 (b)+(c)+(d) :
;(test (tree-fold - add1 (Leaf 1)) => 2 )
;(test (tree-fold - add1 (Node (Leaf 0) (Leaf 1) )) => -1 )
;(test (tree-fold - sub1 (Node (Leaf 1) (Leaf 0) )) => 1 )
;(test (tree-fold + sub1 (Node (Leaf 0) (Leaf 1) )) => -1 )

#|
Example of Ex2_Q4 (b)+(c)+(d) :
;; A combiner function called "comb-f" (Useful in cases where there are at least 2 sub-trees) = "-"
;; A numeric function called "leaf-f" = "add1"
(tree-fold - add1 (Node (Leaf 0) , (Leaf 1) ) ) ==> (- (tree-fold - add1 (Leaf 0) ) , (tree-fold - add1 (Leaf 1) ) ) ==>
(- (tree-fold - add1 (add1(0) = 0+1 = 1 ) ) , (tree-fold - add1 (add1(1) = 1+1 = 2 ) ) ) ==> 1 - 2 (left leaf - right leaf) ==> -1

|#


;; Ex2_Q4 (e)+(f) :
;; The purpose of function "tree-flatten": To flattens a binary tree to a list of its values in left-to-right order.
;; The function "tree-flatten" consumes a BINTREE.
;; The function "tree-flatten" returns a list of BINTREE values from left to right.
;; Additional: The function "tree-flatten" uses the "tree-fold" function to return the list of BINTREE values.
;; **Note: Tests and the examples are shown later in this file.
(: tree-flatten : BINTREE -> (Listof Number))
;; flattens a binary tree to a list of its values in
;; left-to-right order
(define (tree-flatten tree)
  (tree-fold (inst append Number) (inst list Number) tree))

;; tests of Ex2_Q4 (e)+(f) :
;(test (tree-flatten (Leaf 0)) => '(0) )
;(test (tree-flatten (Node (Leaf 0) (Leaf 1) ) ) => '(0 1) )
;(test (tree-flatten (Node (Leaf 1) (Node (Leaf 2) (Leaf 3))) ) => '(1 2 3) )
;(test (tree-flatten (Node (Node (Leaf 2) (Leaf 3)) (Leaf -1)) ) => '(2 3 -1) )
;(test (tree-flatten (Node (Node (Leaf 2) (Leaf 3)) (Node (Leaf 2) (Leaf 3))) ) => '(2 3 2 3) )

#|
Example of Ex2_Q4 (e)+(f) :
;; A has become a list type of numbers (Listof Numbers) (to the best of our understanding from reading the assignment's instructions,
;; understanding the behavior of the "tree-flatten" function, and testing the "tree-flatten" function that actually reinforces our claim).
;; **Note: the use of (inst f Number) — the Typed Racket inference has certain limitations that prevent it from inferring the correct type.
;;         We need to ‘help’ it in these cases, and say explicitly that we use the two polymorphic functions append and list instantiated
;;         with Number.
;;        (Think about an (All (A) ...) type as a kind of a function at the type world, and inst is similar to calling such a
;;        function with an argument for A.)

(tree-flatten (Node (Leaf 0) (Leaf 1) ) ) ==> (tree-fold (inst append Number) (inst list Number) (Node (Leaf 0) (Leaf 1) )) ==>
(tree-fold append list (Node (Leaf 0) (Leaf 1)) ) ==> (append (tree-fold append list (Leaf 0) ) (tree-fold append list (Leaf 1) ) ) ==>
(append (tree-fold append list (list 0) ) (tree-fold append list (list 1) ) ) ==> (append (list 0) (list 1) ) ==> '(0 1)

|#

;; Ex2_Q4 (g):
;; The purpose of function "tree-reverse": To viewing the tree in reverse.
;; The function "tree-reverse" consumes a tree (BINTREE).
;; The function "tree-reverse" return a tree (BINTREE) that is its mirror image.
;; Additional: We use function "tree-fold" to define a function "tree-reverse", that using a one-line helper function called "switch-nodes".
;; Difficulties: Understanding the introduction of variables into the "tree-fold" function and some thinking
;;               in the one-line implementation of the switch-nodes function.
;; Time: It took about 15-20 minutes.
;; **Note: Tests and the examples are shown later in this file.

(: tree-reverse : BINTREE -> BINTREE)
(define (tree-reverse b-tree)
  (tree-fold switch-nodes Leaf b-tree) )

;; The purpose of function "switch-nodes": To turn the sides of the 2 sub-trees (in fact, the same Revers action).
;; The function "switch-nodes" consumes a 2 trees (BINTREE) called "l" (left tree or sub-tree) and "r" (right tree or sub-tree).
;; The function "switch-nodes" return a tree (BINTREE) that is its turn the sides of the 2 trees (or sub-trees), left and right sides.
;; In fact, turns the sides of both trees (the left turns to the right and the other the opposite).
(: switch-nodes : BINTREE BINTREE -> BINTREE)
(define (switch-nodes l r)
  (Node r l) )

;; tests of Ex2_Q4 (g) :
;; tests of "switch-nodes" :
;(test (switch-nodes (Leaf 0) (Leaf 1)) => (Node (Leaf 1) (Leaf 0) ) )
;(test (switch-nodes (Node (Leaf 0) (Leaf 1)) (Node (Leaf 1) (Leaf 0))) => (Node (Node (Leaf 1) (Leaf 0)) (Node (Leaf 0) (Leaf 1)) ) )
;; tests of "tree-reverse" :
;(test (tree-reverse (Leaf 0)) => (Leaf 0) )
;(test (tree-reverse (Node (Leaf 0) (Leaf 1) )) => (Node (Leaf 1) (Leaf 0) ) )
;(test (tree-reverse (Node (Leaf 1) (Node (Leaf 2) (Leaf 3))) ) => (Node (Node (Leaf 3) (Leaf 2)) (Leaf 1)) )
;(test (tree-reverse (Node (Node (Leaf 3) (Leaf 2)) (Leaf 1)) ) => (Node (Leaf 1) (Node (Leaf 2) (Leaf 3))) )
;(test (tree-reverse (Node (Node (Leaf 3) (Leaf 2)) (Node (Leaf 2) (Leaf 3))) ) => (Node (Node (Leaf 3) (Leaf 2)) (Node (Leaf 2) (Leaf 3))) )
;; General tests for any tree t, the following equation holds: "(equal? (reverse (tree-flatten t)) (tree-flatten (tree-reverse t)))"
;(test (equal? (reverse (tree-flatten (Leaf 0) )) (tree-flatten (tree-reverse (Leaf 0) ))))
;(test (equal? (reverse (tree-flatten (Node (Leaf 0) (Leaf 1) ))) (tree-flatten (tree-reverse (Node (Leaf 0) (Leaf 1) )))))
;(test (equal? (reverse (tree-flatten (Node (Leaf 1) (Node (Leaf 2) (Leaf 3))))) (tree-flatten (tree-reverse (Node (Leaf 1) (Node (Leaf 2) (Leaf 3)))))))
;(test (equal? (reverse (tree-flatten (Node (Node (Leaf 2) (Leaf 3)) (Leaf 1)))) (tree-flatten (tree-reverse (Node (Node (Leaf 2) (Leaf 3)) (Leaf 1))))))

#|
;; Example of Ex2_Q4 (g) :
;; Example of "switch-nodes" :
(switch-nodes (Leaf 0) (Leaf 1)) ==> (Node [left sub-tree: (Leaf 0) <--> right sub-tree:(Leaf 1)]) ==> (Node (Leaf 1) (Leaf 0))

;; Example of "tree-reverse" :
(tree-reverse (Node (Leaf 0) (Leaf 1) )) ==> (tree-fold switch-nodes Leaf (Node (Leaf 0) (Leaf 1) )) ==>
(switch-nodes (tree-fold switch-nodes Leaf (Leaf 0)) (tree-fold switch-nodes Leaf (Leaf 1)) ) ==> (switch-nodes (Leaf 0) (Leaf 1) ) ==>
(Node [left sub-tree: (Leaf 0) <--> right sub-tree:(Leaf 1)]) ==> (Node (Leaf 1) (Leaf 0))

|#
