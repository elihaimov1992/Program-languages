#lang pl



;; q1:
;; The purpose of function "plSuffixContained": To check if exists String in the list of strings (Listof String)
;; that Whose extension ends in "pl" letters".
;; The function "plSuffixContained" takes a list of strings (Listof String).
;; The function "plSuffixContained" returns the first String that Whose extension ends in "pl" letters or Boolean value #f (false)
;; if not exists.
;; Note: The detail on this function is later in this file.

#|
EX1_Q1:

;;Description:
The Function "plSuffixContained" takes a list of strings (Listof String) and returns: 
– the first string that contains the string "pl" as a suffix (String)
- otherwise, returns #f (Boolean)

Expansion:
The function "plSuffixContained" consumes a list of string (lst) and cheks:
- First condition: if a list is empty? (or null?) if true -> return #f (Boolean).
- Second condition: "plSuffixContained" helped by helper function ("helper-q1", that it roll is take a String (first lst)
and return Boolean (#t - if suffix of a String is "pl", #f - otherwise).
- Third condition: "plSuffixContained" using in recursion-tail for go to the next String on the list (rest lst)
until it finds the String that contains a String "pl" as a suffix or gets to the end of the list of strings and returns #f (Boolean).

;;Examples:
plSuffixContained() => #f
plSuffixContained '("yyyt" "TplT" "plTT" "PlPl" "plplpl") => "plplpl"
Note: This function works assuming (according to the exercise) that it only gets a list of strings (Listof String)
or empty (or null) List.

;;Source: I helped students in the Whatsapp group of the course.
;;The most Racket tools: Declaration, definition, string-length, U, string-ref, eq?, cond and tail-recursion.
;;Time for this exercise: It took About 3.5 hours (The main difficulty was in returning the value (Union of the types),
calculating a way to find the last two characters of String ("p" and "l") and to use recursion and helper function).

|#

;;Declaration and difinition of helper function
(  :  helper-q1  :  String -> Boolean)
( define ( helper-q1 str)
  ( cond
    [( and (>= ( string-length str) 2) ( eq? ( string-ref str (- ( string-length str) 2)) #\p) ( eq? ( string-ref str (- ( string-length str) 1)) #\l)) #t]
    [ else  #f]) )

;;Declaration and difinition of plSuffixContained function
(  : plSuffixContained : ( Listof String)  -> ( U String Boolean))
( define ( plSuffixContained lst)
    ( cond
      [( null?  lst) #f]
      [( helper-q1 ( first lst)) ( first lst)]
      [ else ( plSuffixContained ( rest lst))]))

#|
;;Tests for q1:
(test (plSuffixContained '()) => #f)
(test (plSuffixContained '("ppllp" "pplyy" "ppp" "lpTT" "lol")) => #f)
(test (plSuffixContained '("pllp" "pplyy" "ppp" "lpTT" "lol")) => #f)
(test (plSuffixContained '("llp" "pplyy" "ppp" "lpTT" "pllolpl")) => "pllolpl")
(test (plSuffixContained '("yyyt" "TplT" "plTT" "PlPl" "plplpl")) => "plplpl")
(test (plSuffixContained '("yyytpl" "TplT" "plTT" "PlPl" "plplpl")) => "yyytpl")
|#


;; q2.1:
;; The purpose of function "write-poly": To get a list of numbers and write the polynomial String.
;; The function "write-poly" takes a list of coefficients (Listof Number).
;; The function "write-poly" returns the polynomial (in a reversed order of coefficients) (String)
;; or "" (if take empty list)(String).
;; Note: The detail on this function is later in this file.

#|
EX1_2

EX1_Q2.1:

;;Description:
The Function write-poly takes a list of coefficients ("coeffs", Listof Numbers)
and returns: 
– the polynomial (in a reversed order of coefficients) "a1x^n+a2x^n-1+⋯+an".
- otherwise, returns "".

;;Expansion:
The Function write-poly helped by helper function (as a loop), "poly-helper", that consumes list of Numbers ("coeffLst", Listof Numbers),
argument/parameter "x" (x, String), power ("power", String) and assign parameter acc (String), that contain
acctualy polynomial String and using in tail-recursion and checking the following conditions (Cond):
(Which length of a list is 1)
- First Cond: if |List|=0 -> return acc (or "" if list is null)
- Second Cond: if |List|=1 && a1=0 && acc = "" -> return acc * "0"
- Third Cond: if |List|=1 && a1=0 -> return acc (or "" if list is null)
- Fourth Cond: if |List|=1 && a1<0 -> appends a acc to a1 without adding an arithmetic sign (adding or subtraction)
at the end of the current String and return it.
- Fifth Cond: if |List|=1 -> appends a acc to a1 without adding an arithmetic sign (adding or subtraction)
at the end of the current String and return it.

(Which length of a list is 2 or more - using in tail-recursion)
- Sixth Cond: if |List|>=2 && a1=0 && a2>0 && |acc|>=2 -> return poly-helper( (a2,..,an), x , power-1 , acc * "+" )
- Seventh Cond: if |List|=2 && a1>0 && a2<=0 -> return poly-helper( (a2,..,an), x , power-1 , acc * "a1" * "x" )
- Eighth Cond: if |List|=2 && a1<0 && a2<=0 -> return poly-helper( (a2,..,an), x , power-1 , acc * "a1" * "x" )
- Ninth Cond: if |List|>=2 && a1=0 -> return poly-helper( (a2,..,an), x , power-1 , acc )
- Tenth Cond: if |List|=2 -> return poly-helper( (a2,..,an), x , power-1 , acc * "a1" * "x" * "+" )
- Eleventh Cond: if |List|>2 && a2<=0 -> return poly-helper( (a2,..,an), x , power-1 , acc * "a1" * "x" * "^" * "power" )
- Otherwise, return poly-helper( (a2,..,an), x , power-1 , acc * "a1" * "x" * "^" * "power" * "+" )

;;Example:
write-poly('()) => ""
write-poly ('(3 2 6)) => "3x^2+2x+6"
{
  write-poly ('(3 2 6)) => poly-helper('(3 2 6),"x", 2 , "") => acc = "" * "3" * "x" * "^" * "2" * "+" =>
  poly-helper('(2 6),"x", 1 , "3x^2+") => acc = "3x^2+" * "2" * "x" * "+" => poly-helper('(6),"x", 0 , "3x^2+2x+")
  => acc = "3x^2+2x+" * "6" => poly-helper('(),"x", -1 , "3x^2+2x+6") => acc = "3x^2+2x+6" * "" => "3x^2+2x+6"
}
Note: This function works assuming (according to the exercise) that it only gets a list of numbers (Listof Number)
or empty (or null) List.

;;source:
I helped students in the Whatsapp group of a course.
Source sites of tail-recursion:
https://docs.racket-lang.org/guide/Lists__Iteration__and_Recursion.html?q=index%20string#%28part._tail-recursion%29
https://stackoverflow.com/questions/55810809/create-polynomial-function
https://php.developreference.com/article/24135453/calculate+polynomial+function+%5Bscheme+racket%5D
https://groups.google.com/forum/#!topic/racket-users/K_p0iawn19Y

;;The most Racket tools: "foramt ~a.."(parse from number to string), tail-recursion, Cond, length, length-string.
;;Time for this exercise: 5 hours. (The main difficulty was converting the number to String,
thinking of all the end cases (listed in explanation and conditions) and addressing the sign of the coefficient (minus or plus)).

|#

(: write-poly : ( Listof Number) -> (U String Boolean))
(define (write-poly coeffs)
  (: poly-helper : (Listof Number) String Integer String -> String)
  (define (poly-helper coeffLst x power acc)
    (cond
      [(= (length coeffLst) 0) acc]
      [(and (= (length coeffLst) 1) (= (first coeffLst) 0) (eq? acc "")) (string-append acc "0")]
      [(and (= (length coeffLst) 1) (= (first coeffLst) 0)) acc]
      [(and (= (length coeffLst) 1) (< (first coeffLst) 0)) ( string-append acc ( format "~a" ( first coeffLst)))]
      [(= (length coeffLst) 1) ( string-append acc ( format "~a" ( first coeffLst)))]

      [(and (>= (length coeffLst) 2) (= (first coeffLst) 0) (> (first (rest coeffLst)) 0) (>= (string-length acc) 2)) (poly-helper (rest coeffLst) x (- power 1) (string-append acc "+"))]
      [(and (= (length coeffLst) 2) (> (first coeffLst) 0) (<= (first (rest coeffLst)) 0)) (poly-helper (rest coeffLst) x (- power 1) ( string-append acc ( format "~a" ( first coeffLst)) x))]
      [(and (= (length coeffLst) 2) (< (first coeffLst) 0) (<= (first (rest coeffLst)) 0)) (poly-helper (rest coeffLst) x (- power 1) ( string-append acc ( format "~a" ( first coeffLst)) x))]

      [(and (>= (length coeffLst) 2) (= (first coeffLst) 0)) (poly-helper (rest coeffLst) x (- power 1) acc)]
      [(= (length coeffLst) 2) (poly-helper (rest coeffLst) x (- power 1) ( string-append acc ( format "~a" ( first coeffLst)) x "+"))]

      [(and (> (length coeffLst) 2) (<= (first (rest coeffLst)) 0)) (poly-helper (rest coeffLst) x (- power 1) ( string-append acc ( format "~a" ( first coeffLst)) ( string-append x "^" ( format "~a" power))))]
      [ else (poly-helper (rest coeffLst) x (- power 1) ( string-append acc ( format "~a" ( first coeffLst)) ( string-append x "^" ( format "~a" power)) "+"))]))
  (poly-helper coeffs "x" (- (length coeffs) 1) ""))
      
#|
;;Tests for q2.1:
(test (write-poly '(3 2 6)) => "3x^2+2x+6")
(test (write-poly '()) => "")
(test (write-poly '(7 8 9 10)) => "7x^3+8x^2+9x+10")
(test (write-poly '(-1)) => "-1")
(test (write-poly '(0)) => "0")
(test (write-poly '(-3 -2)) => "-3x-2")
(test (write-poly '(0 0 0)) => "0")
(test (write-poly '(1 0 0)) => "1x^2")
(test (write-poly '(1 0 1)) => "1x^2+1")
(test (write-poly '(0 0 1)) => "1")
(test (write-poly '(0 1 0)) => "1x")
(test (write-poly '(3 -2 1)) => "3x^2-2x+1")
(test (write-poly '(-3 -2 0)) => "-3x^2-2x")
(test (write-poly '(3 2 -1)) => "3x^2+2x-1")
(test (write-poly '(-0 -0 0)) => "0")
(test (write-poly '(-3 -2 -1)) => "-3x^2-2x-1")
(test (write-poly '(1 0 0 1)) => "1x^3+1")
(test (write-poly '(1 0 1 0)) => "1x^3+1x")
(test (write-poly '(1 1 0 1)) => "1x^3+1x^2+1")
(test (write-poly '(-1 0 0 -1)) => "-1x^3-1")
(test (write-poly '(-1 0 -1 0)) => "-1x^3-1x")
(test (write-poly '(-1 -1 0 1)) => "-1x^3-1x^2+1")
(test (write-poly '(1 0 0 -1)) => "1x^3-1")
(test (write-poly '(0 -1)) => "-1")
(test (write-poly '(0 0 -1)) => "-1")
(test (write-poly '(0 -1 0)) => "-1x")
|#

;; q2.2:
;; The purpose of function "compute-poly": To get a elment of variable "x" and list of numbers and returns calculation of the polynomial.
;; The function "compute-poly" takes a number x (Number) and list of coefficients (Listof Number).
;; The function "compute-poly" returns the calculation of the polynomial (Number).
;; Note: The detail on this function is later in this file.

#|
EX1_Q2.2:

;;Description:
The Function "compute-poly" takes a number x ("argX", Number) and a list of coefficients ("coeffs2", Listof Numbers)
a1,..,an and returns:
- 0, if length of a list of coefficients is empty or null.
- Otherwise, return the result of the polynomial (again, in a reversed order of coefficients) a1x^n+a2x^(n-1)+..+an .

;;Expansion:
The Function "compute-poly" helped by helper function (as a loop), "poly-helper2", that consumes list of Numbers ("coeffLst2", Listof Numbers),
argument/parameter "argX" (x, Number), power ("power2", Integer) and assign parameter acc (Number), that contain
acctualy polynomial compute and "poly-helper2" using in tail-recursion and checking the following conditions (Cond):
- First Cond: if |List|=0 -> 0
- Otherwise, return poly-helper2( (a2,..,an) , argX , |List|-1 , acc2 )

;;Example:
compute-poly(2 , '()) => 0
compute-poly(2 , '(3 2 6)) => 22
{
  compute-poly (2 , '(3 2 6)) => poly-helper2('(3 2 6), 2 , 2 , 0) => acc2 = 0+(3*(2)^2) = 12 =>
  poly-helper('(2 6), 2 , 1 , 12) => acc = 12+(2*(2)^1) = 16 => poly-helper('(6), 2 , 0 , 16)
  => acc = 16+(6*(2)^0) = 22 => poly-helper('(), 2 , 0 , 22) => acc = 22+(0*(2)^(0)) = 22
}
Note: This function works assuming (according to the exercise) that it only gets a Number (for x) and list of numbers (Listof Number)
or empty (or null) List.

;;source:
I helped students in the Whatsapp group of a course.
Source sites of tail-recursion and expt:
https://docs.racket-lang.org/guide/Lists__Iteration__and_Recursion.html?q=index%20string#%28part._tail-recursion%29
https://stackoverflow.com/questions/55810809/create-polynomial-function
https://php.developreference.com/article/24135453/calculate+polynomial+function+%5Bscheme+racket%5D
https://groups.google.com/forum/#!topic/racket-users/K_p0iawn19Y
https://docs.racket-lang.org/reference/generic-numbers.html?q=expt#%28def._%28%28quote._~23~25kernel%29._expt%29%29

;;The most Racket tools: expt (which shortens all conditions compared to the previous section), tail-recursion, Cond, length.
;;Time for this exercise: 0.5 hours. (It took a little time to encode because I relied on the line from the previous section with
a few modifications such as expt, which saved all rows of conditions in the polynomial).

|#

(: compute-poly : Number ( Listof Number) -> (U Number Boolean))
(define (compute-poly argX coeffs2)
  (: poly-helper2 : (Listof Number) Number Integer Number -> Number)
  (define (poly-helper2 coeffLst2 x2 power2 acc2)
    (cond
      [(= ( length coeffLst2) 0) acc2]
      [(and (= ( length coeffLst2) 1) (= ( first coeffLst2) 0)) acc2]
      [else (poly-helper2 (rest coeffLst2) x2 (- power2 1) (+ acc2 (* (first coeffLst2) (expt x2 power2))))]
      ))
  (poly-helper2 coeffs2 argX (- (length coeffs2) 1) 0))

#|
;;Tests for q2.2:
(test (compute-poly 2 '()) => 0)
(test (compute-poly 2 '(3 2 6)) => 22)
(test (compute-poly 3 '(4 3 -2 0)) => 129)
(test (compute-poly -2 '(3 2 6)) => 14)
|#


;; q3.1 and q3.2:
;; The purpose of these section : Implement a keyed-stack data structure and push operation.
;; push operation take Symbol, String and Key-Stack.
;; push operation return Key-Stack.
;; Note: The detail on this function is later in this file.

#|
EX1_Q3.1_2:

;;EX1_Q3 (Generally)
Purpose: Implement a keyed-stack data structure.
Solution: To define a new type called "KeyStack" and
each element in the stack will be keyed (indexed) with a symbol (Symbol).

;;q3.1
Purpose: Implement the empty stack called "EmptyKS".
Solution: This should be a variant of the data type (constructor).

;;q3.2
Purpose: Implement the push operation called "Push".
Solution: This should be a variant of the data type and
take as input a symbol (key), a string (value), and an existing
keyed-stack and return an extended key-stack in the natural way.

sources:
I helped students in the Whatsapp group of a course.
Source sites of define-type:
https://docs.racket-lang.org/plai-typed/Definitions.html

;;The most Racket tools: define-type and variant (Understand what it is).
;;Time for this exercise: 1 hour (The main and difficult part of the above section was
understanding the problem and intent of the section).

|#

(define-type KeyStack
  [EmptyKS]
  [Push Symbol String KeyStack]
  )

#|
;;Tests for q3.1 and q3.2:
(test (EmptyKS) => (EmptyKS))
(test (Push 'b "B" (Push 'a "A" (EmptyKS))) => (Push 'b "B" (Push 'a "A" (EmptyKS))))
(test (Push 'a "AAA" (Push 'b "B" (Push 'a "A" (EmptyKS)))) => (Push 'a "AAA" (Push 'b "B" (Push 'a "A" (EmptyKS)))))
|#


;; q3.3:
;; The purpose of these section : Implement a search operation.
;; search operation take Symbol and Key-Stack.
;; search operation return first String that is keyed.
;; Note: The detail on this function is later in this file.

#|
EX1_Q3.3:

;;q3.3
Purpose: Implement the search operation called "search-stack".
Solution: This should take as input a symbol (key) and a keyed-stack and return the
first (LIFO, last in first out) value that is keyed accordingly.
If the key does not appear in the original stack, it should return a #f value.

;;Expansion:
The operation "search-stack" using in cases, cond and consumes a symbol (key, Symbol) and a KeyStack (keyed-stack)
and returns :
- if the key does not appear in the original stack -> return #f value.
- Otherwise, return first (LIFO, last in first out) value that is keyed accordingly.

;;Example:
search-stack('a , [((default: empty) #f)->('a , "A")->('b , "B")->('a , "AAA")]) => ('a)=('a) => "AAA" .

search-stack('c , [(default: empty)->('a , "A")->('b , "B")->('a , "AAA")]) => ('a)!=('c) =>
search-stack('c , [(default: empty)->('a , "A")->('b , "B")]) => ('b)!=('c) =>
search-stack('c , [(default: empty)->('a , "A")]) => ('a)!=('c) =>
search-stack('c , [(default: empty)]) => Symbol 'c does not appear in the original stack => #f .

;;sources:
I helped students in the Whatsapp group of a course.
Source site of cases:
https://docs.racket-lang.org/eopl/index.html?q=cases#%28form._%28%28lib._eopl%2Feopl..rkt%29._cases%29%29

;;The most Racket tools: cases, cond.
;;Time for this exercise: it took about 20 minutes (The main and hard thing about this section was
Think about what to code in else condition and understand what cases is and how it is used).

|#

( : search-stack : Symbol KeyStack -> (U String #f))
(define (search-stack key keyed-stack)
  (cases keyed-stack
    [(EmptyKS) #f]
    [(Push Symbol String KeyStack)
     (cond
       [( eq? Symbol key ) String]
       [ else (search-stack key KeyStack)])]))

#|
;;Tests for q3.3:
(test (search-stack 'a (Push 'a "AAA" (Push 'b "B" (Push 'a "A" (EmptyKS))))) => "AAA")
(test (search-stack 'c (Push 'a "AAA" (Push 'b "B" (Push 'a "A" (EmptyKS))))) => #f)
|#


;; q3.4:
;; The purpose of these section : Implement a pop operation.
;; pop operation take Key-Stack.
;; pop operation return Key-Stack withou the first String.
;; Note: The detail on this function is later in this file.

#|
EX1_Q3.4:

;;q3.4
Purpose: Implement the pop operation called "pop-stack".
Solution: This should take as input a keyed-stack and return the keyed-stack without its first (keyed) value.
If the original stack was empty, it should return a #f value.

;;Expansion:
The operation "pop-stack" using in tail-recursion, and consumes  a symbol (key, Symbol) and a KeyStack (keyed-stack)
and returns:
- if the original stack was empty -> return a #f value.
- Otherwise, return the keyed-stack without its first (keyed) value.

;;Example:
pop-stack([(default: empty)]) => #f  ;;Base case
pop-stack([(default: empty)->('a , "A")->('b , "B")->('a , "AAA")]) => [(default: empty)->('a , "A")->('b , "B")]

;;sources:
I helped students in the Whatsapp group of a course.
Source site of cases:
https://docs.racket-lang.org/eopl/index.html?q=cases#%28form._%28%28lib._eopl%2Feopl..rkt%29._cases%29%29

;;The most Racket tools: cases, cond.
;;Time for this exercise: It took about 15 minutes. (The main and hard was thing about this section was
Understand what and how to change from the previous section).

|#


( : pop-stack : KeyStack -> (U KeyStack #f))
(define (pop-stack keyed-stack)
  (cases keyed-stack
    [(EmptyKS) #f]
    [(Push Symbol String KeyStack) KeyStack]))

#|
;;Tests for q3.4:
(test (pop-stack (EmptyKS)) => #f)
(test (pop-stack (Push 'a "AAA" (Push 'b "B" (Push 'a "A" (EmptyKS))))) => (Push 'b "B" (Push 'a "A" (EmptyKS))))
|#


#|
EX1_Q4:
|#

(: is-odd? : Natural -> Boolean)
;; The purpose of function "is-odd?": To check if a variable is odd".
;; The function "is-odd?" take a number x (Natural).
;; The function "is-odd?" returns Boolean value.
;; Note: The detail on this function is later in this file.

#|

The function "is-odd?" takes a number x (Natural) and returns:
- if x=0 (=even) -> return #f value.
- Otherwise, going to function "is-even?" and send number "x-1" (Integer).

The purpose of this function is to check if the number is odd, by checking if the number x (Integer) is even:
- if true, the function return #f value.
- Otherwise, helped by global helper function called "is-even?" with sending parameter "x-1", until "is-even?"
returns the Boolean value #t .

;;My expansion (and conclusions):
Case #1: if x=0: "is-odd?" return #f value (Boolean), because number x is even.
Case #2: if x>0 and x is even: "is-odd?" will help by "is-even?" odd number of times (by sending the value x and reducing it by one,
in each transfer between these 2 functions) and finally get a reset value (equal to 0) from the "is-even?" function and
return the Boolean value #f (since it is an even value and the initial number, that "is-odd?" take, was even too).
Case #3: if x>0 and x is odd: "is-odd?" will be decided by the helper function "is-even?", by moving the number x
(and reducing it by one, per transfer) between the 2 functions (like as a circle) until the "is-even?" function receives a reset value (equal to zero)
and returns the Boolean value #t (because zero is indeed an even number but the initial value of the number x in function "is-odd?"
was odd - therefore a Boolean value #t is returned)).

In conclusion, if "is-odd?" get x>=0 and x is even, that it returns the #f value (Boolean) by the first condition or by getting the value
x-1 that equals to 0 (zero) from the helper function "is-even?", and in this case will also fulfill the first condition.
Else if "is-odd?" get x>=0 and x is odd it will be resolved in a finite number of steps by the "is-even?" helper function,
which will finally get the value x-1 (equal to zero), after an odd number of steps,and return a Boolean value #t
by the stopping condition.

Example:
Case #1: (x=0)
is-odd?(x=0) => #f. (In generally, return Boolean value after odd number of times)

Case #2: (x>0 && x%2==0)
is-odd?(x=2) => is-even?(1) => is-odd?(0) => #f. (In generally, Boolean value after odd number of times)

Case #3: (x>0 && x%2==1)
is-odd?(x=3) => is-even?(2) => is-odd?(1) => is-even?(0) => #t. (In generally, Boolean value after even number of times)

|#
(define (is-odd? x)
  (if (zero? x)
      false
      (is-even? (- x 1))))

(: is-even? : Natural -> Boolean)
;; The purpose of function "is-even?": To check if a variable is even".
;; The function "is-even?" take a number x (Natural).
;; The function "is-even?" returns Boolean value.
;; Note: The detail on this function is later in this file.

#|

The function "is-even?" takes a number x (Natural) and returns:
- if x=0 (=even) -> return #t Boolean value.
- Otherwise, going to function "is-odd?" and send number "x-1" (Integer).

The purpose of this function is to check if the number is zero (=even):
- if true, the function return #t value.
- Otherwise, helped by global helper function called "is-odd?" with sending parameter "x-1", until "is-odd?"
returns the Boolean value #f .

;;My expansion (and conclusions):
Case #1: if x=0: "is-even?" return #t value (Boolean), because number x is really even.
Case #2: if x>0 and x is even: "is-even?" will help by "is-odd?" even number of times (by sending the value x and reducing it by one,
in each transfer between these 2 functions) and finally get a reset value (equal to 0) from the "is-odd?" function and
return the Boolean value #t (since 0 is an even value and the initial number, that "is-even?" take, was even too).
Case #3: if x>0 and x is odd: "is-even?" will be decided by the helper function "is-odd?", by moving the number x
(and reducing it by one, per transfer) between the 2 functions (like as a circle) until the "is-odd?" function receives a reset value
(equal to zero) and returns the Boolean value #f (because zero is indeed an even number but the initial value of the number x
in function "is-even?" was odd - therefore a Boolean value #f is returned)).

In conclusion, if "is-even?" get x>=0 and x is even, that it returns the #t value (Boolean) by the first condition or by getting
the value x-1 that equals to 0 (zero) from the helper function "is-odd?", and in this case will
also fulfill the first condition.
Else if "is-even?" get x>=0 and x is odd it will be resolved in a finite number of steps by the "is-odd?" helper function,
which will finally get the value x-1 (equal to zero), after an odd number of steps,and return a Boolean value #f
by the stopping condition of function "is-odd?".

Example:
Case #1: (x=0)
is-even?(x=0) => #t. (In generally, return Boolean value after odd number of times)

Case #2: (x>0 && x%2==0)
is-even?(x=2) => is-odd?(1) => is-even?(0) => #t. (In generally, Boolean value after odd number of times)

Case #3: (x>0 && x%2==1)
is-even?(x=3) => is-odd?(2) => is-even?(1) => is-odd?(0) => #f. (In generally, Boolean value after even number of times)

Note: In both functions ("is-odd?" and "is-even?") the Boolean value is returned whether it is true or false.
The function "is-odd?" returns false value (#f, Boolean) if it take zero or even value and returns true by the global
helper function "is-even?".
And the opposite is the case, if "is-even?" take zero or even value - it returns true value (#t, Boolean).
Otherwise, after several final iterations, the value will return false (#f, Boolean) by the global helper function "is-odd?".

|#
(define (is-even? x)
  (if (zero? x)
      true
      (is-odd? (- x 1))))

#|
;; tests --- is-odd?/is-even?
(test (not (is-odd? 12)) => #t)
(test (is-even? 12) => #t)
(test (not (is-odd? 0)) => #t)
(test (is-even? 0) => #t)
(test (is-odd? 1) => #t)
(test (not (is-even? 1)) => #t)
|#


(: every? : (All (A) (A -> Boolean) (Listof A) -> Boolean))
;; See explanation about the All syntax at the end of the file…

;; The purpose of function "every?": To check if all (every) variables are comply the condition of operation/function A.
;; The function "every?" takes parameter A (Operation or Function of Any type) and List of type A.
;; The function "every?" returns Boolean value.
;; Note: The detail on this function is later in this file.

#|

The function "every?" takes a parameter A (operation of Any type) and List of type A and returns: 
- If the list is empty (null) or the first variable and also the rest variables in the list
are comply the condition of operation A -> return #t value (Boolean).
- Otherwise, (One of the variables in the list does not comply the condition of operation A) return #f value (Boolean).

The purpose of this function ("every?") is to check whether all the variables in the list are comply the condition of operation A by:
A base (or default) case test (= list is empty) or the first variable test in the list, which is in comply in condition of operation A
and also that the other variables in the list, of the same type, comply with this condition,
by iteratively passing all list variables by tail-recursion and by the helper function (Operation A).

;;My expansion:
If all the variables in the list are comply the condition of operation A or the function "every?" reaches the base (or default) case,
it returns the Boolean value true (#t).
Otherwise, once one of the variables in the list does not comply the condition of operation A,
the function returns a Boolean value false (#f).

The most tools of racket: All and pred.
source of All and pred explanation:
https://docs.racket-lang.org/ts-reference/type-ref.html?q=All#%28form._%28%28lib._typed-racket%2Fbase-env%2Fbase-types-extra..rkt%29._.All%29%29
https://docs.racket-lang.org/ts-reference/Legacy_Forms.html?q=pred#%28form._%28%28lib._typed-racket%2Fbase-env%2Fbase-types-extra..rkt%29._pred%29%29

|#
(define (every? pred lst)
  (or (null? lst)
      (and (pred (first lst))
           (every? pred (rest lst)))))


;; An example for the usefulness of this polymorphic function
(: all-even? : (Listof Natural) -> Boolean)
;; The purpose of function "all-even?": To check if all variables in the list are even".
;; The function "all-even?" takes a list of natural numbers (Listof Natural).
;; The function "all-even?" returns Boolean value.
;; Note: The detail on this function is later in this file.

#|

The function "all-even?" takes a list of natural numbers (Listof Natural) and returns: 
- If all variables in the list are even -> return Boolean value #t (true).
- Otherwise, (One of the variables in the list does not even) return Boolean value #f (false).

The purpose of this function ("all-even?") is to check whether all the variables in the list are even by helper function/operations
"every?" (and "is-even?"), that return Boolean value #t (true) if all variables are even, otherwise, return Boolean value #f (false).
The function "all-even?" is based on the test and return Boolean value of function "every?"
that based on the test and return Boolean value of function "is-even?".
Note: On function "is-even?", i expanded at the beginning of this exercise.

;;General example used functions "every?", "all-even?" ans "is-even?":
Base/ default case: "every?"("is-even?" , '()) => #t .
"all-even?"('(2 4)) => "every?"("is-even?" , '(2 4)) => #f or ("is-even?"(2) and "every?"("is-even?" , '(4))) =>
#f or (#t and (#f or ("is-even?"(4) and "every?"("is-even?" , '())))) => #f or (#t and (#f or (#t and #t))) =>
#f or (#t and (#f or #t)) => #f or (#t and #t) => #f or #t => #t .

|#
(define (all-even? lst)
  (every? is-even? lst))

#|
;; tests
(test (all-even? null) => #t)
(test (all-even? (list 0)) => #t)
(test (all-even? (list 2 4 6 8)) => #t)
(test (not (all-even? (list 1 3 5 7))) => #t)
(test (not (all-even? (list 1))) => #t)
(test (not (all-even? (list 2 4 1 6))) => #t)
|#


(: every2? : (All (A B) (A -> Boolean) (B -> Boolean) (Listof A) (Listof B) -> Boolean))
;; The purpose of function "every2?": To check if all (every) variables of a list1 (Listof A) and a list2 (Listof B)
;; are comply the condition of operation/function A and B, Respectively.
;; The function "every2?" takes parameters A and B (Operations or Functions of Any type), List of type A and List of type B.
;; The function "every2?" returns Boolean value.
;; Note: The detail on this function is later in this file and in the above example,
;; all elements in the list argument must be of the same (polymorphic) type and both lists assumed to be of same length.

#|

Similar to function "every?", the function "every2?" takes a parameters A and B (operations of Any type),List of type A
and List of type B and returns: 
- If the list of A or B is empty (null,by assumption both lists assumed to be of same length) or the first variable of lists A and B
and also the rest variables in the lists A and B are comply the condition of operations A and B, Respectively  ->
return #t value (Boolean).
- Otherwise, (One of the variables in the list A or B does not comply the condition of operations A or B, Respectively)
return #f value (Boolean).

The purpose of this function ("every2?") is to check whether all the variables in the lists A and B are comply the condition of
operations A and B, Respectively, by:
A base (or default) case test (= list A is empty) or the first variable test in the lists A and B, which is in
comply in condition of operations A and B, Respectively, and also that the other variables in the lists A and B, of the same type,
comply with this conditions, by iteratively passing all lists A and B variables by tail-recursion and by the helper functions (Operation A and B).
Note: On functions "is-even?" and "is-odd?", i expanded at the beginning of this exercise.

;;My expansion:
If all the variables in the lists A and B are comply the conditions of operation A and B, Respectively, or the function "every2?"
reaches the base (or default) case, it returns the Boolean value true (#t).
Otherwise, once one of the variables in the lists A or B does not comply the condition of operations A or B, Respectively,
the function returns a Boolean value false (#f).

;;General example used functions "every2?", "is-even?" ans "is-odd?":
Base/ default case: "is-even?"(0) => #t , "is-odd?"(0) => #f and "every2?"("is-even?" , "is-odd?" , '() , '())) => #t.

"every2?"("is-even?" , "is-odd?" , '(2) , '(1)) => #f or ("is-even?"(2) and "is-odd?"(1) and "every2?"("is-even?" , "is-odd?" , '() , '()))
=> #f or (#t and #t and #t) => #f or #t => #t .

;;time for this exercise (EX_1_Q4): It took about a 1.5 hours.

|#
(define (every2? pred1 pred2 lst1 lst2)
  (or (null? lst1) ;; both lists assumed to be of same length
      (and (pred1 (first lst1))
           (pred2 (first lst2))
           (every2? pred1 pred2 (rest lst1) (rest lst2)))))

#|
;; tests
(test (every2? is-even? is-odd? '() '())=> #t)
(test (every2? is-even? is-odd? '(2) '(1))=> #t)
|#
