// Exercise 1.1
let sqr x = x * x;;
sqr 4;;

//Exercise 1.2
let pow (x,n) = System.Math.Pow(x,n);;
pow(2.0, 2.0);;

//Exercise 1.3 - Changed multiplication to addition.
let g n = n + 4;;
let test = g 4;;

//Exercise 1.4
let h (x,y) = System.Math.Sqrt(pow(x, 2.0) + pow(y, 2.0));;
let test = h(3.0, 3.0);;

//Exercise 1.5
let rec f = function
    | 0 -> 0 // i
    | n -> f(n - 1) + n;; // ii
let test = f 4;;

//Exercise 1.6 - added evaluation
let rec f = function
    | 0 -> 0
    | 1 -> 1
    | n -> f(n - 1) + f(n - 2);;

// Evaluation of F4
// ~> f(4 - 1) + f(4 - 2)                               (1)
// ~> (f(3 - 1) + f(3 - 2)) + (f(2 - 1) + f(2 - 2))     (2)
// ~> (f(2) + f(1)) + (f(1) + f(0))                     (3)
// ~> (f(2 - 1) + 1) + (1 + 0)                          (4)
// ~> (f(1) + 1) + (1 + 0)                              (5)
// ~> (1 + 1) + (1 + 0)                                 (6)
// ~> 2 + 1                                             (7)
// ~> 3                                                 (8)



//Exercise 1.7 added + n
let rec sum = function
    | (m, 0) -> m
    | (m, n) -> sum(m, n - 1) + m + n;;

let test = sum (1, 1);;

//Exercise 1.8
//Define the types for the following expressions
//(System.Math.PI, fact -1)
// Stack overflow

//fact(fact 4) 
// valt it: int

//power(System.Math.PI, fact 2)
// float

//(power, fact)
// (float * int -> float * int) * (int -> int)

// Exercvise 1.9
let a = 5;;
let f a = a + 1;;
let g b = (f b) + a;;

// Evaluation of f 3
// f 3
// ~> 3 + 1
// ~> 4

// Evalutation of g 3
// g 3
// ~> (f 3) + 5
// ~> (3 + 1) + 5
// ~> 4 + 5
// ~> 9

//Exercise 1.10
let dup s = s + "" + s;;
dup "hi ";;

//Exercise 1.11
let rec dupn s n =
    if n <= 0 then
        ""
    else
        s + (dupn s (n-1));;
dupn "hi " 3;;
