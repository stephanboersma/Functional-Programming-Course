// Exercise 2.1
let timediff (hh, mm) (HH, MM) = ((HH * 60) + MM) - ((hh * 60) + mm);;
timediff(12, 34) (11, 35);;

// Exercise 2.2

let minutes (hh,mm) = timediff (00, 00) (hh, mm);;
minutes (23,1)

// Exercise 2.3
let rec pow = function
    | (s, 0) -> ""
    | (s, n) -> s + "" + pow(s, n - 1);;
pow ("hej", 3);;
//val it : string = "hejhejhej"

// Exercise 2.4
let rec bin = function
    | (n, 0) -> 1
    | (n, k) when n = k -> 1
    | (n, k) -> bin(n - 1, k - 1) + bin(n - 1, k);;

// Exercise 2.5
let rec f = function 
    | (0,y) -> y
    | (x,y) -> f(x-1, x*y);;



// 2.5.1
// Val it: int

//2.5.2
// The function f terminates for all values x >= 0

//2.5.3
// f (2,3)
// ~> f(2 - 1, 3 * 2)
// ~> f(1, 6)
// ~> f(1 - 1, 1 * 6)
// ~> f(0, 6)
// ~>6


// 2,.5.4
// I have no idea.

// Exercise 2.6

// 2.6.1
// The type of test(c,e) is int.
// The type is inferred because if c is false, then test will return the integer 0
// bool * int -> int

//2.6.2
// The result of evaluating test(false, fact(-1)) is stack overflow due to fact - 1

//2.6.3
// The result of evaluating if false then fact -1 else 0 is 0. The stack overflow will not happen because fact -1 is never executed.


//2.7

let curry f x y = f (x,y);;

let uncurry f (x,y) = f x y;;

