// Solved with Mikkel Villebro with help from Anders Fischer
//Exercise 3.1

let rec downTo n =
    if n = 1 then 
        1::[] 
    else 
        n::downTo (n-1);;

let rec downTo2 n = 
    match n with
    | 1 -> 1::[]
    | n -> n::downTo2 (n-1);;


//Exercise 3.2

let rec removeOddIdx (xs: int list) =
    match xs with
    | [] -> []
    | a::b -> (if b.Length % 2 = 0 then
                [a]@removeOddIdx b
                else 
                removeOddIdx b);;

removeOddIdx [];;

//Exercise 3.3
let rec combinePair (xs: int list) =
    match xs with
    | [] -> []
    | xs when xs.Length = 1 -> []
    | x0 :: x1 :: xs -> (x0, x1) :: combinePair xs;;

let c = combinePair [];;

//Exercise 3.4
// With triples
type amount = int * int * int;;
let penceToAmount n = (n / 240, (n%240)/12, (n%240)%12);;
let amountToPence (pound, shilling, pence) = (pound * 240) + (shilling * 12) + pence;;
    
//Infix
let (+++) (this: amount) (that: amount) =
    let a = amountToPence this
    let b = amountToPence that
    penceToAmount (a+b);;

let (---) (this: amount) (that: amount) =
    let a = amountToPence this
    let b = amountToPence that
    penceToAmount (a-b);;


// With Records
type Money = {
    pounds : int
    shilling : int
    pence : int
};;

let penceToAmountRecord n = {pounds = n / 240; shilling = (n%240)/12; pence = (n%240)%12};;
let amountToPenceRecord record = 
    let {pounds = pounds; shilling = shilling; pence = pence} = record
    (pounds * 240) + (shilling * 12) + pence;;

//Infix
let (.+++.) (this: Money) (that: Money) = 
    let a = amountToPenceRecord this
    let b = amountToPenceRecord that
    penceToAmountRecord (a+b);;

let money1 = {pounds = 2; shilling = 3; pence = 4};;
let money2 = {pounds = 1; shilling = 1; pence = 1};;

money1 .+++. money2;;

let (.---.) (this: Money) (that: Money) = 
    let a = amountToPenceRecord this
    let b = amountToPenceRecord that
    penceToAmountRecord (a-b);;

money1 .---. money2;;


//Exercise 3.5

type ComplexNumber = float * float;;

let (+^) (a: ComplexNumber) (b: ComplexNumber) =
    let aa, ab = a
    let ba, bb = b
    ( aa + ba, ab + bb )

let ( *^) (a: ComplexNumber) (b: ComplexNumber) =
    let aa, ab = a
    let ba, bb = b
    ( aa * ba - ab * bb, aa * bb + ab * ba )


let (-^) (a: ComplexNumber) (b: ComplexNumber) =
    let ba, bb = b
    a +^ ( -ba, -bb )

let (/^) (a: ComplexNumber) (b: ComplexNumber) =
    let ba, bb = b
    let d = ba ** 2. + bb ** 2.
    a *^ (ba / d, -bb / d)


//Exercise 3.6
let rec altsum = function
    | [] -> 0
    | x::xs -> x - altsum xs;;



