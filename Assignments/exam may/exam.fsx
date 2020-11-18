
//Question 1.1a
let infSeq3 = Seq.initInfinite(fun i -> i * 3)

//Question 1.1b
let finSeq3 n = seq { 1 .. n }

//Question 1.1c
let sumSeq3 n = Seq.sum (finSeq3 n) 

sumSeq3 100

//Qusetion 1.2a
let seqMap2 f s1 s2 =
    seq { for (x,y) in Seq.zip s1 s2 do
            yield f x y }
// seqMap2 returns a sequence with the output of function f applied to
// all pairs (x,y) in s1 and s2 when zipped.

//Question 1.2b
// let swap (x,y) = (y,x)
// seqMap2 swap [1;3;3] [4;5;2] 
// seqMap2 does not work because seqMap2 yields a sequence
// where the function f is applied to x y and not applied to a pair like f(x,y).

//Question 1.2c
// To fix the problem with rewrite the swap function to take swap x y instead
// of swap (x,y)
let swap x y = (y,x)
seqMap2 swap [1;3;3] [4;5;2]

//----------------------------------------------------------------------------

type TrieNode <'a when 'a : equality> = TN of 'a * bool * TrieNode<'a> list
let trie01 = TN('a',false,[TN('n',false,[TN('d',true,[])])])

//Question 2.1a

let trie03 = TN('a', false, [TN('n', true, [TN('d', true, [])]); TN('d', false, [TN('d', true, [])]);TN('t', true, [])])
let trie04 = TN('a', false, [TN('n', false, [TN('d', true,[])]); TN('d', false, [TN('d', true, [])]);TN('t', true, [TN('x', false, [])])])

//Question 2.1b
// The type of trie04 is TrieNode<char> and is monomorphic because it has been declared with 
// chars and it is not possible to change it up.
// The type TrieNode is polymorphic since TrieNode<'a'> can be lots of different types
// e.g. TrieNode<int> or TrieNode<char> as the case is above.

//Question 2.1c
exception TrieError of string



//Question 2.2a
// Create a function numLetters that takes an argument trie of tipe TrieNode<'a>
// An inner function called sum takes an accumulator and the trie as arguments where it extracts the third value in the triple.
// The list. It recursively processes all the TrieNodes and increments the accumulator with 1.
(* let numLetters (trie:TrieNode<'a>) =
   let rec countAC (t:TrieNode<'a>) a c =
        match t with
        | TN (_, _, []) -> c 0
        | TN (_, _, x0::xs) -> countAC x0 (a+1) (fun v1 -> countAC xs (a+1) (fun v2 -> c(v1+v2+1)))
    countAC trie 0 id

numLetters trie03 *)

let rec numLetters = function
    | TN(_,_,[]) -> 1
    | TN(a,b,lst) -> 1 + List.fold (fun acc tn -> acc + numLetters tn) 0 lst
numLetters trie03
//Question 2.2b
(* let numWords (trie:TrieNode<'a>) =
    let rec countWords acc (t:TrieNode<'a>) = 
        match t with
        | (l, b, []) when b -> acc + 1
        | (l, b, x0::xs) when b -> countWords (acc+1) (l, b, xs)
        | (l, b, x0::xs) when b  -> countWords acc (l, b, xs)
    countWords 0 trie
numWords trie03 *)


//Question 3.1a
let rec F m i n k = 
    match k with
    | k when k <= 0 -> m
    | k when k>0 -> (F m i n (k-1)) * (1.0 + (i/n))
F 100.0 0.1 1.0 10

//It is not tail recursive because (F m i n (k-1)) * (.... still needs to return to the multiplication computation.

(* let rec FA m i n k =
   let rec inner n acc =
    if n = k then
        acc
    else
        inner (n+1) (acc)*)

let tabulate f start step stop =
    let rec innerAcc f pos acc = 
        if pos > stop then
            acc
        else
            innerAcc f (pos + step) (acc @ [(pos, (f pos))])
    innerAcc f start []

let listOfTuples = tabulate (F 100.0 0.1 1.0) 0 2 4

let prettyPrint xs = 
    printf "  x   | f(x)   \n"
    printf "------+-------\n"
    xs |> Seq.iter (fun (x, y) -> printf "   %A  |  %A \n" x y)

let dt(d,m,y) = System.DateTime(y, m, d) 
exception Error of string
type Position =
    | Stock of string
    | Cash of float
type Action =
    | Aquire of System.DateTime * Position 
    | Give of System.DateTime * Position
    | Scale of int * Action
    | All of Action list

let sellApple = Scale(100, All[Give(dt(1,3,2018), Stock "APPLE");Aquire(dt(1,3,2018), Cash 400.4)])

let price (s: string ,d: System.DateTime) =
    match (s, d: System.DateTime) with
    | ("APPLE", dt(1,2,2018)) -> 300.3
    | ("APPLE", dt(1,3,2018)) -> 400.4
    | ("ISS",  dt(1,2,2018))-> 150.0
    | ("ISS", dt(1,3,2018)) -> 200.2
    | ("TIVOLI", dt(1,2,2018)) -> 212.0
    | ("TIVOLI", dt(1,3,2018)) -> 215.2
    | _ -> raise (Error "Stock and Date unknown")

price("APPLE", dt(1,2,2018))

let buyStock n (s:string) d = 
    match n with
    | 1 -> All[(Aquire (d, Stock s)); Give(d, Cash (price (s,d)))]
    | n when n > 1 -> Scale(n, All[Aquire(d, Stock s); Give(d, Cash (price (s,d)))])
    | _ -> failwith "You cannot buy 0 stocks"


let receiveCash c d = Aquire(d, Cash c)

let actions =
    let d1 = dt(1,2,2018)
    let d2 = dt(1,3,2018)
    All [receiveCash 100000.0 d1;
         buyStock 100 "APPLE" d1;
         buyStock 200 "ISS" d1;
         buyStock 50 "TIVOLI" d2]