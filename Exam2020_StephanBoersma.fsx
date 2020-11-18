(* 

    Name: Stephan Boersma
    I hereby declare that I myself have created this exam hand–in in its entirety without help from
    anybody else.

 *)

(* -------------------------| QUESTION 1 |-------------------------------*)
(* OBS: Note to examiner. I have changed MyMap of list<'a*'b> to list<'a*'b>
    I did this because i failed to figure out how to work with the custom defined type. Now my use of Library functions is successfull *)
type mymap<'a,'b> = list<'a*'b>

let ex1 = [('A',65);('B',66);('C',67)]
let ex1' = [('C',67);('A',65);('B',66)]

(* Question 1.1a *)
let dice1Map = [(1, 4);(2, 2);(3, 3);(4, 2);(5, 2);(6, 2)]
let dice2Map = [(1, 4);(2, 2);(3, 3);(4, 3);(5, 5);(6, 3)]


(* Question 1.1b 
ex1 and dice1Map are both of type MyMap but ex1 is MyMap<char, int> where dice1Map is MyMap<int, int>. 
MyMap is essentially a list of tuples 'a * 'b.  
*)

(* Question 1.1c *)
(* Return an empty MyMap list <'a, 'b> *)
let emptyMap() =  []

(* Question 1.1d *)
(* Since mymap<'a,'b> essentially is a list<'a * 'b>, then I am using the library function
   List.length to determine the length of mymap. There seems to be missing some understanding of how the type works
   since I can't use library functions on mymap<'a, 'b> which in my understanding is a list of tuples. *)
let rec size (m: mymap<'a,'b>) = List.length m

size ex1
size (emptyMap())

(* Question 1.2a *)
(* Here I am using pattern matching on the output of the function size(m). If the length is 0 then return true else false. *)
let isEmpty (m: mymap<'a,'b>) = 
    match size(m) with
    | 0 -> true
    | _ -> false

isEmpty ex1
isEmpty (emptyMap())

(* Question 1.2b *)
(* Use library function List.tryFind to find the tuple in mymap where k = a. The contraint when 'a : equality is important to denote that
    the 'a in mymap<'a, 'b> is the same as k. Again, I have issues with the typesystem. *)

let tryFind (k: 'a) (m: mymap<'a,'b> when 'a : equality) = List.tryFind (fun (a, b) -> a = k) m
tryFind 'B' ex1
tryFind 'D' ex1

(* Question 1.2c *)
(* Use library function List.filter to filter the mymap<'a, 'b>. It will return all values where k is not equal to a in the tuple.
    Again, I have issues with the typesystem. *)
let remove (k: 'a) (m: mymap<'a,'b> when 'a : equality) = List.filter (fun (a,b) ->  a <> k) m
remove 'B' ex1

(* Question 1.2d *)
(* If tryFind returns None and could find the key, add the new key value tuple to the map. Else map through list of tuples and replace the value when found *)
let add k v m = 
    if tryFind k m = None then
        m @ [(k, v)]
    else
        List.map (fun (key, value) -> if key = k then (key, v) else (k, value)) m

add 'D' 68 ex1
add 'A' 222 ex1

(* Question 1.3a *)
(* If tryFind returns none and could not find the key in the map, then it simply adds the new key value pair to the Map
 If it does find a match, the function f is applied to v and v' and replaced in the map. *)
let upd f k v m =
    let result = tryFind k m
    match result with
    | Some (_, v') -> add k (f v v') m
    | None -> add k v m

upd (+) 'A' 65 ex1
upd (+) 'D' 68 ex1
(* Question 1.3b *)
(* Using the List.foldBack to implement the map function. From H.R page 102. *)
let map f m = List.foldBack f m

map (fun k v -> v+2) ex1


(* Question 1.3c *)
let rec fold f (s:int) m = List.fold f s m

fold (fun s k v -> s+v) 0 dice1Map
(* -------------------------| QUESTION 2 |-------------------------------*)

(* Question 2.1a *)
(* Pattern match n. use when guard to determine if n is even *)
let even n =
    match n with
    | n when n % 2 = 0 -> true
    | _ -> false

even 1
even 42

(* Question 2.1b *)
let collatz n =
    match n with
    | n when even n -> n /2
    | n -> 3*n + 1

collatz 42

(* Question 2.1c *)
let collatz' n = 
    match n with
    | n when n <= 0 -> failwith "collatz': n is zero or less"
    | n -> collatz n
collatz' 45
collatz' 0

(* Question 2.2a *)
(* applyN is recursively invoked while the value N decreases. When N is < 0 it stops. *)
let rec applyN f n N =
    match N with
    | N when N < 0 -> []
    | N when N >= 0 -> n::applyN f (f n) (N-1)

applyN collatz 42 8

(* Question 2.2b *)
(* Using a recursive inner function with an accumulator to do the same as applyN. When the inner function is done, I return the length of the list. *)
let applyUntilOne f n =
    let rec innerAcc n acc = 
        match n with
        | n when n = 1 -> acc
        | n -> innerAcc (f n) (n::acc)
    let list = innerAcc n []
    list.Length

applyUntilOne collatz 42


(* Question 2.3a *)
(* The function mySeq returns a sequence [a_0, a_1 .. a_n->infinity]. mySeq adds the result of f a_n-1 to the sequence where the base a_0 = x (the inital input)
    mySeq collatz 42 = [42; collatz 42; collatz (collatz 42) ...]  *)
let rec mySeq f x =
  seq { yield x
        yield! mySeq f (f x)}

mySeq collatz 42

(* Question 2.3b *)
(* The function need to receive such a sequence is simply the entry multiplied by two *)
let g x = x * 2

mySeq g 1

(* -------------------------| QUESTION 3 |-------------------------------*)

type name = string
type quantity = float
type date = int * int * int
type price = float
type transType = Buy | Sell
type transData = date * quantity * price * transType
type trans = name * transData

let ts : trans list =
  [("ISS", ((24,02,2014),100.0,218.99,Buy));  ("Lego",((16,03,2015),250.0,206.72,Buy));
   ("ISS", ((23,02,2016),825.0,280.23,Buy));  ("Lego",((08,03,2016),370.0,280.23,Buy));
   ("ISS", ((24,02,2017),906.0,379.46,Buy));  ("Lego",((09,11,2017), 80.0,360.81,Sell));
   ("ISS", ((09,11,2017),146.0,360.81,Sell)); ("Lego",((14,11,2017),140.0,376.55,Sell));
   ("Lego",((20,02,2018),800.0,402.99,Buy));  ("Lego",((02,05,2018),222.0,451.80,Sell));
   ("ISS", ((22,05,2018),400.0,493.60,Buy));  ("ISS", ((19,09,2018),550.0,564.00,Buy));  
   ("Lego",((27,03,2019),325.0,625.00,Sell)); ("ISS", ((25,11,2019),200.0,680.50,Sell)); 
   ("Lego",((18,02,2020),300.0,720.00,Sell))]


(* Question 3.1a *)
(* First I extract the name and transaction data from the input t and I check if the name exist in the map m
    If it does not exist, then I add the name to the map with the transaction data list. (containing only one enty). If the name already
    exist in the map, then I append the transaction data to the existing list. (using the cons operator) *)
let addTransToMap (t:trans) (m:Map<name, transData list>) =
    let (name, td) = t
    let result = m.TryGetValue name
    match result with
    | false, _ -> m.Add(name, [td])
    | true, v -> m.Add(name, td::v)

let m1 = addTransToMap ("ISS", ((24,02,2014),100.0,218.99,Buy)) Map.empty
let m2 = addTransToMap ("ISS", ((22,05,2018),400.0,493.60,Buy)) m1


(* Question 3.1b *)
(* Using the foldBack function to add all transactions to the transaction map. *)
let shares = List.foldBack (fun t m -> addTransToMap t m) ts Map.empty


(* Question 3.2a *)
(* Using pattern matchin on transaction type. If the type is Buy, i execute the calculations specified in the assignment. Same goes for when the type is Sell *)
let accTrans (tq:float,avg:float) ((d,q,p,tType):transData) =
  match tType with
    Buy -> (tq + q, (avg * tq + q * p) / (tq+q))
  | Sell -> (tq - q, avg)

let quantityAndAvgPrice ts = 
    List.fold accTrans (0.0,0.0) ts

quantityAndAvgPrice [((24,02,2014),100.0,218.99,Buy);
                     ((23,02,2016),825.0,280.23,Buy)]


(* Question 3.1c *)
(* Using the Map.map function to calculate the total and averages for each share in "shares" *)
let res = Map.map (fun n td -> quantityAndAvgPrice td) shares


(* -------------------------| QUESTION 4 |-------------------------------*)
let rec dup = function
    [] -> []
  | x::xs -> x::x::dup xs

(* Question 4.1a *)
(* The function dup returns a list where each entry from the input list has been duplicated. e.g. [1;2] becomes [1;1;2;2] *)

(* Question 4.1b *)
let rec dupA acc xs = 
    match xs with
    |[] -> List.rev acc
    |x::xs -> dupA (x::x::acc) xs

dupA [] [1;2;3]

(* Question 4.2a *)
let replicate2 i = seq {for m in 1 .. 2 do yield i}


replicate2 4

(* Question 4.2b *)
(* This solution resolves in seq<seq<int>> which is not optimal. But I could figure out how to flatten it. *)
let dupSeq2 (s:seq<int>) = Seq.initInfinite replicate2

dupSeq2 (seq[1;2])