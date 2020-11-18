open System.IO
open System
// Exercise 8.1 H.R. exercise 9.8

type BinTree<'a> = | Leaf
                   | Node of BinTree<'a> * 'a * BinTree<'a>


let rec countA acc = function
    | Leaf -> acc
    | Node(t1, _, t2) ->
        let acc' = countA (acc + 1) t1
        countA acc' t2

let tree = Node(Node(Leaf, 4, Node(Leaf, 5, Leaf)),1, Node(Leaf, 10, Node(Leaf, 11, Leaf)))

countA 0 tree

// Exercise 8.2 H.R exercise 9.9

let rec countAC t a c =
    match t with
    | Leaf -> c 0
    | Node(t1, _, t2) -> countAC t1 (a+1) (fun v1 -> countAC t2 (a+1) (fun v2 -> c(v1+v2+1)))

countAC tree 0 id

// Exercise 8.3 h.R exercise 9.10

let rec bigListK n k =
    if n=0 then k []
    else bigListK (n-1) (fun res -> 1::k(res))

bigListK 130000 id

// The problem is the way the continuation function is written.
// (fun res -> 1::k(res)) is not tail-recursive and each recursion is therefore saved
// in the stack which is why the stack overflows when n is large enough.
// Comparing with bigListC from the slides: (fun res -> c(1::res)) which is tail-recursive.


// Exercise 8.4 H.R exercise 9.11

let rec leftTree n c =
    if n=0 then c Leaf
    else leftTree (n-1) (fun prevNode -> c(Node(prevNode, n, Leaf)))

let rec rightTree n c =
    if n=0 then c Leaf
    else rightTree (n-1) (fun prevNode -> c(Node(Leaf, n, prevNode)))

let rec countC t c = 
    match t with
    | Leaf -> c 0 
    | Node(tl,n,tr) ->
        countC tl (fun vl -> countC tr (fun vr -> c(vl+vr+1)))

let treeWithoutStackOverflow = leftTree 1000 id
let treeWithStackOverflow = rightTree 1300000 id

// Have not found exact stack limit. Don't know how to solve this

#time
countC treeWithStackOverflow id
// Results: Real: 00:00:00.136, CPU: 00:00:00.135, GC gen0: 20, gen1: 0


countAC treeWithStackOverflow 0 id
// Results: Real: 00:00:00.122, CPU: 00:00:00.166, GC gen0: 22, gen1: 0

#time



// Exercise 8.5 HR exercise 11.1
Seq.initInfinite (fun i -> 2 * i - 1)

// Exercise 8.6 HR exercise 11.2    
let rec fact x =
    if x < 1 then 1
    else x * fact (x - 1)

let factSeq n = seq {for i in 1 .. n do yield fact i}
