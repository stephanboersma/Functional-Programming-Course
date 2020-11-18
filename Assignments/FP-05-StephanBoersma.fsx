
// Exercise 5.1
type 'a BinTree =
    Leaf
  | Node of 'a * 'a BinTree * 'a BinTree

let intBinTree = 
  Node(43, Node(25, Node(56,Leaf, Leaf), Leaf),
                    Node(562, Leaf, Node(78, Leaf, Leaf)))

let rec inOrder = function
    | Leaf -> []
    | Node(n, t1, t2) ->
        inOrder t1 @ n::inOrder t2

inOrder intBinTree;;


//Exercise 5.2
let rec mapInOrder f = function
    | Leaf -> Leaf
    | Node(n, t1, t2) -> 
      let t1' = mapInOrder f t1
      let n' = f n
      let t2' = mapInOrder f t2
      Node(n', t1', t2')

mapInOrder (fun n -> n+1) intBinTree

// If a given function has side effects then the order of traversal make sense. 
// e.g. printing the node value. Post order would print in the reverse order of the in order function


//Exercise 5.3
let rec foldInOrder f a = function
  | Leaf -> a
  | Node(n, t1, t2) ->
    let a' = foldInOrder f a t1
    foldInOrder f (f n a') t2


// Exercise 5.4 - 5.6
type aExp = 
  | N of int
  | V of string
  | Inc of string
  | Add of aExp * aExp
  | Mul of aExp * aExp
  | Sub of aExp * aExp

type bExp = (* Boolean expressions *)
  | TT (* true *)
  | FF (* false *)
  | Eq of aExp * aExp (* equality *)
  | Lt of aExp * aExp (* less than *)
  | Neg of bExp (* negation *)
  | Con of bExp * bExp (* conjunction *)

type stm = (* statements *)
  | Ass of string * aExp (* assignment *)
  | Skip
  | Seq of stm * stm
  | ITE of bExp * stm * stm (* if-then-else *)
  | While of bExp * stm (* while *)
  | IT of bExp * stm
  | RepeatUn of bExp * stm

exception NotFound of string

let add x a s = Map.add x a s

let lookUp a s = 
  match Map.tryFind a s with
    | None -> raise (NotFound("Could not find element"))
    | Some b -> (b, s)


let rec arithEvalState a s =
  match a with
  | N n -> (n, s)
  | V v -> lookUp v s
  | Inc x -> 
    let (n, s') = lookUp x s
    (n + 1, add x (n+1) s')
  | Add(a,b) -> 
    let (a', s') = (arithEvalState a s) 
    let (b', s'') = (arithEvalState b s')
    (a' + b', s'')
  | Mul(a,b) -> 
    let (a', s') = (arithEvalState a s) 
    let (b', s'') = (arithEvalState b s')
    (a' * b', s'')
  | Sub(a,b) -> 
    let (a', s') = (arithEvalState a s) 
    let (b', s'') = (arithEvalState b s')
    (a' - b', s'')


let rec evalBool b s =
  match b with
  | TT -> (true, s)
  | FF -> (false, s)
  | Eq(a, b) -> 
    let (a', s') = arithEvalState a s
    let (b', s'') = arithEvalState a s'
    (a' = b', s'')
  | Lt(a, b) -> 
    let (a', s') = arithEvalState a s
    let (b', s'') = arithEvalState a s'
    (a' < b', s'')
  | Neg a -> 
    let (a', s') = evalBool a s
    (not a', s')
  | Con(a,b) -> 
    let (a', s') = evalBool a s
    let (b', s'') = evalBool a s'
    (a' && b', s'')

let rec I stm s =
  match stm with
  | Ass(x,a) -> 
    let (a', s') = (arithEvalState a s)
    add x a' s'
  | Skip -> s
  | Seq(stm1, stm2) -> I stm1 s |> I stm2
  | ITE(b,stm1,stm2) ->
    let (b', s') = evalBool b s
    if b' then I stm1 s' else I stm2 s'
  | While(b, stm) ->
    let (b', s') = evalBool b s
    if b' then I (While(b, stm)) s' else I (Skip) s'
  | IT(b, stm) ->
    let (b', s') = evalBool b s
    if b' then I stm s' else I (Skip) s'
  | RepeatUn(b, stm) ->
    let (b', s') = evalBool b s
    if b' then I (Skip) s' else I (RepeatUn(b, stm)) s'
  



(*Alternative solution:

A lot cleaner where the Inc has been moved to I. The only function that can change the state is I.

type aExp = 
  | N of int
  | V of string
  | Add of aExp * aExp
  | Mul of aExp * aExp
  | Sub of aExp * aExp

type bExp = (* Boolean expressions *)
  | TT (* true *)
  | FF (* false *)
  | Eq of aExp * aExp (* equality *)
  | Lt of aExp * aExp (* less than *)
  | Neg of bExp (* negation *)
  | Con of bExp * bExp (* conjunction *)

type stm = (* statements *)
  | Ass of string * aExp (* assignment *)
  | Skip
  | Seq of stm * stm
  | ITE of bExp * stm * stm (* if-then-else *)
  | While of bExp * stm (* while *)
  | IT of bExp * stm
  | RepeatUn of bExp * stm
  | Inc of string

exception NotFound of string

let add x a s = Map.add x a s

let lookUp a s = 
  match Map.tryFind a s with
    | None -> raise (NotFound("Could not find element"))
    | Some b -> b


let rec arithEval a s =
  match a with
  | N n -> n
  | V v -> lookUp v s
  | Add(a,b) -> (+) (arithEval a s) (arithEval b s) 
  | Mul(a,b) -> (INSERT ASTERIX HERE) (arithEval a s) (arithEval b s) 
  | Sub(a,b) -> (-) (arithEval a s) (arithEval b s)


let rec evalBool b s =
  match b with
  | TT -> true
  | FF -> false
  | Eq(a, b) -> (=) (arithEval a s) (arithEval b s)
  | Lt(a, b) -> (<) (arithEval a s) (arithEval b s)
  | Neg a -> not (evalBool a s)
  | Con(a,b) -> (&&) (evalBool a s) (evalBool b s)

let rec I stm s =
  match stm with
  | Ass(x,a) -> add x (arithEval a s) s
  | Skip -> s
  | Seq(stm1, stm2) -> I stm1 s |> I stm2
  | ITE(b,stm1,stm2) ->
    if evalBool b s then I stm1 s else I stm2 s
  | While(b, stm) ->
    if evalBool b s then I (While(b, stm)) s else I (Skip) s
  | IT(b, stm) ->
    if evalBool b s then I stm s else I (Skip) s
  | RepeatUn(b, stm) ->
    if evalBool b s then I (Skip) s else I (RepeatUn(b, stm)) s 
  | Inc x ->
    let n = arithEval (V x) s
    I (Ass(x, N (n+1))) s

*)








