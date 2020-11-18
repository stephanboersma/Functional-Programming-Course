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

let lookUp (a:string) s = 
  match Map.tryFind a s with
    | None -> raise (NotFound("Could not find element"))
    | Some b -> b


let rec arithEval a s =
  match a with
  | N n -> n
  | V v -> lookUp v s
  | Add(a,b) -> (+) (arithEval a s) (arithEval b s) 
  | Mul(a,b) -> (*) (arithEval a s) (arithEval b s) 
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