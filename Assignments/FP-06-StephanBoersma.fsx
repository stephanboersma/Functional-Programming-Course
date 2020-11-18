// Exercise 6.1
type Fexpr = | Const of float
             | X
             | Add of Fexpr * Fexpr
             | Sub of Fexpr * Fexpr
             | Mul of Fexpr * Fexpr
             | Div of Fexpr * Fexpr
             | Sin of Fexpr
             | Cos of Fexpr
             | Log of Fexpr
             | Exp of Fexpr

let test = Mul(Add(X, Const 4.5), Sub(Const 4.0, Const 2.0))
//( X + 4,5 )* (4 - 2)

let rec toString2 = function
    | Const x -> string x + " "
    | X -> "x "
    | Add(fe1, fe2) -> toString2 fe1 + " " + toString2 fe2 + "+ "
    | Sub(fe1, fe2) -> toString2 fe1 + " " + toString2 fe2 + "- "
    | Mul(fe1, fe2) -> toString2 fe1 + " " + toString2 fe2 + "* "
    | Div(fe1, fe2) -> toString2 fe1 + " " + toString2 fe2 + "/ "
    | Sin fe -> toString2 fe + "sin "
    | Cos fe -> toString2 fe + "cos "
    | Log fe -> toString2 fe + "log "
    | Exp fe -> toString2 fe + "exp "

//Exercise 6.2
type Instruction = | ADD | SUB | MULT | DIV | SIN |COS|LOG|EXP |PUSH of float

type Stack = float list

//6.2.1
let intpInstr (stack: Stack) (instruction:Instruction) =
    match instruction, stack with
    | PUSH(n), xs -> n::xs
    | ADD, x0::x1::xs -> (((+) x1 x0))::xs
    | SUB, x0::x1::xs -> ((-) x1 x0)::xs
    | MULT, x0::x1::xs -> ((*) x1 x0)::xs
    | DIV, x0::x1::xs -> ((/) x1 x0)::xs
    | SIN, x0::xs -> sin(x0)::xs
    | COS, x0::xs -> cos(x0)::xs
    | LOG, x0::xs -> log(x0)::xs
    | EXP, x0::xs -> exp(x0)::xs
    | _ -> failwith "Illegal Instruction"

//6.2.2
let intpProg (instructions: Instruction list) =
    let rec execute (stack: Stack) instructions =
        match instructions with
        | [] -> stack.Head
        | instr::rest -> 
            let stack' = intpInstr stack instr
            execute stack' rest
    execute [] instructions

//6.2.3
let rec trans (fe, x:float) =
    match fe with
    | Const(value) -> [PUSH(value)]
    | Add(fe1, fe2) -> (trans (fe1, x)) @ (trans (fe2, x)) @ [ADD]
    | Sub(fe1, fe2) -> (trans (fe1, x)) @ (trans (fe2, x)) @ [SUB]
    | Mul(fe1, fe2) -> (trans (fe1, x)) @  (trans (fe2, x)) @ [MULT]
    | Div(fe1, fe2) -> (trans (fe1, x)) @  (trans (fe2, x)) @ [DIV]
    | Sin fe -> (trans (fe, x)) @ [SIN]
    | Cos fe -> (trans (fe, x)) @ [COS]
    | Log fe -> (trans (fe, x)) @ [LOG]
    | Exp fe -> (trans (fe, x)) @ [EXP]
    | X -> [PUSH(x)]

let instr = trans (test, 5.0)

intpProg instr