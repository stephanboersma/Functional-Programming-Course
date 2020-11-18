//Exercise 4.1
let explode (s:string) = [for c in s -> c]

let rec explode2 = function
    | "" -> []
    | s -> s.[0]:: explode2(s.Remove(0,1))

//Exercise 4.2
let implode (list: char list) = List.foldBack (fun accumulator element -> (string)accumulator + element) list ""

let implode2 (list: char list) = List.fold (fun element accumulator -> (string)accumulator + element) "" list

//Exercise 4.3
let toUpper s = 
    let upperCaseList = List.map (System.Char.ToUpper) (explode s)
    implode upperCaseList

let toUpper1 = explode >> (List.map (System.Char.ToUpper) >> implode) 

let toUpper2  = implode << (List.map System.Char.ToUpper << explode)
// Shouldn't this work?? let toUpper2 = (List.map System.Char.ToUpper << explode) |> implode


// Exercise 4.4
// Version one without pipe forward operator
let palindrome s = (implode2 (explode (toUpper s))) = toUpper s

// Version two with pipe forward operator
let palindrome2 s = (toUpper s |> explode |> implode2) = toUpper s


// Exercise 4.5
let rec ack = function
    | (m, n) when m = 0 -> n + 1
    | (m, n) when m > 0 && n = 0 -> ack(m - 1, 1)
    | (m, n) when m > 0 && n > 0 -> ack(m - 1, ack(m, n - 1))

ack (3, 11)
//Result is 16381

// Exercise 4.6
let time f =
    let start = System.DateTime.Now in
    let res = f () in
    let finish = System.DateTime.Now in
    (res, finish - start);

time(fun () -> ack (3,11))

let timeArg1 f a = time (fun () -> f a)
timeArg1 ack (3,11)

// Exercise 4.7
let rec downTo1 f n e = 
    match (n, e) with
    | (0, e) -> e
    | (n, e) -> f(n, (downTo1 f (n-1) e))

let fact n = downTo1 (fun (n, e) -> n*e) n 1

let applyFunctionToList g n = downTo1 (fun (n, e) -> e @ [g(n)]) n []

applyFunctionToList (fun n -> n+2) 10