// Exercise 7.1 HR exercise 9.1
let xs = [1;2];;
let rec g = function 
    | 0 -> xs
    | n -> 
        let ys = n::g(n-1) 
        List.rev ys;;
g 2
List.rev (1::2::xs)
(*

    { xs    [-> [1;2|x]]
sf0 { g     [-> "closure for g"]
    { it    [?] // compute g 2

//PUSH FRAME
    { n      [2]
sf1 { ys     [?] // compute g(2-1)
    { result [?]

//PUSH FRAME
    { n      [1]
sf2 { ys     [?] // compute g(1-1)
    { result [?]

//PUSH FRAME
    { n      [0]
sf3 { result [-> xs]

//POP sf3

    { n  [1]
sf2 { ys [ -> 1::xs]
    { result [-> List.rev ys] // which is List.rev [1;1;2|x] -> [2; 1; 1|x]

//POP sf2
    { n         [2]
sf1 { ys        [-> 1::[2;1;1|x]]
    { result    [-> List.rev ys] // which is List.rev [2;2;1;1|x] -> [1;1;2;2|x"]

//POP sf1
    { xs    [-> [1;2|x]]
sf0 { g     [-> "closure for g"]
    { it    [-> [1;1;2;2|x]]
    
*)
//Exercise 7.2 HR exercise 9.3


//Exercise 7.3 HR exercise 9.4


//Exercise 7.4 HR exercise 9.6


//Exercise 7.5 HR exercise 8.6


//Exercise 7.6 HR exercise 9.7