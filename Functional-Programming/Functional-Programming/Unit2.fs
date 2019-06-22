module Unit2


   
//let curryAdd = fun a b -> a + b
let (curryAdd : int -> int -> int) = fun x -> fun y -> x + y

// let addUncurried = fun (x,y) -> x + y
// with the types
let (addUncurried : int * int -> int) = fun (x,y) -> x + y

let testUnit2() =
    //printfn "%d" add(5)(4)
    printfn "%d" (curryAdd 4 5)
    printfn "%d" (addUncurried(5, 10))