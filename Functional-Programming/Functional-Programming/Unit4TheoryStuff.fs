module Unit4TheoryStuff

// basics of Higher Order Functions
let incr x = x + 1
let double x = x * 2
let halve x = x / 2

let compose f g = fun x -> g(f(x))

// the same thing
let f1 = compose incr double
let f2 = incr >> double



// for loop function
let rec repeat n f = if n <= 0 then id else f >> repeat (n-1) f

let star s = s + "*"
let space s = s + " "
let newline s = s + "\n"
let row n = repeat n star
let square n = repeat n (row n >> newline)



// map
let mapTupleLeft f (x,y) = (f x,y)

// does the operation either once or not at all Some or None
let mapOption f o = match o with None -> None | Some x -> Some(f x)

let rec mapList f l = 
    match l with 
    | [] -> [] 
    | x :: xs -> (f x) :: mapList f xs



// multiple maps
let mapListOption f = mapList (mapOption f)



// filter
let rec filterOption predicate l =
    match l with
    | Some x when predicate
        x -> Some x
    | _ -> None

let rec filterList predicate l =
    match l with
    | [] -> []
    | x :: xs when predicate
        x -> x :: filterList predicate xs
    | x :: xs ->
        filterList predicate xs



// fold
let rec foldList z f l = 
    match l with 
    | [] -> z // z being the folded factor
    | x :: xs -> f x (foldList z f xs)

let rec foldList2 z f l =
    match l with
    | [] -> z
    | x :: xs -> (foldList2 (f z x)) f xs

//let minOption x y =
//    match y with
//     | Some a when a < y 
//        -> y
//     | _ -> Some x

// uses fold
let mapList2 f = foldList [] (fun x xs -> f x::xs)

let filterList2 p l =
 foldList [] (fun x xs -> if p x then x::xs else xs)



let curry f x y = f (x,y)
let uncurry f (x,y) = f x y




let testUnit4TheoryStuff() =
    printfn "%A" (f1 4)
    printfn "%A" (f2 4)
    printfn "%A" (repeat (-1) (incr) (2))
    printfn "%s" (repeat 5 star "")
    printfn "%A" (square (5) "")
    printfn "%A" (row)
    printfn "%A" (mapTupleLeft incr (1,"a"))
    printfn "%A" (mapTupleLeft (incr >> double) (2,["a";"b"]))
    printfn "%A" (mapOption (repeat (2) (incr)) (Some 3))
    printfn "%A" (mapList double [1;3;5])
    // condition: x % 2 = 0
    printfn "%A" (filterList (fun x -> x % 2 = 0) [1;2;3;4;5;6])
    // 4 being the fold factor
    printfn "%A" (foldList 4 (*) [1;2;3;4])
    //printfn "%A" (foldList None minOption [1;2;3])
    printfn "%A" (mapList2 double [1;3;5])
    printfn "%A" (filterList2 (fun x -> x % 2 = 0) [1;2;3;4;5;6])
