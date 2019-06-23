module Unit3

let rec sum (l : List<int>) : int =
    match l with
    | [] -> 0
    | x :: xs -> x + (sum xs)




//type ListElement<'a> =
//    | Element of 'a
//    | NestedList of List<ListElement<'a>>

//let rec unzip (l : List<'a * 'b>) : List<'a> * List<'b> =
//    match l with
//         | [] -> ([],[])
//         | (x,y) :: xs ->
//            let l1,l2 = unzip xs
//            x :: l1,y :: l2

//let l2 = [3;5; [4;3; [2;1;3] ;1] ;3;4; [1;2;3] ;6] : ListElement<'a>



let rec last (l: List<'a>) : Option<'a> = 
    match l with
    | [] -> None
    | [x] -> Some x
    | _ :: xs -> last (xs)

let rec rev (l: List<'a>) : List<'a> =
    match l with
    | [] -> []
    | x :: xs -> rev (xs) @ [x]

let rec append (l1 : List<'a>) (l2 : List<'a>) : List<'a> =
    match l1, l2 with
    | [], l2 -> l2
    | x :: xs, l2 -> x :: (append xs l2)

let rec nth (n : int) (l : List<'a>) : Option<'a> =
    if (n < 0) then
        None
    elif (n = 0) then
        Some l.Head
    else
        nth (n - 1) (l.Tail)

let rec palindrome (l : List<'a>) : bool =
    l = rev (l)

let rec compress (l : List<'a>) : List<'a> =
    match l with
    | [] -> []
    | [x] -> [x]
    | x :: y :: xs ->
        if x = y then 
            compress (y :: xs)
        else 
            x :: (compress (y :: xs))

// ex7
//let caesarCypher (l : List<char>) (shift : int) : List<char> =

let rec splitAt (i : int) (l : List<'a>) : List<'a> * List<'a> =
    match l with 
    | [] -> [],[]
    | x :: xs ->
        if (i = 0) then
            [], x :: xs
        else 
            splitAt (i - 1) (xs)

let rec merge (l1 : List<'a>) (l2 : List<'a>) : List<'a> =
    match l1, l2 with
    | [], l -> l
    | l, [] -> l
    | x :: xs, y :: ys ->
        if x <= y then
            x :: (merge (xs) (y :: ys))
        else 
            y :: (merge (x :: xs) (ys))

//let rec mergeSort (l1: List<'a>) (l2 : List<'a>) : List<'a> =


let l1 = [1;3;3;5;5;8] 
let l2 = [2;4;6;7]

let testUnit3() =
    printfn "%A" (l1)
    printfn "%A" (sum (l1))
    printfn "%A" (last (l1))
    printfn "%A" (rev (l1))
    printfn "%A" (merge (l1) (l2))