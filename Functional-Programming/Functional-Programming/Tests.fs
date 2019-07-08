module Tests

// curry -> f () ()
let curry f x y = f (x, y)
let uncurry f (x, y) = f x y

let rec allNumber (n : int) : string =
    if (n <= 0) then
        ""
    else 
        allNumber(n - 1) + " " + string n

let rec allNumberRev (n : int) : string =
    if (n <= 0) then
        ""
    else
        string n + " " + allNumberRev(n - 1)

let rec allNumberRange (lower : int, upper : int) : string =
    if (lower >= upper) then
        string upper
    else 
        string lower + " " + allNumberRange(lower + 1, upper)

let rec allNumberRangeRev (lower : int, upper : int) : string =
    if (lower >= upper) then
        string upper
    else 
        string upper + " " + allNumberRangeRev(lower, upper - 1)

let rec allEvenRange (lower : int, upper : int) : string =
    if (lower >= upper) then
        if lower % 2 = 0 then
            string lower
        else 
            ""
    else 
        if lower % 2 = 0 then
            string lower + " " + allEvenRange(lower + 1, upper)
        else
            allEvenRange(lower + 1, upper)

let rec drawLine (length : int) : string =
    if (length <= 0) then
        ""
    else
        "*" + drawLine(length - 1)


// uncurried
let rec drawSymbol (length : int, symbol : string) : string =
    if (length <= 0) then
        ""
    else
        string symbol + drawSymbol (length - 1, symbol)

// curried
let rec drawSymbolCurried (length : int) (symbol : string) : string =
    if (length <= 0) then
        ""
    else
        string symbol + drawSymbolCurried (length - 1) (symbol)

let rec toBinary (n : int) : string =
    if (n = 0) then
        ""
    else
        let digit = n % 2
        (toBinary(n / 2) + string digit)

let rec toBase (n : int) (_base : int) : string =
    if (n = 0) then
        ""
    else
        let digit = n % _base
        (toBase(n / _base) (_base) + string digit)



let rec sum (l : List<int>) : int =
    match l with
    | [] -> 0
    | x :: xs ->
        x + sum (xs)


let rec last (l : List<'a>) : Option<'a> =
    match l with
    | [] -> None
    | [x] -> Some x
    | _ :: xs ->
        last (xs)

let rec rev (l : List<'a>) : List<'a> =
    match l with
    | [] -> []
    | x :: xs ->
        rev (xs) @ [x]

let rec append (l1 : List<'a>) (l2 : List<'a>) : List<'a> =
    match l1, l2 with
    | [], l2 -> l2
    | l1, [] -> l1
    | x :: xs, y :: ys ->
        if x <= y then
            [x] @ append (xs)(y :: ys)
        else
            [y] @ append (x :: xs) (ys)

let rec nth (n : int) (l : List<'a>) : Option<'a> =
    match l with
    | [] -> None
    | x :: xs ->
        if (n < 0) then
            None
        elif (n = 0) then
            Some x
        else
            nth (n - 1) (xs)

let rec palindrome (l : List<'a>) : bool =
    l = rev(l)

let rec compress (l : List<'a>) : List<'a> =
    match l with
    | [] -> []
    | [x] -> [x]
    | x :: y :: xs ->
        if (x = y) then
            compress (y :: xs)
        else
            [x] @ compress (y :: xs)

let rec splitAt (i : int) (l : List<'a>) : List<'a> * List<'a> =
    match l with
    | [] -> [],[]
    | x :: xs ->
        if (i = 0) then
            [], x :: xs
        else 
            let left, right = splitAt(i - 1) (xs)
            x :: left, right

let rec merge (l1 : List<'a>) (l2 : List<'a>) : List<'a> =
    match l1, l2 with
    | l1, [] -> l1
    | [], l2 -> l2
    | x :: xs, y :: ys ->
        if x <= y then
            [x] @ merge (xs) (y :: ys)
        else
            [y] @ merge (x :: xs) (ys)

let rec mergeSort (l : List<'a>) : List<'a> =
    match l with
    | [] -> []
    | [x] -> [x]
    | _ ->
        let m = l.Length / 2
        let l1, l2 = splitAt (m) (l)
        let sortedL1 = mergeSort (l1)
        let sortedL2 = mergeSort (l2)
        merge sortedL1 sortedL2



let mapFold (f: 'a -> 'b) (l : List<'a>) : List<'b> =
    l |> List.fold(fun mappedList x -> mappedList @ [f x]) []

let filterFold (f : 'a -> bool) (l : List<'a>) : List<'a> =
    l |> List.fold(fun filteredList x ->
                        if f x then
                            x :: filteredList
                        else 
                            filteredList) [] |> List.rev

let flatten (l : List<List<'a>>) : List<'a> =
    l |> List.fold(fun flattenedList l -> flattenedList @ l) []



let testTests() =
    printfn "%s" (allNumber(10))
    printfn "%s" (allNumberRev(10))
    printfn "%s" (allNumberRange(5, 10))
    printfn "%s" (allNumberRangeRev(5, 10))
    printfn "%s" (allEvenRange(5, 10))
    printfn "%s" (drawSymbol(5, "&"))
    printfn "%s" (drawSymbolCurried (5) ("*"))
    printfn "%s" (toBinary(10))
    printfn "%s" (toBase(10)(3))
    printfn "%A" (sum([1;2;3;4;5]))
    printfn "%A" (last(["a";"b";"c"]))
    printfn "%A" (rev([1;2;3;4;5]))
    printfn "%A" (append ([1;3;4;5])([2;2;6;6;7;8]))
    printfn "%A" (nth (7)([1;2;3;4;5;6]))
    printfn "%A" (palindrome(["t";"a";"c";"o";"c";"a";"t"]))
    printfn "%A" (palindrome([1;2;3;4;5]))
    printfn "%A" (compress([1;2;2;2;3;4;5;6;6;7]))
    printfn "%A" (splitAt (2) ([1;2;3;4;5;6]))
    printfn "%A" (merge([1;2;3;7;8])([2;3;4;5;6;9]))
    printfn "%A" (mergeSort([1;2;4;67;9;76;5;4;2;6;88;7;41;31;26;73]))