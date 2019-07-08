module Unit4

let concat x = x + 1


let mapFold (f: 'a -> 'b) (l : List<'a>) : List<'b> =
    l |> List.fold (fun mappedList x -> f x :: mappedList) [] |> List.rev



let filterFold (f: 'a -> bool) (l: List<'a>) : List<'a> =
    l |> List.fold
            (fun filteredList x ->
                if f x then
                    x :: filteredList
                else 
                    filteredList) [] |> List.rev



let flatten (l : List<List<'a>>) : List<'a> =
    l |> List.fold (fun flattenedList l -> flattenedList @ l) []



let rec map2 (f: 'a -> 'b -> 'c) (l1 : List<'a>) (l2 : List<'b>) : Option<List<'c>> =
    match l1, l2 with
    | [], [] -> Some []
    | [], _ -> None
    | _, [] -> None
    | x :: xs, y :: ys ->
        let lopt = map2 f xs ys
        match lopt with
        | None -> None
        | Some l -> Some((f x y) :: l)



let rec fold2 (f: 'state -> 'a -> 'b -> 'state) (init : 'state) (l1 : List<'a>) (l2 : List<'b>) : Option<'state> =
    match l1, l2 with
    | [], [] -> Some []
    | [], _ -> None
    | _, [] -> None
    | x :: xs, y :: ys ->
        fold2 f (f init x y) xs ys



let rec zip (l1 : List<'a>) (l2 : List<'b>) : Option<List<'a * 'a>> =
    match l1, l2 with
    | [], [] -> Some []
    | _, [] -> None
    | [], _ -> None
    | x :: xs, y :: ys ->
        let lopt = zip xs ys
        match lopt with
        | None -> None
        | Some l -> Some((x, y) :: l)



let rec map2Safe (f : 'a -> 'b -> 'c) (l1 : List<'a>) (l2 : List<'b>) : List<Option<'c>> =
    match l1, l2 with
    | [], [] -> []
    | l1, [] -> l1 |> List.map (fun _ -> None)
    | [], l2 -> l2 |> List.map (fun _ -> None)
    | x :: xs,y :: ys ->
      (Some (f x y)) :: (map2Safe f xs ys)

let testUnit4() =
    printfn "%A" (mapFold concat [1;2;3])
    //printfn "%A" (map2 concat [1;2;3] [2;3;4])