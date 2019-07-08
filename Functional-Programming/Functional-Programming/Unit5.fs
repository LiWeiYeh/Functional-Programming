module Unit5

type Tree<'a when 'a : equality> =
    | Empty
    | Node of 'a * List<Tree<'a>>

    with

    member this.Map(f: 'a -> 'b) =
        match this with
        | Empty -> Empty
        | Node(x, subtrees) ->
            Node(f x, subtrees |> List.map(fun t -> t.Map f))

    member this.Fold(f : 'state -> 'a -> 'state) (state : 'state) : 'state =
        match this with
        | Empty -> state
        | Node(x, subtrees) ->
            let state1 = f state x
            subtrees |>
            List.fold(fun s tree -> tree.Fold f s) state1

    member this.Find(element : 'a) : Option<'a> =
        match this with
        | Empty -> None
        | Node (x, children) ->
            if (x = element) then
                Some x
            else 
                children |> List.fold (fun s current_child ->
                                        match s with
                                        | Some _ -> s
                                        | None -> current_child.Find(element)) None
                

let myTree : Tree<int> = Node(1, [Node(2, [Node(3, [Empty])])])

let testUnit5() = 
    printfn "%A" (myTree)
    printfn "%A" (myTree.Map(fun x -> x + 1))