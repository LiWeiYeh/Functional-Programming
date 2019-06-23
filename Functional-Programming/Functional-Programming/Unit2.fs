module Unit2

open System


// Theory 
////let curryAdd = fun a b -> a + b
//let (curryAdd : int -> int -> int) = fun x -> fun y -> x + y

//// let addUncurried = fun (x,y) -> x + y
//// with the types
//let (addUncurried : int * int -> int) = fun (x,y) -> x + y

//let testUnit2() =
//    //printfn "%d" add(5)(4)
//    printfn "%d" (curryAdd 4 5)
//    printfn "%d" (addUncurried(5, 10))

// Exercise 1

let r = Random()

type Point2D =
    {
        Position    : float * float
    }
    with
        static member Create(xPos: float, yPos: float) =
            {
                Position = (xPos, yPos)
            }
        member this.xPos = fst this.Position
        member this.yPos = snd this.Position
        // sqrt (a^2 + b^2) = c
        //static member Distance(firstPoint2D: Point2D, secondPoint2D: Point2D) = 


let x5floaty6float = Point2D.Create(5.0, 6.0)

let testUnit2() = 
    printfn "%A" x5floaty6float.xPos