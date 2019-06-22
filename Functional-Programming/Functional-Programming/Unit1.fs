module Unit1

let rec allNumber(n: int) : string =
    if(n <= 0) then 
        ""
    else 
        allNumber(n - 1) + " " + (string n)

let rec allNumberRev =
    fun (n) ->
        if(n <= 0) then 
            ""
        else 
            (string n) + " " + allNumberRev(n - 1)

let rec allNumberRange =
    fun (lower, upper) ->
        if(lower > upper) then 
            "lower value is greater than upper value"
        elif(lower = upper) then 
            (string upper)
        else 
            (string lower) + " " + allNumberRange(lower + 1, upper)

let rec allNumberRangeRev =
    fun (lower, upper) ->
        if(lower > upper) then 
            "lower is greater than upper"
        elif(lower = upper) then 
            (string upper)
        else 
            allNumberRangeRev(lower + 1, upper) + " " + (string lower)

let rec allEvenRange =
    fun (lower, upper) ->
        if(lower > upper) then "lower is greater than upper"
        else 
            if(lower % 2 = 0) then 
                if(lower = upper) then 
                    (string upper)
                else 
                    (string lower) + " " + allEvenRange(lower + 1, upper)
            else 
                allEvenRange(lower + 1, upper)

let rec drawLine =
    fun (length) ->
        if(length <= 0) then 
            ""
        else 
            "*" + drawLine(length - 1)

let rec drawSymbolUncurried =
    fun (length, symbol) ->
        if(length <= 0) then 
            ""
        else 
            symbol + drawSymbolUncurried(length - 1, symbol)

let rec drawSymbolCurried = 
    fun symbol length ->
        if(length <= 0) then 
            ""
        else 
            symbol + drawSymbolCurried(symbol)(length-1)

let rec toBinary =
    fun n ->
        if(n = 0) then
            ""
        else 
            let digit = n % 2
            (toBinary (n / 2)) + (string digit)

            // 10 in binary is 1010

let rec toBase (n : int) (_base : int) : string =
    if n = 0 then
        ""
    else 
        let digit = n % _base
        (toBase(n / _base) _base) + (string digit) 












let testUnit1() = 
    printfn "%s" ("allNumber(10): " + allNumber(10))
    printfn "%s" ("allNumberRev(10): " + allNumberRev(10))
    printfn "%s" ("allNumberRange(2, 8): " + allNumberRange(2, 8))
    printfn "%s" ("allNumberRangeRev(2, 8): " + allNumberRangeRev(2, 8))
    printfn "%s" ("allEvenRange(2, 8): " + allEvenRange(2, 8))
    printfn "%s" ("drawLine(6): " + drawLine(6))
    printfn "%s" ("drawSymbolUncurried(6, '*'): " + drawSymbolUncurried(6, "*"))
    printfn "%s" ("drawSymbolCurried('#')(6): " + drawSymbolCurried("#")(6))
    printfn "%s" ("toBinary(10): " + toBinary(10))
    printfn "%s" ("toBase(10)(3): " + toBase(10)(3))