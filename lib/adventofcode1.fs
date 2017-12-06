module adventofcode1

let explode (s:string) =
        [for c in s -> c]

let toDigit (digit:char) = 
    System.Int32.Parse (digit.ToString())

let pairwise (list:List<_>) = 
    let headAndTail = (list.Head, List.last list)
    let pairs = List.pairwise list
    headAndTail :: pairs

let result input =
    input 
    |> explode
    |> List.map toDigit
    |> pairwise 
    |> List.map (
        fun (a, b) -> 
            if a = b then
                a
            else
                0
        )
    |> List.sum 
//let count
