module adventofcode1

let explode (s:string) =
        [for c in s -> c]

let toDigit (digit:char) = 
    System.Int32.Parse (digit.ToString())

let pairwise (list:List<_>) = 
    let headAndTail = (list.Head, List.last list)
    let pairs = List.pairwise list
    headAndTail :: pairs

let halfWaywise (list:List<_>) = 
    let offset = list.Length / 2
    List.mapi (
        fun i e -> 
            let pairIndex = (offset + i) % list.Length
            (e, list.Item(pairIndex))
            )
        list

let result input =
    input 
    |> explode
    |> List.map toDigit
    //|> pairwise 
    |> halfWaywise
    |> List.map (
        fun (a, b) -> 
            if a = b then
                a
            else
                0
        )
    |> List.sum 
//let count
