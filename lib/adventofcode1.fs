module adventofcode1

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

let sovlve input =
    input 
    |> Seq.toList
    |> List.map toDigit
    //|> pairwise 
    |> halfWaywise
    |> List.filter (fun (a, b) -> a = b)
    |> List.sumBy (fun (a,_) -> a) 
//let count
