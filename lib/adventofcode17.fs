module adventofcode17

let insert index element list = 
    let (head, tail) = List.splitAt index list
    List.concat [head ; [element] ; tail]

let getInsertionIndex currentIndex step length =
    ((currentIndex + step) % length) + 1

let solve step limit =
    let rec solve' i currentIndex list = 
        printfn "%i" i
        if i = limit then
            list |> List.item (currentIndex+1)
        else
            let insertionIndex = getInsertionIndex currentIndex step (List.length list)
            let newList = insert insertionIndex (i+1) list
            solve' (i+1) insertionIndex newList
    solve' 0 0 [0]

    
let solve2 step limit =
    let rec solve' i currentIndex answer = 
        if i = limit then
            answer
        else
            let insertionIndex = getInsertionIndex currentIndex step (i+1)
            let newAnswer = if insertionIndex = 1 then (i+1) else answer
            solve' (i+1) insertionIndex newAnswer
    solve' 0 0 0
    