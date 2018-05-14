module adventofcode10

let rotate step input =
    let  ls = input |> List.ofSeq
    List.fold (fun (s, c) e -> if s <> 0 then (s-1 , List.append c.Tail [e]) else (0, c)) (step, ls) ls
    |> fun (_,y) -> y |> List.ofSeq

let rotatePart start length (list:'a list) = 
    if (start + length) <= list.Length then
        let reversedPart = 
            list 
            |> List.skip (start)
            |> List.take length
            |> List.rev
        let headPart = List.take start list
        let tailPart = List.skip (start+length) list
        List.concat [headPart; reversedPart; tailPart]
    else 
        let firstSlice = List.take (start+length-list.Length) list
        let lastSlice = List.skip start list
        let reversed = 
            List.concat [lastSlice;firstSlice]
            |> List.rev
        List.concat [reversed; list |> List.skip (start+length-list.Length) |> List.take (list.Length - length)] |> rotate lastSlice.Length
        
type knotState =
    {
        list : int list
        currentPosition : int
        skipSize : int
    }

let foldKnot state length = 
    let newList = rotatePart state.currentPosition length state.list
    {list = newList; currentPosition = (state.currentPosition + length + state.skipSize) % newList.Length ;skipSize = state.skipSize+1}

let solve lengths = 
    let calculatedState = lengths 
                            |> List.fold foldKnot {list = [0..255]; currentPosition=0;skipSize=0}
    calculatedState.list.[0]*calculatedState.list.[1]
    
let calculateSparseHash lengths =
    let sparseHashIteration state =
        lengths 
            |> List.fold foldKnot state

    let initialState = {list = [0..255]; currentPosition=0;skipSize=0}
    let finalState = 
        [0..63] 
        |> List.fold (fun state _ -> sparseHashIteration state) initialState
    finalState.list   

let calculateDenseHash sparseHash = 
    sparseHash
    |> List.chunkBySize 16
    |> List.map (fun chunk -> List.reduce (^^^) chunk)

let denseHashToHex denseHash = 
    denseHash
    |> List.map (fun hashPart ->  sprintf "%02x" hashPart)
    |> List.reduce (+)

let convertInput (input:string) = 
    input 
    |> Seq.map int 
    |> Seq.toList
    |> List.append <| [17; 31; 73; 47; 23]

let solve2 input =
    input 
    |> convertInput
    |> calculateSparseHash
    |> calculateDenseHash
    |> denseHashToHex

let knotHash =
    solve2