module adventofcode6

let parseInput (input:string) =
    input.Split(' ')
    |> Array.map System.Int32.Parse

let toAddOrNotToAdd i j m n x =
    if i < j && j <=i+m then
        x+1
    else if 0 <=j && j <= i+m-n then
        x+1
    else
        x

let redistribute state = 
    let max = Array.max state
    let i = Array.findIndex (fun i -> i=max) state
    let n = state.Length
    let d = max / n
    let m = max % n
    Array.mapi (fun j x -> if j <> i then x + d else x + d - max) state
    |> Array.mapi (fun j x -> toAddOrNotToAdd i j m n x)
    
let getNextSeqState state =
    let (currentState, history) = state
    let nextState = redistribute currentState
    if List.contains nextState history then
        let loopLength = List.findIndex (fun x -> x = nextState) history
        Some (loopLength, (nextState, nextState::history))
    else
        Some (0, (nextState, nextState::history))
    
let solve input =
    let data = parseInput input
    let seq = Seq.unfold getNextSeqState (data,[])
    Seq.find (fun x-> x > 0) seq
    |> (+) 1

let test = @"0 2 7 0"

let input = @"2 8 8 5 4 2 3 1 5 5 1 2 15 13 5 14"