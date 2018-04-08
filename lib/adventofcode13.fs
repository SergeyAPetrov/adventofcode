module adventofcode13

open System

type Layer = 
    {
        Depth : int
        Range : int
        Severity : int
    }
    
let parseItem (item:string) = 
    let parts = item.Split([|": "|], StringSplitOptions.RemoveEmptyEntries)
                    |> Array.map System.Int32.Parse
    {Depth = parts.[0]; Range=parts.[1]; Severity = parts.[0] * parts.[1]}
    
let parseInput (input:string) = 
    input.Split [|'\n'|]
    |> Seq.map parseItem 
    
let isCaught layer  =
    layer.Depth % (2*(layer.Range-1)) = 0 
    
let isCaughtWithWait wait layer = 
    isCaught {layer with Depth = layer.Depth + wait}

let solve input =
    let layers = parseInput input
    layers 
    |> Seq.where isCaught
    |> Seq.map (fun l -> l.Severity)
    |> Seq.sum

let checkWithWaitTime layers wait = 
    layers
    |> Seq.forall (fun layer -> not (isCaughtWithWait wait layer))

let solve2 input =
    let layers = parseInput input
    let checkingFunction = checkWithWaitTime layers

    let waitTimes = Seq.initInfinite (fun i -> i)
    waitTimes
    |> Seq.find checkingFunction
    

let test = @"0: 3
             1: 2
             4: 4
             6: 4"
 
let puzzle = @"0: 3
               1: 2
               2: 4
               4: 6
               6: 4
               8: 6
               10: 5
               12: 8
               14: 8
               16: 6
               18: 8
               20: 6
               22: 10
               24: 8
               26: 12
               28: 12
               30: 8
               32: 12
               34: 8
               36: 14
               38: 12
               40: 18
               42: 12
               44: 12
               46: 9
               48: 14
               50: 18
               52: 10
               54: 14
               56: 12
               58: 12
               60: 14
               64: 14
               68: 12
               70: 17
               72: 14
               74: 12
               76: 14
               78: 14
               82: 14
               84: 14
               94: 14
               96: 14"