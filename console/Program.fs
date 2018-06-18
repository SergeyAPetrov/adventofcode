// Learn more about F# at http://fsharp.org
// See the 'F# Tutorial' project for more help.

open adventofcode16
open QuickGraph

[<EntryPoint>]
let main argv = 
    let answer = solve2 input
    printf "%s" (System.String.Concat(Array.ofList(answer)))
    0 // return an integer exit code
