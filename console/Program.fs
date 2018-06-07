// Learn more about F# at http://fsharp.org
// See the 'F# Tutorial' project for more help.

open adventofcode14
open QuickGraph

[<EntryPoint>]
let main argv = 
    let answer = solve2 "nbysizxe"
    printf "%i" answer
    0 // return an integer exit code
