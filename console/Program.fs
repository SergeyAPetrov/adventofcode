// Learn more about F# at http://fsharp.org
// See the 'F# Tutorial' project for more help.

open adventofcode12
open QuickGraph

[<EntryPoint>]
let main argv = 
    let someData = [1..10]
                    |> List.toSeq
                    |> Seq.windowed 2
                    //|> Seq.map (fun (pair:int[]) -> (pair.[0], pair.[1]))
                    |> Seq.map (fun pair -> Edge<int>(pair.[0], pair.[1]))
    let graph = someData.ToUndirectedGraph()
    
    printf "%i" 1
    0 // return an integer exit code
