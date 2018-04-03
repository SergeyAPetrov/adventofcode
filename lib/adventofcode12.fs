module adventofcode12

open QuickGraph
open QuickGraph.Algorithms.ConnectedComponents
open System
open System.Collections.Generic


type Vertex = {id: int}

let buildEdgeTuple (input:string) = 
    let stringParts = input.Split([|"<->"|], StringSplitOptions.RemoveEmptyEntries)
    let firstVertex =  System.Int32.Parse (stringParts.[0])
    let otherEnds = stringParts.[1].Split([|", "|], StringSplitOptions.RemoveEmptyEntries)
                        |> Seq.map System.Int32.Parse
    otherEnds
    |> Seq.map (fun e -> (firstVertex, e)) 
    
let buildGraph  (input:string) = 
   let edges = 
       input.Split [|'\n'|]
       |> Seq.map buildEdgeTuple
       |> Seq.concat
       |> Seq.map (fun (a, b) -> ({id = a}, {id = b}))
       |> Seq.map (fun x -> Edge<Vertex> x)
   edges.ToUndirectedGraph()

let solve input =
    let graph = buildGraph input
    let x = QuickGraph.Algorithms.ConnectedComponents.ConnectedComponentsAlgorithm(graph)
    x.Compute()
    x.Components 
        |> Seq.groupBy (fun kv -> kv.Value)
        |> Seq.where (fun (comp, vertices) -> 
            let o = vertices 
                    |> Seq.tryFind (fun (vertex:KeyValuePair<Vertex,int>) -> vertex.Key.id = 0)
            o.IsSome)
        |> Seq.exactlyOne
        |> snd
        |> Seq.length
        
let solve2 input =
    let graph = buildGraph input
    let x = QuickGraph.Algorithms.ConnectedComponents.ConnectedComponentsAlgorithm(graph)
    x.Compute()
    x.Components 
        |> Seq.groupBy (fun kv -> kv.Value)
        |> Seq.length