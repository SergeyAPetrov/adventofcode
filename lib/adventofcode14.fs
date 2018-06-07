module adventofcode14

open adventofcode10
open QuickGraph
open System

let intToBinary (i:int) =
    Convert.ToString(i, 2).PadLeft(4, '0');
   
let solve input =
    let hashInputs = [0..127] |> List.map (fun number -> sprintf "%s-%i" input number)
    let binHashes = hashInputs 
                    |> List.map knotHash 
                    |> List.fold (+) ""
                    |> Seq.map (fun x-> x.ToString())
                    |> Seq.map (fun x-> Int32.Parse(x, Globalization.NumberStyles.HexNumber))
                    |> Seq.map intToBinary
    binHashes
        |> Seq.map (fun binHash -> (Seq.countBy id binHash |> Seq.tryFind (fun e -> fst e = '1')))
        |> Seq.where (fun x-> x.IsSome)
        |> Seq.map (fun x -> snd (Option.get x))
        |> Seq.sum

let hash2Binary hash = 
    let binaries =
        hash
        |> Seq.map (fun x -> x.ToString())
        |> Seq.map (fun x -> Int32.Parse(x, Globalization.NumberStyles.HexNumber))
        |> Seq.map intToBinary
    String.Concat(binaries)

type Vertex = {i: int; j : int}

let line2Edges line i= 
    line
    |> Seq.mapi ( fun j ch -> (j,ch))
    |> Seq.windowed 2
    |> Seq.where (fun window -> snd window.[0] = '1' && snd window.[1] = '1')
    |> Seq.map (fun window -> Edge({i=i;j=fst window.[0]},{i=i;j=fst window.[1]}))

let columns2Edges lineOne lineTwo i =
    Seq.mapi2 (fun j ch1 ch2 -> (j,ch1,ch2)) lineOne lineTwo
    |> Seq.where (fun (_, ch1, ch2) -> ch1='1' && ch2='1')
    |> Seq.map (fun (j, _, _) -> Edge({i=i;j=j},{i=i+1;j=j}))

let solve2 input =
    let hashInputs = [0..127] |> List.map (fun number -> sprintf "%s-%i" input number)
    let hashes = hashInputs 
                    |> List.map knotHash 
    let binaryStrings = hashes |> List.map hash2Binary
    let graph = new UndirectedGraph<Vertex, Edge<Vertex>>();
    for i in 0..binaryStrings.Length-1 do
        let currentLine = binaryStrings.Item i
        let vertices = currentLine
                    |> Seq.mapi (fun j char -> (j, char))
                    |> Seq.where (fun (_, char) -> char = '1')
                    |> Seq.map (fun (j, _) -> {i=i; j=j})
        graph.AddVertexRange(vertices) |> ignore
    
    let horizontalEdges = binaryStrings
                            |> List.mapi (fun i str -> line2Edges str i)
                            |> List.map Seq.toList
                            |> List.concat
    graph.AddEdgeRange(horizontalEdges) |> ignore

    let verticalEdges = binaryStrings
                            |> List.mapi (fun i str -> (i,str))
                            |> List.windowed 2
                            |> List.map (fun window -> columns2Edges (snd window.[0]) (snd window.[1]) (fst window.[0]))
                            |> List.map Seq.toList
                            |> List.concat
    graph.AddEdgeRange(verticalEdges) |> ignore
    let x = QuickGraph.Algorithms.ConnectedComponents.ConnectedComponentsAlgorithm(graph)
    x.Compute()
    x.Components 
        |> Seq.groupBy (fun kv -> kv.Value)
        |> Seq.length
    //let componentsMap = Array2D.create 128 128 0

    
                