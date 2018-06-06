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


let solve2 input =
    let hashInputs = [0..127] |> List.map (fun number -> sprintf "%s-%i" input number)
    let hashes = hashInputs 
                    |> List.map knotHash 
    let binaryStrings = hashes |> List.map hash2Binary
    let graph = new UndirectedGraph<Vertex, Edge<Vertex>>();
    for i in 0..binaryStrings.Length do
        let currentLine = binaryStrings.Item i
        let vertices = currentLine
                    |> Seq.mapi (fun j char -> (j, char))
                    |> Seq.where (fun (_, char) -> char = '1')
                    |> Seq.map (fun (j, _) -> {i=i; j=j})
        graph.AddVertexRange(vertices) |> ignore
    
    1
    //let componentsMap = Array2D.create 128 128 0

    
                