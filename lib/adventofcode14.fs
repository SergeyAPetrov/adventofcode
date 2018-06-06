module adventofcode14

open adventofcode10
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

        
let solve2 input =
    let hashInputs = [0..127] |> List.map (fun number -> sprintf "%s-%i" input number)
    let binHashes = hashInputs 
                    |> List.map knotHash 
                    |> List.fold (+) ""
                    |> Seq.map (fun x-> x.ToString())
                    |> Seq.map (fun x-> Int32.Parse(x, Globalization.NumberStyles.HexNumber))
                    |> Seq.map intToBinary
                    //|> Seq.map (fun x -> x.Replace('1', 'x'))
                    |> Seq.chunkBySize 128
                    |> Seq.map (fun line -> String.concat(Array.toSeq line))
                    |> Seq.map (fun line -> Seq.toArray line)
                    |> Seq.toArray
    1
    //let componentsMap = Array2D.create 128 128 0

    
                