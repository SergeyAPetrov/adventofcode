module adventofcode15

let divider = 2147483647L

let generatorFunction (value, factor) =
    let nextValue = (value * factor) % divider
    Some (nextValue, (nextValue, factor))
    
let seqA = Seq.unfold generatorFunction (679L, 16807L)
let seqB = Seq.unfold generatorFunction (771L, 48271L)

let getLowBits (value:int64) =
    value &&& 0xFFFFL

let jointSeq = Seq.zip seqA seqB


let solve seq amount = 
    seq  
    |> Seq.take amount
    |> Seq.where (fun (a, b) -> getLowBits a = getLowBits b)
    |> Seq.length

let solve1 = solve jointSeq 40000000

let seqAv2 = seqA |> Seq.where (fun x-> x%4L=0L)
let seqBv2 = seqB |> Seq.where (fun x-> x%8L=0L)

let jointSeqv2 = Seq.zip seqAv2 seqBv2

let solve2 = solve jointSeqv2 5000000