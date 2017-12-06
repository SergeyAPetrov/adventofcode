#if INTERACTIVE
#else
module sandbox
#endif

let x = 42
let sum (a : float) (b:float) : float = a + b

let printOdd min max =
    let isOdd number = 
        number % 2 = 0
    for number in min..max do
        if isOdd number then
            printfn "%i" number

let tuplize a b =
    a,b

open System.IO

let files = Directory.EnumerateFiles (@"c:\stuff", "*.*")

let tupSum (a,b) = a + b

let array = [|1..3|]

let echo a = 
    a

let array2 = 
    Array.init 10 echo

let array3 = 
    Array.init 10 (fun i-> i)

let isOdd number = 
    number % 2 = 0

let evenArray = Array.filter isOdd array3

let PrintSquares min max = 
    [|min..max|]
    |> Array.map (fun i -> i*i)

//TODO: array vs list - immutable array vs seq - implement ienumerable