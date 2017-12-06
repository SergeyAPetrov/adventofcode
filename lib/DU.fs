module DU

type Build = 
    | Success of int
    | Fail of string list

let s = Success 1
let f = Fail ["a";"b"]


let PrintBuild (build : Build) = 
    match build with
    | Success s -> printfn "%i" s
    | Fail f -> printfn "%s" f.[0]

PrintBuild s
PrintBuild f
//DU
//type IntOrBool = 
//  | I of int
//  | B of bool
    
//let i  = I 99    // use the "I" constructor
//// val i : IntOrBool = I 99

//let b  = B true  // use the "B" constructor
//// val b : IntOrBool = B true

