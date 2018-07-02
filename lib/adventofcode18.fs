module adventofcode18

let input = @"set i 31
set a 1
mul p 17
jgz p p
mul a 2
add i -1
jgz i -2
add a -1
set i 127
set p 618
mul p 8505
mod p a
mul p 129749
add p 12345
mod p a
set b p
mod b 10000
snd b
add i -1
jgz i -9
jgz a 3
rcv b
jgz b -1
set f 0
set i 126
rcv a
rcv b
set p a
mul p -1
add p b
jgz p 4
snd a
set a b
jgz 1 3
snd b
set f 1
add i -1
jgz i -11
snd a
jgz f -16
jgz a -19"

let instructions = 
    input.Split [|'\n'|]
    |> Array.map (fun line -> line.Trim('\r'))
    |> Array.map (fun line -> line.Split [|' '|])

let solve =
    let registersInit = ['a'..'z'] |> List.map ( fun char-> (char.ToString(), 0L)) |> Map.ofList

    let rec iteration (registers:Map<string, int64>) sound (instructionIndex:int64) =
        let eval (value:string) =
            if System.Char.IsLetter value.[0] then
                registers.[value]
            else
                int64 value

        let instruction = instructions.[int instructionIndex]
        let a = instruction.[1]

        match instruction.[0] with
        | "snd" -> iteration registers (eval a) (instructionIndex+1L)
        | "set" -> iteration (registers.Add(a, eval instruction.[2])) sound (instructionIndex+1L)
        | "add" -> iteration (registers.Add(a, (registers.[a] + eval instruction.[2]))) sound (instructionIndex+1L)
        | "mul" -> iteration (registers.Add(a, (registers.[a] * eval instruction.[2]))) sound (instructionIndex+1L)
        | "mod" -> iteration (registers.Add(a, (registers.[a] % eval instruction.[2]))) sound (instructionIndex+1L)
        | "rcv" -> if (eval a) = 0L then iteration registers sound (instructionIndex+1L) else sound
        | "jgz" -> if (eval a) > 0L then iteration registers sound (instructionIndex + (eval instruction.[2])) else iteration registers sound (instructionIndex+1L)

    iteration registersInit 0L 0L



type Queues = 
    {
        input : int64 list 
        output : int64 list
    }

let send queues value =
    { queues with output =  queues.output @ [value]}

let recieve queues =
    (queues.input.Head, {queues with input =  queues.input.Tail})

let solve2 =
    let registersInit = ['a'..'z'] |> List.map ( fun char-> (char.ToString(), 0L)) |> Map.ofList
    let registers0 = registersInit.Add("p", 0L)
    let registers1 = registersInit.Add("p", 1L)

    let rec iteration (registers:Map<string, int64>) queues (instructionIndex:int64) =
        let eval (value:string) =
            if System.Char.IsLetter value.[0] then
                registers.[value]
            else
                int64 value

        let instruction = instructions.[int instructionIndex]
        let a = instruction.[1]

        match instruction.[0] with
        | "snd" -> iteration registers (send queues (eval a)) (instructionIndex+1L)
        | "set" -> iteration (registers.Add(a, eval instruction.[2])) queues (instructionIndex+1L)
        | "add" -> iteration (registers.Add(a, (registers.[a] + eval instruction.[2]))) queues (instructionIndex+1L)
        | "mul" -> iteration (registers.Add(a, (registers.[a] * eval instruction.[2]))) queues (instructionIndex+1L)
        | "mod" -> iteration (registers.Add(a, (registers.[a] % eval instruction.[2]))) queues (instructionIndex+1L)
        | "rcv" -> if (queues.input.IsEmpty) then (registers, queues, instructionIndex) else 
                                                                                            let (value, queues) = recieve queues
                                                                                            iteration (registers.Add(a, value)) queues (instructionIndex+1L)
        | "jgz" -> if (eval a) > 0L then iteration registers queues (instructionIndex + (eval instruction.[2])) else iteration registers queues (instructionIndex+1L)
    
    let rec doubleIteration registers0 index0 registers1 index1 queues counter =
        let reg0, queues0, index0 = iteration registers0 {input = queues.output; output = queues.input} index0
        let reg1, queues1, index1 = iteration registers1 {input = queues0.output; output = queues0.input} index1
        if queues1.input.IsEmpty && queues1.output.IsEmpty 
        then
            counter
        else
            doubleIteration reg0 index0 reg1 index1 queues1 (counter + queues1.output.Length)

    doubleIteration registers0 0L registers1 0L {input = []; output = []} 0

