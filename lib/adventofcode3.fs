module adventofcode3

let squares = Seq.initInfinite (fun i ->
    pown (2*i+1) 2
    )

let closestSquare number =
    let topSquareIndex = Seq.findIndex (fun x -> x>number) squares
    (topSquareIndex, Seq.item (topSquareIndex-1) squares)

let axisPoints i prevSquare =
    [prevSquare + i; prevSquare + 3*i; prevSquare + 5*i; prevSquare + 7*i]

let distanceToClosestPoint (number:int) points = 
    List.map (fun i -> abs (i-number) ) points
    |> List.min

let solve number = 
    let (i, square) = closestSquare number
    axisPoints i square
    |> distanceToClosestPoint number
    |> (+) i


type State =
    {
        n: int
        prev: int
    }
let xSpiralCoordinates = Seq.unfold (
                                        fun state -> (
                                                        if state.n = 1 then
                                                            Some (0, {n=2;prev=0})
                                                        else
                                                            let x = float(4*(state.n-2)+1)
                                                            let k = int(System.Math.Floor(sqrt x)) % 4;  
                                                            let y = (float state.prev) + sin(float(k)*System.Math.PI/float(2))
                                                            let z = int y
                                                            Some (z, {n=state.n+1; prev=z})
                                                     )
                                     ) {n=1;prev=0}

let ySpiralCoordinates = Seq.unfold (
                                        fun state -> (
                                                        if state.n = 1 then
                                                            Some (0, {n=2;prev=0})
                                                        else
                                                            let x = float(4*(state.n-2)+1)
                                                            let k = int(System.Math.Floor(sqrt x)) % 4;  
                                                            let y = (float state.prev) - cos(float(k)*System.Math.PI/float(2))
                                                            let z = int y
                                                            Some (z, {n=state.n+1; prev=z})
                                                     )
                                     ) {n=1;prev=0}

let getNeighbors (x,y) =
    [(x+1,y);(x+1,y+1);(x,y+1);(x-1,y+1);(x-1,y);(x-1,y-1);(x,y-1);(x+1,y-1)]
    
type pointWithValue =
    {
        point:int*int
        value:int
    }

let xy = Seq.zip xSpiralCoordinates ySpiralCoordinates

let solve2rec number = 
    let cells = xy.GetEnumerator()
    cells.MoveNext() |> ignore
    let rec solve state =
        cells.MoveNext() |> ignore
        let p = cells.Current
        let neighbors = getNeighbors p
        let nextValue = state
                            |> List.where (fun x-> List.contains x.point neighbors) 
                            |> List.map (fun x-> x.value)
                            |> List.sum
        if nextValue > number then
            nextValue
        else
            solve ({point = p; value = nextValue} :: state)
    solve [{point=cells.Current;value=1}]

let solve2 number = 
    Seq.fold 
        (fun (state:pointWithValue list) (p:int*int) 
            -> 
                if p=(0,0) then
                    [{point=p;value=1}]
                else
                    let neighbors = getNeighbors p
                    let nextValue = state
                                        |> List.where (fun x-> List.contains x.point neighbors) 
                                        |> List.map (fun x-> x.value)
                                        |> List.sum
                    if nextValue > number then
                        failwith (sprintf "%i" nextValue)
                    {point = p; value = nextValue} :: state
        ) 
        [] xy
