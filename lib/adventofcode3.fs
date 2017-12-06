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