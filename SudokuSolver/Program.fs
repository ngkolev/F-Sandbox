let SUB_MATRIX_SIZE = 3
let SUB_MATRIX_COUNT = 3
let MATRIX_SIZE = SUB_MATRIX_SIZE * SUB_MATRIX_COUNT

let getSubMatrice (sudoku: int [,]) i j =
    let xStart, yStart = i * SUB_MATRIX_SIZE, j * SUB_MATRIX_SIZE
    let endIndexDelta = SUB_MATRIX_SIZE - 1
    let xEnd, yEnd = xStart + endIndexDelta, yStart + endIndexDelta
    sudoku.[xStart .. xEnd, yStart .. yEnd]

let getSubMatrices (sudodku: int [,]) =
      seq { for i in 0 .. SUB_MATRIX_COUNT - 1 do
            for j in 0 .. SUB_MATRIX_COUNT - 1 ->
                getSubMatrice sudodku i j |> Seq.cast }

let getRows (sudoku: int[,]) =
    seq { for i in [1 .. MATRIX_SIZE - 1] -> sudoku.[i, *] |> Seq.cast }

let getColumns (sudoku: int[,]) =
    seq { for j in [1 .. MATRIX_SIZE - 1] -> sudoku.[*, j] |> Seq.cast }

let checkSudoku (sudoku: int [,]) =
    [getSubMatrices; getRows; getColumns] 
    |> Seq.map (fun f -> f sudoku)
    |> Seq.concat
    |> Seq.forall (fun r -> r |> Set.ofSeq |> Seq.length |> (=) (r |> Seq.length))
    

[<EntryPoint>]
let main argv = 
    printfn "%A" argv
    0
