// Contants
let SUB_MATRIX_SIZE = 3
let SUB_MATRIX_COUNT = 3
let MATRIX_SIZE = SUB_MATRIX_SIZE * SUB_MATRIX_COUNT

// Check if Sudoku is finsihed
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
    
// Find Sudoku solution
let getNextEmptyCell (sudoku: int[,]) =
    Array2D.init MATRIX_SIZE MATRIX_SIZE (fun i j -> i, j)
    |> Seq.cast
    |> Seq.tryFind (fun (i, j) -> sudoku.[i, j] = 0)
    |> function 
        | Some x -> x
        | _ -> -1, -1

type Sudoko = 
    | SolvedSudoku of int[,]
    | NotSolvedSudoku

let matrixWithChange (sudoku: _[,]) i j x =
     seq { for i' in 0 .. SUB_MATRIX_COUNT - 1 -> 
            seq { for j' in 0 .. SUB_MATRIX_COUNT - 1 ->
                    if i' = i && j' = j  then x else sudoku.[i, j] }}

let rec getSolution (sudoku: int[,]) =
    let (i, j) = getNextEmptyCell sudoku

    if (i, j) <> (-1, -1) then 
        seq { for x in [1 .. 9] -> matrixWithChange sudoku i j x }
        |> Seq.map (fun y -> getSolution (array2D y))
        |> Seq.tryFind ((<>) NotSolvedSudoku)
        |> function 
            | Some x -> x
            | _ -> NotSolvedSudoku
    elif checkSudoku sudoku then SolvedSudoku(sudoku)
    else NotSolvedSudoku
    
// Main entry point
[<EntryPoint>]
let main argv = 
    0
