// Learn more about F# at http://fsharp.org
// See the 'F# Tutorial' project for more help.
open Sudoku
open TabuSearch

[<EntryPoint>]
let main argv = 
    let result =
        let startingBoard = Array2D.create 4 4 None
        in
        doSearch
          []
          40000
          startingBoard
          startingBoard
          (fun board -> (getScore board) = 0)
          (fun board -> (makeGuess board) :: [])
          (fun board -> -1 * (getScore board))

    printfn "%A" result
    0 // return an integer exit code
