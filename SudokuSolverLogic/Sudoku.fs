module Sudoku

type Cell =
  | Guess of int
  | Given of int

let random = System.Random()

(* Get the indices of cells in the ith column of an nxn board (indexed from 0) *)
let getColumnIndices n i =
  [0..n-1] |> List.map (fun x -> (i, x))

(* Get the indices of cells in the ith row of an nxn board (indexed from 0)*)
let getRowIndices n i =
  [0..n-1] |> List.map (fun x -> (x, i))

(* Get the indices of cells in the ith box of an nxn board (indexed from 0, left to right, top to bottom) *)
let getBoxIndices n i =
  let sqrtN = int(sqrt (float(n))) in
  let rowRange =
    [0 .. sqrtN-1]
  in
  let xOffset =
    (i % sqrtN) * sqrtN
  in
  let yOffset =
    (i/sqrtN) * sqrtN
  in
  let rec buildList acc rangeList =
    match rangeList with
    | [] -> acc
    | h :: remainingRange ->
        let currentList = [xOffset .. (xOffset + sqrtN)-1] |> List.map (fun x -> (x, (yOffset + h))) in
            buildList (acc @ currentList) remainingRange
  in
  buildList [] rowRange

let getCellContents (board : Cell option[,]) indices =
  let rec buildList acc indexList =
    match indexList with
    | [] -> acc
    | (x, y) :: remainingIndices -> buildList (board.[x,y] :: acc) remainingIndices
  in
  buildList [] indices

let getRow board i =
  getCellContents board (getRowIndices (Array2D.length1 board) i)

let getColumn board i =
  getCellContents board (getColumnIndices (Array2D.length1 board) i)

let getBox board i =
  getCellContents board (getBoxIndices (Array2D.length1 board) i)

let getScore board =
  let getSectionScore cellContents =
    let someCells = cellContents |> List.choose id
    in
    let rec cellsToValues acc cells =
      match cells with
        | [] -> acc
        | Guess x :: tail -> cellsToValues (x :: acc) tail
        | Given x :: tail -> cellsToValues (x :: acc) tail
    in
    let cellValues = cellsToValues [] someCells in
      Set.count (Set.difference (Set.ofList [1 .. (Array2D.length1 board)]) (Set.ofList cellValues))
  in
  let rec buildScoreList acc rangeList =
    match rangeList with
      | [] -> acc
      | h :: remainingRange ->
        buildScoreList ((getSectionScore (getRow board h) +
                            getSectionScore (getColumn board h) +
                            getSectionScore (getBox board h)) :: acc)
                         remainingRange
  in
  List.sum (buildScoreList [] [0 .. (Array2D.length1 board) - 1])

let makeGuess board  =
  let x = random.Next (Array2D.length1 board) in
  let y = random.Next (Array2D.length1 board) in
  let currentGuess = random.Next(1, (Array2D.length1 board) + 1) in
  let cell = board.[x,y] in
  match cell with
    | Some (Given _) -> board
    | None | Some (Guess _) -> board.[x,y] <- Some (Guess currentGuess); board;

