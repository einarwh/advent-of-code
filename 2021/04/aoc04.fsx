// Advent of Code 2021. Day 4.
// dotnet fsi aoc04.fsx

open System
open System.IO
open System.Text.RegularExpressions

type BoardNumber = int option
type BoardSequence = BoardNumber list
type Board = BoardNumber[,]

let parseNumbers (numberString : string) : int list =
    numberString.Split(",") 
    |> Array.toList 
    |> List.map int 

let parseRow (rowString : string) : BoardSequence = 
    Regex(" +").Split(rowString.Trim())
    |> Array.toList 
    |> List.map (int >> Some)

let parseBoard (boardString : string) : Board = 
    boardString.Split("\n") 
    |> Array.toList 
    |> List.map parseRow 
    |> array2D

let parseAllBoards (boardStrings : string list) : Board list = 
    boardStrings |> List.map parseBoard 
    
let parseText (text : string) = 
    match text.Trim().Split("\n\n") |> Array.toList with 
    | [] -> failwith "Invalid input."
    | h :: t -> 
        let numbers = parseNumbers h 
        let boards = parseAllBoards t
        (numbers, boards)

let updateBoard (number : int) (board : Board) = 
    let update = Option.bind (fun n -> if n = number then None else Some n)
    board |> Array2D.map update

let rows (board : Board) : BoardSequence list = 
    let rowCount = board |> Array2D.length1 
    [0 .. rowCount - 1] |> List.map (fun r -> board[r, 0..] |> Array.toList)

let columns (board : Board) : BoardSequence list = 
    let colCount = board |> Array2D.length2
    [0 .. colCount - 1] |> List.map (fun c -> board[0.., c] |> Array.toList)

let winningSequence (sequence : BoardSequence) : bool = 
    sequence |> List.forall Option.isNone

let winningBoard (board : Board) : bool = 
    let lists = rows board @ columns board 
    lists |> List.exists winningSequence

let losingBoard board = board |> winningBoard |> not

let calculateScore lastNumber board = 
    let sumOfUnmarked = 
        board 
        |> Seq.cast<int option> 
        |> Seq.choose id 
        |> Seq.sum
    lastNumber * sumOfUnmarked

let rec play (playToWin : bool) (numbers : int list) (boards : Board list) = 
    match numbers with 
    | [] -> failwith "game over, no winner"
    | number :: remaining -> 
        let updatedBoards = boards |> List.map (updateBoard number)
        let winners = updatedBoards |> List.filter winningBoard
        match winners with 
        | [] -> play playToWin remaining updatedBoards 
        | winner :: t -> 
            if playToWin then 
                (number, winner)
            else 
                let losers = updatedBoards |> List.except winners
                if List.isEmpty losers then 
                    (number, winner)
                else 
                    play playToWin numbers losers

let run (text : string) = 
    match parseText text with 
    | (numbers, boards) ->
        let (winningNumber, winningBoard) = play true numbers boards
        calculateScore winningNumber winningBoard |> printfn "Win: %d"
        let (losingNumber, losingBoard) = play false numbers boards
        calculateScore losingNumber losingBoard |> printfn "Lose: %d"

"input"
|> File.ReadAllText
|> run 
