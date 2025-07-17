// Advent of Code 2016. Day 02: Bathroom Security.
// dotnet fsi aoc02.fsx

open System
open System.IO

type Move = U | D | L | R 

let toMoves (s : string) =
    let toMove (ch : char) = 
        match ch with 
        | 'U' -> U 
        | 'D' -> D
        | 'L' -> L 
        | 'R' -> R 
        | _ -> failwith <| sprintf "%c ?" ch
    s |> Seq.toList |> List.map toMove

let moveSimpleKeypad (x, y) (move : Move) = 
    match move with 
    | U -> (x, max 0 (y - 1))
    | D -> (x, min 2 (y + 1))
    | L -> (max 0 (x - 1), y)
    | R -> (min 2 (x + 1), y) 

let moveComplexKeypad (x, y) (move : Move) = 
    let (x', y') = 
        match move with 
        | U -> (x, y - 1)
        | D -> (x, y + 1)
        | L -> (x - 1, y)
        | R -> (x + 1, y) 
    if abs x' + abs y' > 2 then (x, y) else (x', y')

let makeMoves (move : int*int -> Move -> int*int) (pos : int * int) (moves : Move list) = 
    let rec fn pos moves = 
        match moves with 
        | [] -> pos 
        | h :: rest -> 
            fn (move pos h) rest  
    fn pos moves

let toButtonPart1 (x, y) = 
    y * 3 + x + 1 |> string

let toButtonPart2 (x, y) = 
    match (x, y) with 
    | (0, -2)  -> "1"
    | (-1, -1) -> "2"
    | (0, -1)  -> "3"
    | (1, -1)  -> "4"
    | (-2, 0) -> "5"
    | (-1, 0)  -> "6"
    | (0, 0)  -> "7"
    | (1, 0)  -> "8"
    | (2, 0)  -> "9"
    | (-1, 1)  -> "A"
    | (0, 1)  -> "B"
    | (1, 1)  -> "C"
    | (0, 2)  -> "D"
    | _ -> failwith "out of bounds"

let findCode (startPos : int*int) (move : int*int -> Move -> int*int) (toButton : int*int -> string) (movesList : Move list list) = 
    let rec fn (pos : int * int) (acc : (int * int) list) (lst : Move list list) = 
        match lst with 
        | [] -> acc |> List.rev |> List.map toButton |> String.concat ""
        | h :: rest -> 
            let pos' = makeMoves move pos h 
            fn pos' (pos' :: acc) rest
    fn startPos [] movesList

let readLines = 
    File.ReadAllLines
    >> Array.filter (fun line -> line <> String.Empty)
    >> Array.toList

let run fileName = 
    let lines = readLines fileName
    let movesList = lines |> List.map toMoves
    movesList |> findCode (1, 1) moveSimpleKeypad toButtonPart1 |> printfn "%A"
    movesList |> findCode (-2, 0) moveComplexKeypad toButtonPart2 |> printfn "%A"

run "input"
