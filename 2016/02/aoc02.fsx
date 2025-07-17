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

let makeMoves (move : int*int -> Move -> int*int) (pos : int * int) (moves : Move list) = 
    let rec fn pos moves = 
        match moves with 
        | [] -> pos 
        | h :: rest -> 
            fn (move pos h) rest  
    fn pos moves

let toDigit (x, y) = 
    y * 3 + x + 1

let findCode (move : int*int -> Move -> int*int) (movesList : Move list list) = 
    let rec fn (pos : int * int) (acc : (int * int) list) (lst : Move list list) = 
        match lst with 
        | [] -> acc |> List.rev |> List.map toDigit |> List.map string |> String.concat ""
        | h :: rest -> 
            let pos' = makeMoves move pos h 
            fn pos' (pos' :: acc) rest
    fn (1, 1) [] movesList

let readLines = 
    File.ReadAllLines
    >> Array.filter (fun line -> line <> String.Empty)
    >> Array.toList

let run fileName = 
    let lines = readLines fileName
    let movesList = lines |> List.map toMoves
    movesList |> findCode moveSimpleKeypad |> printfn "%A"

run "sample"
