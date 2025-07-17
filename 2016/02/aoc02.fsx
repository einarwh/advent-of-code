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

let move (x, y) (move : Move) = 
    match move with 
    | U -> (x, max 0 (y - 1))
    | D -> (x, min 2 (y + 1))
    | L -> (max 0 (x - 1), y)
    | R -> (min 2 (x + 1), y)

let rec makeMoves (pos : int * int) (moves : Move list) = 
    match moves with 
    | [] -> pos 
    | h :: rest -> 
        makeMoves (move pos h) rest  

let toDigit (x, y) = 
    y * 3 + x + 1

let findCode (movesList : Move list list) = 
    let rec fn (pos : int * int) (acc : (int * int) list) (lst : Move list list) = 
        match lst with 
        | [] -> acc |> List.rev |> List.map toDigit |> List.map string |> String.concat ""
        | h :: rest -> 
            let pos' = makeMoves pos h 
            fn pos' (pos' :: acc) rest
    fn (1, 1) [] movesList

let readLines = 
    File.ReadAllLines
    >> Array.filter (fun line -> line <> String.Empty)
    >> Array.toList

let run fileName = 
    let lines = readLines fileName
    let movesList = lines |> List.map toMoves
    movesList |> findCode |> printfn "%A"

run "input"
