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

let readLines = 
    File.ReadAllLines
    >> Array.filter (fun line -> line <> String.Empty)
    >> Array.toList

let run fileName = 
    let lines = readLines fileName
    let movesList = lines |> List.map toMoves
    movesList |> printfn "%A"

run "sample"
