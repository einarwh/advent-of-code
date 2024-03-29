// Advent of Code 2022. Day 2: Rock Paper Scissors.
// dotnet fsi aoc02.fsx

open System.IO

let score1 = function 
    | "A X" -> 4
    | "A Y" -> 8
    | "A Z" -> 3
    | "B X" -> 1
    | "B Y" -> 5
    | "B Z" -> 9
    | "C X" -> 7
    | "C Y" -> 2
    | "C Z" -> 6
    | _ -> 0

let score2 = function 
    | "A X" -> 3
    | "A Y" -> 4 
    | "A Z" -> 8 
    | "B X" -> 1 
    | "B Y" -> 5 
    | "B Z" -> 9 
    | "C X" -> 2 
    | "C Y" -> 6 
    | "C Z" -> 7 
    | _ -> 0 

let run lines = 
    lines |> Array.sumBy score1 |> printfn "%d"
    lines |> Array.sumBy score2 |> printfn "%d"

"input" |> File.ReadAllLines |> run 
