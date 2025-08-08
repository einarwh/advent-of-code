// Advent of Code 2021. Day 1: Sonar Sweep.
// dotnet fsi aoc01a.fsx

open System.IO

let run fileName = 
    let numbers = fileName |> File.ReadAllLines |> Array.map int 
    numbers 
    |> Array.pairwise 
    |> Array.sumBy (fun (fst, snd) -> if snd > fst then 1 else 0)
    |> printfn "%d"
    numbers
    |> Array.windowed 3
    |> Array.pairwise 
    |> Array.sumBy (fun (fst, snd) -> if Array.sum snd > Array.sum fst then 1 else 0)
    |> printfn "%A"

"input.txt" |> run 
