// Advent of Code 2021. Day 1, part A.
// dotnet fsi aoc01b.fsx

open System.IO

"input"
|> File.ReadAllLines 
|> Array.map int
|> Array.windowed 3
|> Array.pairwise 
|> Array.sumBy (fun (fst, snd) -> if Array.sum snd > Array.sum fst then 1 else 0)
|> printfn "%A"
