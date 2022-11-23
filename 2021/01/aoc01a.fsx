// Advent of Code 2021. Day 1, part A.
// dotnet fsi aoc01a.fsx

open System
open System.IO

"input"
|> File.ReadAllLines 
|> Array.map int
|> Array.pairwise 
|> Array.sumBy (fun (fst, snd) -> if snd > fst then 1 else 0)
|> printfn "%d"
