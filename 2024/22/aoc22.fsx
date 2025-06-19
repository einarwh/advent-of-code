// Advent of Code 2024. Day 22: Monkey Market. Part 1.
// dotnet fsi aoc22.fsx

open System.IO

let mix a b = a ^^^ b 
let prune x = x &&& 16777215
let step1 x = prune (mix (x <<< 6) x)
let step2 x = prune (mix (x >>> 5) x) 
let step3 x = prune (mix (x <<< 11) x)
let next = step1 >> step2 >> step3 
let twok x = 
    Seq.unfold (fun n -> Some (n, next n)) x |> Seq.item 2000

let run fileName =
    let lines = File.ReadAllLines fileName
    lines |> Seq.toList |> List.map (int >> twok >> int64) |> List.sum |> printfn "%d"

run "input"
