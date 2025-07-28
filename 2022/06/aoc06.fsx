// Advent of Code 2022. 
// Day 6: Tuning Trouble.
// dotnet fsi aoc06.fsx

open System.IO

let isMarker count cs = 
    count = (cs |> Set.ofSeq |> Set.count)

let findMarker count = 
    Seq.windowed count 
    >> Seq.findIndex (isMarker count)
    >> (+) count 

let run cs = 
    cs |> findMarker 4 |> printfn "%d"
    cs |> findMarker 14 |> printfn "%d"

"input.txt"
|> File.ReadAllText
|> run
