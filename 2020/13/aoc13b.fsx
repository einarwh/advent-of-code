// Advent of Code 2020. 
// Day 13: Shuttle Search, Part B.
// dotnet fsi aoc13b.fsx

open System.IO

let rec findTime (time, inc) (offset, bus) =
    if (time + offset) % bus = 0L then
        (time, inc * bus)
    else
        findTime (time + inc, inc) (offset, bus)

let run (lines : string array) =
    let line = lines.[1]
    let buses =
        line.Split(',')
        |> Array.toList
        |> List.mapi (fun i b -> (i, b))
        |> List.filter (fun (_, b) -> b <> "x")
        |> List.map (fun (i, b) -> (int64 i, int64 b))
    match buses with
    | [] -> failwith "no buses"
    | (_, firstBus) :: remainingBuses ->
        remainingBuses
        |> List.fold findTime (0L, firstBus)
        |> fst
        |> printfn "%d"    

"input" |> File.ReadAllLines |> run 