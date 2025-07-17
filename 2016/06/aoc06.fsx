// Advent of Code 2016. Day 06: Signals and Noise.
// dotnet fsi aoc06.fsx

open System
open System.IO

let readLines = 
    File.ReadAllLines
    >> Array.filter (fun line -> line <> String.Empty)
    >> Array.toList

let getMostCommon (chars : char list) = 
    chars 
    |> List.countBy id 
    |> List.sortByDescending (fun (ch, count) -> count)
    |> List.head 
    |> fst 

let getErrorCorrectedMessage (messageLength : int) (messages : string list) = 
    [| 0 .. messageLength - 1 |]
    |> Array.map (fun index -> messages |> List.map (fun m -> m[index]) |> getMostCommon)
    |> fun chars -> new string(chars)

let run fileName = 
    let lines = readLines fileName
    let length = lines |> List.head |> Seq.length
    let message = getErrorCorrectedMessage length lines 
    message |> printfn "%s"

run "input"
