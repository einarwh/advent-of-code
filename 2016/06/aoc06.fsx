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

let getLeastCommon (chars : char list) = 
    chars 
    |> List.countBy id 
    |> List.sortBy (fun (ch, count) -> count)
    |> List.head 
    |> fst 

let getErrorCorrectedMessage (charSelector : char list -> char)(messages : string list) = 
    let length = messages |> List.head |> Seq.length
    [| 0 .. length - 1 |]
    |> Array.map (fun index -> messages |> List.map (fun m -> m[index]) |> charSelector)
    |> fun chars -> new string(chars)

let run fileName = 
    let messages = readLines fileName
    messages |> getErrorCorrectedMessage getMostCommon |> printfn "%s"
    messages |> getErrorCorrectedMessage getLeastCommon |> printfn "%s"

run "input"
