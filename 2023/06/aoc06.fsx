// Advent of Code 2023. Day 6: Wait For It
// dotnet fsi aoc06.fsx

open System
open System.IO
open System.Text.RegularExpressions

let parse (s : string) : string list =
    Regex.Matches(s, "\d+") 
    |> Seq.map (fun m -> m.Value)
    |> Seq.toList

let parseDoubles = parse >> List.map double

let parseDouble = parse >> String.concat "" >> double

let solve (time : double, distance : double) = 
    let v = sqrt(time * time - 4. * distance)
    let r1 = (time - v) / 2.
    let r2 = (time + v) / 2.
    let first = r1 |> ceil
    let offset1 = if r1 = first then 1. else 0.
    let last = r2 |> floor 
    let offset2 = if r2 = last then - 1. else 0.
    last - first + 1. + offset1 + offset2 |> int

let readLines = 
    File.ReadAllLines >> Array.filter ((<>) String.Empty) >> Array.toList

let run fileName = 
    let lines = readLines fileName
    match lines |> List.map parseDoubles with 
    | [times; distances] -> 
        distances 
        |> List.zip times
        |> List.map solve
        |> List.reduce (*)
        |> printfn "%d"
    | _ -> failwith "Wrong"
    match lines |> List.map parseDouble with 
    | [time; distance] -> 
        solve (time, distance) |> printfn "%d"
    | _ -> failwith "Wrong"

"input" |> run 
