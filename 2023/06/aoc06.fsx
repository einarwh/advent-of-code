// Advent of Code 2023. Day 6: Wait For It
// dotnet fsi aoc06.fsx

open System
open System.IO
open System.Text.RegularExpressions

let parseNumbers (s : string) : int list =
    Regex.Matches(s, "\d+") 
    |> Seq.map (fun m -> int m.Value)
    |> Seq.toList

let strategies (duration : int) : int list = 
    [0 .. duration] |> List.map (fun i -> i * (duration - i))

let winners (record, attempts) = 
    attempts |> List.filter (fun a -> a > record) |> List.length

let readLines = 
    File.ReadAllLines
    >> Array.filter (fun line -> line <> String.Empty)

let run fileName = 
    let lines = readLines fileName |> Array.toList
    match lines |> List.map parseNumbers with 
    | [durations; distances] -> 
        durations 
        |> List.map strategies 
        |> List.zip distances
        |> List.map winners
        |> List.reduce (*)
        |> printfn "%d"
    | _ -> failwith "Wrong"

"input" |> run 
