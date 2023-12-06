// Advent of Code 2023. Day 6: Wait For It
// dotnet fsi aoc06.fsx

open System
open System.IO
open System.Text.RegularExpressions

let parse (s : string) : string list =
    Regex.Matches(s, "\d+") 
    |> Seq.map (fun m -> m.Value)
    |> Seq.toList

let parseDoubles (s : string) = 
    s |> parse |> List.map double

let parseDouble (s : string) = 
    s |> parse |> String.concat "" |> double

let solve (duration : double, record : double) = 
    let b = duration 
    let c = record 
    let v = Math.Sqrt(b * b - 4. * c)
    let r1 = (b - v) / 2.
    let r2 = (b + v) / 2.
    let firstRounded = r1 |> Math.Ceiling
    let first = if r1 = firstRounded then int r1 + 1 else int firstRounded
    let lastRounded = r2 |> Math.Floor 
    let last = if r2 = lastRounded then int r2 - 1 else int lastRounded
    last - first + 1

let readLines = 
    File.ReadAllLines
    >> Array.filter (fun line -> line <> String.Empty)

let run fileName = 
    let lines = readLines fileName |> Array.toList
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
        solve (time, distance) |> printfn "%A"
    | _ -> failwith "Wrong"

"input" |> run 
