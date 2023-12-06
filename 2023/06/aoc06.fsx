// Advent of Code 2023. Day 6: Wait For It
// dotnet fsi aoc06.fsx

open System
open System.IO
open System.Text.RegularExpressions

let parse (s : string) : string list =
    Regex.Matches(s, "\d+") 
    |> Seq.map (fun m -> m.Value)
    |> Seq.toList

let parseNumbers (s : string) = 
    s |> parse |> List.map int

let parseDouble (s : string) = 
    s |> parse |> String.concat "" |> double

let strategies (duration : int) : int list = 
    [0 .. duration] |> List.map (fun i -> i * (duration - i))

let winners (record, attempts) = 
    attempts |> List.filter (fun a -> a > record) |> List.length

let solve (duration : double) (record : double) = 
    let b = duration 
    let c = record 
    let v = Math.Sqrt(b * b - 4. * c)
    let r1 = (b - v) / 2.
    let r2 = (b + v) / 2.
    let first = r1 |> Math.Ceiling |> int
    let last = r2 |> Math.Floor |> int
    last - first + 1

let readLines = 
    File.ReadAllLines
    >> Array.filter (fun line -> line <> String.Empty)

let run fileName = 
    let lines = readLines fileName |> Array.toList
    match lines |> List.map parseNumbers with 
    | [times; distances] -> 
        times 
        |> List.map strategies 
        |> List.zip distances
        |> List.map winners
        |> List.reduce (*)
        |> printfn "%d"
    | _ -> failwith "Wrong"
    match lines |> List.map parseDouble with 
    | [time; distance] -> 
        solve time distance |> printfn "%A"
    | _ -> failwith "Wrong"

"input" |> run 
