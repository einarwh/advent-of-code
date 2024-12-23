// Advent of Code 2021. Day 13
// dotnet fsi aoc13.fsx

open System
open System.IO

let trim (input : string) = input.Trim()

let split (splitter : string) (input : string) = input.Split(splitter)

let parsePosition s = 
    match s |> split "," with 
    | [|a;b|] -> (int a, int b)
    | _ -> failwith "?"

let parseTransformation s =
    match s |> split " " |> Array.item 2 |> split "=" with 
    | [|"x";str|] -> 
        let n = int str  
        fun (x, y) -> (2 * n - x, y)
    | [|"y";str|] -> 
        let n = int str  
        fun (x, y) -> (x, 2 * n - y) 
    | _ -> failwith "?"

let run fileName = 
    let text = File.ReadAllText fileName |> trim |> split "\n\n"
    let positions = 
        text.[0] |> split "\n" |> Array.toList |> List.map parsePosition
    positions |> printfn "%A"
    let transformations = 
        text.[1] |> split "\n" |> Array.toList |> List.map parseTransformation
    let x = 655
    2 * x - 1305 |> printfn "%A"
    0

run "input"
