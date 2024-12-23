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
        fun (x, y) -> ((if x > n then 2 * n - x else x), y)
    | [|"y";str|] -> 
        let n = int str  
        fun (x, y) -> (x, (if y > n then 2 * n - y else y))
    | _ -> failwith "?"

let rec applyTransformations transformations positions =
    match transformations with 
    | [] -> positions
    | t :: rest -> 
        positions 
        |> List.map t
        |> List.distinct
        |> applyTransformations rest 

let run fileName = 
    let text = File.ReadAllText fileName |> trim |> split "\n\n"
    let positions = 
        text.[0] |> split "\n" |> Array.toList |> List.map parsePosition
    let transformations = 
        text.[1] |> split "\n" |> Array.toList |> List.map parseTransformation
    let t = List.head transformations
    positions |> List.map t |> List.distinct |> List.length |> printfn "%d"
    0

run "input"
