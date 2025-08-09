// Advent of Code 2021. Day 13: Transparent Origami.
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
    let xs = positions |> List.map fst 
    let ys = positions |> List.map snd
    let xMin = xs |> List.min 
    let xMax = xs |> List.max 
    let yMin = ys |> List.min 
    let yMax = ys |> List.max 
    printfn "%A - %A" (xMin, yMin) (xMax, yMax)
    match transformations with 
    | [] -> positions
    | t :: rest -> 
        positions 
        |> List.map t
        |> List.distinct
        |> applyTransformations rest 

let createRow (posSet : Set<int*int>) (xRange : int list) (y : int) = 
    xRange |> List.map (fun x -> if Set.contains (x, y) posSet then "#" else ".") |> String.concat ""

let draw positions = 
    let posSet = positions |> Set.ofList 
    let xs = positions |> List.map fst 
    let ys = positions |> List.map snd
    let xMin = xs |> List.min 
    let xMax = xs |> List.max 
    let yMin = ys |> List.min 
    let yMax = ys |> List.max 
    let yRange = [yMin .. yMax]
    let xRange = [xMin .. xMax]
    yRange |> List.map (createRow posSet xRange) |> List.iter (printfn "%s")

let run fileName = 
    let text = File.ReadAllText fileName |> trim |> split "\n\n"
    let positions = 
        text.[0] |> split "\n" |> Array.toList |> List.map parsePosition
    let transformations = 
        text.[1] |> split "\n" |> Array.toList |> List.map parseTransformation
    let t = List.head transformations
    positions |> List.map t |> List.distinct |> List.length |> printfn "%d"
    positions |> applyTransformations transformations |> draw

run "input.txt"

// ###..#..#..##..#....###...##..###...##.
// #..#.#..#.#..#.#....#..#.#..#.#..#.#..#
// #..#.####.#..#.#....#..#.#....#..#.#..#
// ###..#..#.####.#....###..#....###..####
// #.#..#..#.#..#.#....#.#..#..#.#.#..#..#
// #..#.#..#.#..#.####.#..#..##..#..#.#..#

// RHALRCRA
