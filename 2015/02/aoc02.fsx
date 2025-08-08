// Advent of Code 2015. Day 02: I Was Told There Would Be No Math.
// dotnet fsi aoc02.fsx

open System
open System.IO

let parse (s : string) = 
    match s.Split("x") |> Array.map int |> Array.sort with 
    | [|a; b; c|] -> Some (a, b, c)
    | _ -> None


let paper (a, b, c) = 
    3 * a * b + 2 * a * c + 2 * b * c

let ribbon (a, b, c) = 
    a + a + b + b + a * b * c

let readLines = 
    File.ReadAllLines
    >> Array.filter (fun line -> line <> String.Empty)
    >> Array.toList

let run fileName = 
    let lines = readLines fileName
    let boxes = lines |> List.choose parse
    boxes |> List.map paper |> List.sum |> printfn "%d"
    boxes |> List.map ribbon |> List.sum |> printfn "%d"

run "input.txt"
