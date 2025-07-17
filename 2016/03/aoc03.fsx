// Advent of Code 2016. Day 03
// dotnet fsi aoc03.fsx

open System
open System.IO

let parse (s : string) : int*int*int = 
    let numbers = 
        s.Split(" ", StringSplitOptions.RemoveEmptyEntries) 
        |> Array.toList
        |> List.map int
    match numbers with 
    | [a; b; c] -> (a, b, c)
    | _ -> failwith "?"

let substring (startIndex : int) (length : int) (s : string) : string = 
    s.Substring(startIndex, length)

let readLines = 
    File.ReadAllLines
    >> Array.filter (fun line -> line <> String.Empty)
    >> Array.toList

let isPossible (a, b, c) = 
    a + b > c && a + c > b && b + c > a

let rec toTriplets numbers = 
    match numbers with 
    | [] -> []
    | a :: b :: c :: rest -> 
        (a, b, c) :: toTriplets rest 
    | _ -> failwith "?"

let run fileName = 
    let lines = readLines fileName
    let rowTriplets = lines |> List.map parse
    let rowPossible = rowTriplets |> List.filter isPossible |> List.length
    printfn "%d" rowPossible
    let column1 = lines |> List.map (substring 0 5)
    let column2 = lines |> List.map (substring 5 5)
    let column3 = lines |> List.map (substring 10 5)
    let colNumbers = (column1 @ column2 @ column3) |> List.map int
    let colTriplets = colNumbers |> toTriplets
    let colPossible = colTriplets |> List.filter isPossible |> List.length
    printfn "%d" colPossible

run "input"
