// Advent of Code 2017. Day 02: Corruption Checksum.
// dotnet fsi aoc02.fsx

open System
open System.IO

let checksum = List.map (fun row -> List.max row - List.min row) >> List.reduce (+) 

let tryFindDiv x = 
  List.tryFind (fun d -> x > d && x % d = 0) >> Option.map (fun d -> x / d)

let division lst = 
  lst |> List.choose (fun x -> tryFindDiv x lst) |> List.head
     
let readLine (line : string) = 
  line.Split() |> List.ofArray |> List.map int

let readSheet = 
  Seq.map readLine >> Seq.toList

let readLines = 
    File.ReadAllLines
    >> Array.filter (fun line -> line <> String.Empty)
    >> Array.toList

let run fileName = 
    let lines = readLines fileName
    let sheet = readSheet lines
    sheet |> checksum |> printfn "%d"
    sheet |> List.map division |> List.sum |> printfn "%d"

run "input.txt"
