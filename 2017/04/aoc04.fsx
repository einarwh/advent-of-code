// Advent of Code 2017. Day 04: High-Entropy Passphrases.
// dotnet fsi aoc04.fsx

open System
open System.IO

let rec dup = function 
  | [] -> false
  | h::t -> List.contains h t || dup t
  
let count = List.filter (dup >> not) >> List.length 

let sorted : string -> string = 
  Seq.sort >> fun cs -> new string(Seq.toArray cs)
  
let count2 = List.filter (List.map sorted >> dup >> not) >> List.length 

let readLine (line : string) = 
  line.Split() |> List.ofArray

let readLines = 
    File.ReadAllLines
    >> Array.filter (fun line -> line <> String.Empty)
    >> Array.toList

let run fileName = 
    let lines = readLines fileName
    let phrases = lines |> List.map readLine
    phrases |> count |> printfn "%A"
    phrases |> count2 |> printfn "%A"
    
run "input.txt"
