// Advent of Code 2015. Day 17: No Such Thing as Too Much.
// dotnet fsi aoc17.fsx

open System
open System.IO

let parse (s : string) = int s

let findPermutations (amount : int) (nums : int list) = 
    let rec loop (numbers : int list) = 
        match numbers with 
        | [] -> [[]]
        | n :: rest -> 
            let subPermutations = loop rest 
            let withNumber = 
                subPermutations 
                |> List.map (fun perm -> n :: perm)
                |> List.filter (fun perm -> List.sum perm <= amount) 
            let combined = withNumber @ subPermutations
            combined
    loop nums |> List.filter (fun perm -> List.sum perm = amount) 

let readLines = 
    File.ReadAllLines
    >> Array.filter (fun line -> line <> String.Empty)
    >> Array.toList

let run amount fileName = 
    let lines = readLines fileName
    let permutations = lines |> List.map parse |> findPermutations amount 
    permutations |> List.length |> printfn "%d"
    let ways = 
        permutations 
        |> List.groupBy (fun p -> List.length p) 
        |> List.map (fun (count, lst) -> (count, List.length lst)) 
        |> List.sort
        |> List.head
        |> snd 
    ways |> printfn "%d"

// run 25 "sample.txt"
run 150 "input.txt"
