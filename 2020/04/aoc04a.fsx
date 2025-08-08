// Advent of Code 2020. Day 4, Part A.
// dotnet fsi aoc04a.fsx

open System.IO

let readKeys (chunk : string) : string list =
    chunk.Split()
    |> Array.map (fun s -> s.Split(":"))
    |> Array.map (fun a -> a.[0])
    |> Array.toList
    
let check (keys : string list) =
    [ "byr"; "iyr"; "eyr"; "hgt"; "hcl"; "ecl"; "pid" ]
    |> List.map (fun req -> (List.contains req keys))
    |> List.reduce (&&)

let run (text : string) = 
    text.Split("\n\n")
    |> Array.map readKeys
    |> Array.filter check
    |> Array.length
    |> printfn "%d"

"input.txt" |> File.ReadAllText |> run 
