// Advent of Code 2024. Day 19: Linen Layout.
// dotnet fsi aoc19.fsx

open System
open System.IO
open System.Collections.Concurrent

let trim (input : string) = input.Trim()

let split (splitter : string) (input : string) = input.Split(splitter)

let memoize f =
    let cache = ConcurrentDictionary()
    let rec rf x =
        cache.GetOrAdd(x, lazy f rf x).Value
    rf

let createChecker (towels : string list) =
    fun recursive pattern -> 
        if String.length pattern = 0 then true 
        else 
            let candidates = towels |> List.filter (fun t -> pattern.StartsWith(t))
            candidates |> List.exists (fun c -> recursive (pattern.Substring(String.length c)))

let run fileName =
    let text = File.ReadAllText fileName |> trim |> split "\n\n"
    let towels = text.[0] |> split ", " |> Array.toList |> List.sort
    let patterns = text.[1] |> split "\n" |> Array.toList
    let checkPattern = memoize (createChecker towels)
    let possible = patterns |> List.filter checkPattern |> List.length
    printfn "%d" possible

run "input"