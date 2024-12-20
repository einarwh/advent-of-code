// Advent of Code 2024. Day 19: Linen Layout.
// dotnet fsi aoc19.fsx

open System
open System.IO
open System.Collections.Concurrent

let trim (input : string) = input.Trim()

let split (splitter : string) (input : string) = input.Split(splitter)

let startsWith (input : string) (prefix : string) = input.StartsWith(prefix)

let memoize f =
    let cache = ConcurrentDictionary()
    let rec recur x =
        cache.GetOrAdd(x, lazy f recur x).Value
    recur

let createChecker towels =
    fun recur pattern -> 
        if String.length pattern = 0 then 1L 
        else 
            let candidates = towels |> List.filter (startsWith pattern)
            candidates |> List.sumBy (fun c -> recur (pattern.Substring(String.length c)))

let run fileName =
    let text = File.ReadAllText fileName |> trim |> split "\n\n"
    let towels = text.[0] |> split ", " |> Array.toList |> List.sort
    let patterns = text.[1] |> split "\n" |> Array.toList
    let checkPattern = memoize (createChecker towels)
    let results = patterns |> List.map checkPattern
    results |> List.filter ((<) 0L) |> List.length |> printfn "%d"
    results |> List.sum |> printfn "%d"

run "input"
