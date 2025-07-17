// Advent of Code 2015. Day 05
// dotnet fsi aoc05.fsx

open System
open System.IO

let readLines = 
    File.ReadAllLines
    >> Array.filter (fun line -> line <> String.Empty)
    >> Array.toList

let containsThreeVowels (s : string) = 
    let vowels = "aeiou" |> Set.ofSeq
    let count = s |> Seq.filter (fun ch -> vowels |> Set.contains ch) |> Seq.length
    count >= 3

let containsLetterPair (s : string) = 
    s |> Seq.pairwise |> Seq.exists (fun (a, b) -> a = b)

let containsDisallowedSubstring (s : string) = 
    let disallowed = [|"ab"; "cd"; "pq"; "xy"|] 
    disallowed |> Array.exists (fun d -> s.Contains d)

let doesNotContainDisallowedSubstring = not << containsDisallowedSubstring

let isNiceString (s : string) = 
    containsThreeVowels s && containsLetterPair s && doesNotContainDisallowedSubstring s

let run fileName = 
    let lines = readLines fileName
    lines |> List.filter isNiceString |> List.length |> printfn "%d"

run "input"
