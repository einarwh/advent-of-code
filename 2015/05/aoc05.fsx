// Advent of Code 2015. Day 05: Doesn't He Have Intern-Elves For This?
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

let containsPairWithoutOverlap (str : string) =
    let rec fn (s : string) = 
        if s.Length < 2 then false 
        else 
            let two = s.Substring(0, 2)
            s.Substring(2).Contains(two) || fn (s.Substring(1))
    fn str

let containsRepeatingLetter (str : string) = 
    let rec fn (chars : char list) = 
        match chars with 
        | a :: b :: c :: rest -> 
            a = c || fn (b :: c :: rest)
        | _ -> false
    fn (str |> Seq.toList)

let isNicerString (s : string) = 
    containsPairWithoutOverlap s && containsRepeatingLetter s

let run fileName = 
    let lines = readLines fileName
    lines |> List.filter isNiceString |> List.length |> printfn "%d"
    lines |> List.filter isNicerString |> List.length |> printfn "%d"

run "input.txt"
