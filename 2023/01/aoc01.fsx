// Advent of Code 2023. Day 1: Trebuchet?!
// dotnet fsi aoc01.fsx

open System
open System.IO

let tryParseInt (ch : char) : int option = 
    if Char.IsDigit ch then 
        Some (Char.GetNumericValue ch |> int)
    else 
        None

let toNum (nums : int seq) : int = 
    let tens = nums |> Seq.head
    let ones = nums |> Seq.rev |> Seq.head 
    tens * 10 + ones

let numberFinder1 = Seq.choose tryParseInt >> Seq.toList

let tryFindNum (line : string) : int option = 
    let ch = line[0]
    if Char.IsDigit ch then 
        Some (Char.GetNumericValue ch |> int)
    elif line.StartsWith("one") then 
        Some 1
    elif line.StartsWith("two") then 
        Some 2
    elif line.StartsWith("three") then 
        Some 3
    elif line.StartsWith("four") then 
        Some 4
    elif line.StartsWith("five") then 
        Some 5
    elif line.StartsWith("six") then 
        Some 6
    elif line.StartsWith("seven") then 
        Some 7
    elif line.StartsWith("eight") then 
        Some 8
    elif line.StartsWith("nine") then 
        Some 9
    else 
        None

let rec subStrings (str : string) = 
    if str = "" then []
    else str :: subStrings (str.Substring(1))

let numberFinder2 = subStrings >> List.choose tryFindNum

let calculate numberFinder = 
    List.map (numberFinder >> toNum) >> List.sum

let readLines = 
    File.ReadAllLines
    >> Array.toList
    >> List.filter (fun line -> line <> String.Empty)

let run fileName = 
    let lines = readLines fileName
    lines |> calculate numberFinder1 |> printfn "%d"
    lines |> calculate numberFinder2 |> printfn "%d"

run "input"
