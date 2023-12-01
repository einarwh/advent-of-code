// Advent of Code 2023. Day 1: Calorie Counting.
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

let parseLine1 (line : string) : int =
    line |> Seq.choose tryParseInt |> toNum

let rec tokenize (line : string) : int list = 
    if line.Length = 0 then 
        []
    else
        let ch = line[0]
        if Char.IsDigit ch then 
            let n = Char.GetNumericValue ch |> int
            n :: tokenize (line.Substring(1))
        elif line.StartsWith("one") then 
            1 :: tokenize (line.Substring(3))
        elif line.StartsWith("two") then 
            2 :: tokenize (line.Substring(3))
        elif line.StartsWith("three") then 
            3 :: tokenize (line.Substring(5))
        elif line.StartsWith("four") then 
            4 :: tokenize (line.Substring(4))
        elif line.StartsWith("five") then 
            5 :: tokenize (line.Substring(4))
        elif line.StartsWith("six") then 
            6 :: tokenize (line.Substring(3))
        elif line.StartsWith("seven") then 
            7 :: tokenize (line.Substring(5))
        elif line.StartsWith("eight") then 
            8 :: tokenize (line.Substring(5))
        elif line.StartsWith("nine") then 
            9 :: tokenize (line.Substring(4))
        else 
            tokenize (line.Substring(1))

let parseLine2 (line : string) : int =
    line |> tokenize |> toNum

let part1 filename = 
    filename 
    |> File.ReadAllLines
    |> Array.toList
    |> List.filter (fun line -> line <> String.Empty)
    |> List.map parseLine1
    |> List.sum

let part2 filename = 
    filename 
    |> File.ReadAllLines
    |> Array.toList
    |> List.filter (fun line -> line <> String.Empty)
    |> List.map parseLine2
    |> List.sum

let run filename = 
    //part1 filename |> printfn "%d"
    part2 filename |> printfn "%A"

run "sample2"
