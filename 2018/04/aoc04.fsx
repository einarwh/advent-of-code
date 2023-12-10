// Advent of Code 2018. Day 4: Repose Record
// dotnet fsi aoc02.fsx

open System
open System.IO
open System.Text.RegularExpressions

type Shift = {
    Guard : int 
    Total : int 
    Minutes : int list
}

let readLines = 
    File.ReadAllLines >> Array.filter ((<>) String.Empty)

let chunk (lines : string list) = 
    let folder (s : string) (current, total) = 
        let updated = s :: current 
        if s.Contains("Guard") then ([], updated :: total)
        else (updated, total)
    let (_, total) = List.foldBack folder lines ([], [])
    total

let parseMinute (s : string) = 
    let m = Regex.Match(s, "00:(\d\d)")
    int m.Groups[1].Value 

let toShift (lines : string list) : Shift = 
    match lines with 
    | h :: observations ->
        let s = h.Split("#")[1]
        let guard = int <| s.Split(" ")[0]
        let ranges = 
            observations 
            |> List.pairwise 
            |> List.filter (fun (a, _) -> a.EndsWith "falls asleep")
            |> List.map (fun (a, b) -> (parseMinute a, parseMinute b - 1))
        let minutes = 
            ranges 
            |> List.collect (fun (a, b) -> [ a .. b ])
        let total = 
            ranges 
            |> List.map (fun (a, b) -> b - a + 1)
            |> List.sum 
        { Guard = guard; Total = total; Minutes = minutes }
    | _ -> failwith "oof"

let combine (guardNumber, shifts) = 
    let total = shifts |> List.sumBy (fun g -> g.Total)
    let minutes = shifts |> List.collect (fun g -> g.Minutes)
    {
        Guard = guardNumber
        Total = total
        Minutes = minutes
    }

let combineAll (shifts : Shift list) = 
    shifts 
    |> List.groupBy (fun shift -> shift.Guard)
    |> List.map combine

let part1 shifts = 
    let sleepy = shifts |> List.maxBy (fun g -> g.Total)
    let guardNumber = sleepy.Guard
    let minute = 
        sleepy.Minutes 
        |> List.countBy id 
        |> List.maxBy snd
        |> fst
    guardNumber * minute 

let part2 shifts = 
    shifts 
    |> List.filter (fun g -> g.Total > 0)
    |> List.map (fun g -> g.Guard, g.Minutes |> List.countBy id |> List.maxBy snd |> fst)
    |> List.sortByDescending (fun (gn, minute) -> minute)
    |> List.iter (fun sh -> printfn "%A" sh)
    let sleepy = 
        shifts 
        |> List.filter (fun g -> g.Total > 0)
        |> List.maxBy (fun g -> g.Minutes |> List.countBy id |> List.maxBy snd)
    let guardNumber = sleepy.Guard
    // printfn "sleepy 2 %A" sleepy
    let minute = 
        sleepy.Minutes 
        |> List.countBy id 
        |> List.maxBy snd
        |> fst
    guardNumber * minute 

let run fileName = 
    let lines = readLines fileName |> Array.toList |> List.sort
    let chunked = chunk lines
    let shifts = 
        chunked 
        |> List.map toShift 
        // |> List.filter (fun g -> g.Total > 0)
    // shifts |> List.iter (fun sh -> printfn "%A" sh)
    let combined = shifts |> combineAll
    combined |> part1 |> printfn "%d"
    combined |> part2 |> printfn "%A"

"sample" |> run 
