// Advent of Code 2021. Day 21: Dirac Dice.
// dotnet fsi aoc21.fsx

open System
open System.IO

let readLines = 
    File.ReadAllLines
    >> Array.filter (fun line -> line <> String.Empty)
    >> Array.toList

let rec play rolls (pos1, score1) (pos2, score2) = 
    // printfn "%A %A" (pos1, score1) (pos2, score2)
    if (score2 >= 1000) then rolls * score1
    else 
        let x = (pos1 + (rolls + 1) + (rolls + 2) + (rolls + 3)) % 10 
        let pos = if x = 0 then 10 else x 
        play (rolls + 3) (pos2, score2) (pos, score1 + pos)

let run fileName = 
    let lines = readLines fileName
    let startPositions = lines |> List.map (fun s -> s[s.Length - 1].ToString() |> int)
    let pos1 = startPositions[0]
    let pos2 = startPositions[1]
    play 0 (pos1, 0) (pos2, 0) |> printfn "%A"

run "input.txt"
