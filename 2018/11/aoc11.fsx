// Advent of Code 2018. Day 11: Chronal Charge.
// dotnet fsi aoc11.fsx

open System
open System.IO

let getHundredsDigit n = 
    (n / 100) % 10

let getPowerLevel serialNumber (x, y) = 
    let rackId = x + 10
    let d = getHundredsDigit <| (rackId * y + serialNumber) * rackId 
    d - 5

let getTotalPower serialNumber (x, y) = 
    [ for x in [x..x+2] do for y in [y..y+2] do yield (x, y) ]
    |> List.map (getPowerLevel serialNumber)
    |> List.sum 

let findFuelCell serialNumber = 
    let range = [0..300-2]
    [ for x in range do for y in range do yield (x, y) ]
    |> List.map (fun pos -> (pos, getTotalPower serialNumber pos))
    |> List.sortByDescending snd 
    |> List.map fst 
    |> List.head

let run fileName = 
    let serialNumber = File.ReadAllText(fileName).Trim() |> int 
    let (x, y) = findFuelCell serialNumber
    printfn "%d,%d" x y

run "input.txt"
