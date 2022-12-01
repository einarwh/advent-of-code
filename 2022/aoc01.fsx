// Advent of Code 2022. Day 1.
// dotnet fsi aoc01.fsx

open System.IO

let trim (input : string) = input.Trim()

let split (splitter : string) (input : string) = input.Split(splitter)

let toCalories =
    split "\n" >> Array.map int >> Array.sum

let toCaloriesArray = 
    File.ReadAllText
    >> trim 
    >> split "\n\n"
    >> Array.map toCalories 
    >> Array.sortDescending

let run input = 
    let calories = input |> toCaloriesArray
    calories |> Array.item 0 |> printfn "%d"
    calories |> Array.take 3 |> Array.sum |> printfn "%d"

run "input"
