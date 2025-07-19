// Advent of Code 2016. Day 19: An Elephant Named Joseph.
// See: https://en.wikipedia.org/wiki/Josephus_problem
// dotnet fsi aoc19.fsx

open System
open System.IO
open System.Numerics 

let highestOneBit x = 
    if x = 0u then 0u else 1u <<< BitOperations.Log2(x)

let getSafePosition (n : uint) = 
    ~~~(highestOneBit (n*2u)) &&& ((n <<< 1) ||| 1u)

let run fileName = 
    let text = File.ReadAllText(fileName).Trim()
    text |> uint |> getSafePosition |> printfn "%d"

run "input.txt"
