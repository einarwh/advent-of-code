// Advent of Code 2025. Day 10: Factory.
// dotnet fsi aoc10.fsx

open System
open System.IO

type Machine =
    { lights : bool array
      buttons : bool array array
      joltage : int array }

let middleString (s : string) = s[1 .. s.Length - 2]

let middleArray (arr : 'a array) = arr[1 .. arr.Length - 2]
      
let parseLights (s : string) = 
    let booleans = middleString s |> Seq.toArray |> Array.map ((=) '#')
    booleans 
    
let parseNumbers (s : string) = 
    s.Split "," |> Array.map int

let parseButton numLights (s : string) = 
    let numbers = middleString s |> parseNumbers
    [|0 .. numLights - 1|] |> Array.map (fun i -> Array.contains i numbers)

let parseJoltage (s : string) = 
    let numbers = middleString s |> parseNumbers
    numbers 

let parseMachine (s : string) = 
    let parts = s.Split " "
    let lights = parts |> Array.head |> parseLights
    let numLights = Array.length lights 
    let joltage = parts |> Array.last |> parseJoltage
    let buttons = middleArray parts |> Array.map (parseButton numLights)
    { lights = lights; buttons = buttons; joltage = joltage }
    
let readLines = 
    File.ReadAllLines
    >> Array.filter (fun line -> line <> String.Empty)
    >> Array.toList

let run fileName = 
    let lines = readLines fileName
    let machines = lines |> List.map parseMachine
    machines |> printfn "%A"

run "sample.txt"
