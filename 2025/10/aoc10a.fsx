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

let push (pattern : bool array) (button : bool array) : bool array = 
    pattern |> Array.zip button |> Array.map (fun (a, b) -> a <> b)

let pushAll (buttons : bool array array) (pattern : bool array) : bool array array = 
    buttons |> Array.map (push pattern)

let pushJoltage (pattern : int array) (button : bool array) : int array = 
    pattern |> Array.zip button |> Array.map (fun (b, n) -> n + if b then 1 else 0)

let pushAllJoltage (buttons : bool array array) (pattern : int array) : int array array = 
    buttons |> Array.map (pushJoltage pattern)

let solvePart1 (m : Machine) = 
    let lights = m.lights 
    let buttons = m.buttons
    let rec loop (pushCount : int) (seen : Set<bool array>) (patterns : Set<bool array>) = 
        if Set.contains lights patterns then pushCount
        else
            let patterns' = patterns |> Set.toArray |> Array.collect (pushAll buttons) |> Set.ofArray
            let shrunk = Set.difference patterns' seen
            let seen' = Set.union seen patterns 
            loop (pushCount + 1) seen' shrunk
    let allOff = lights |> Array.map (fun _ -> false)
    let patterns = Set.empty |> Set.add allOff
    loop 0 Set.empty patterns 

let lessOrEqual (joltage : int array) (pattern : int array) = 
    pattern |> Array.zip joltage |> Array.forall (fun (j, v) -> v <= j)

let solvePart2 (m : Machine) = 
    printfn "\nMACHINE"
    let buttons = m.buttons
    let joltage = m.joltage
    printfn "JOLTAGE %A" joltage
    let rec loop (pushCount : int) (seen : Set<int array>) (patterns : Set<int array>) = 
        printfn "loop. seen %A. patterns %A." (seen.Count) (patterns.Count)
        if Set.contains joltage patterns then pushCount
        else
            let patterns' = patterns |> Set.toArray |> Array.collect (pushAllJoltage buttons) |> Set.ofArray
            // patterns' |> Set.iter (printfn "patterns' item %A")
            // printfn "patterns' %A" patterns'
            let shrunk = Set.difference patterns' seen
            // shrunk |> Set.iter (printfn "shunk item %A")
            let filtered = shrunk |> Set.filter (lessOrEqual joltage)
            // filtered |> Set.iter (printfn "filtered %A")
            let seen' = Set.union seen patterns 
            // seen' |> Set.iter (printfn "seen' item %A")
            loop (pushCount + 1) seen' filtered
    let allZero = joltage |> Array.map (fun _ -> 0)
    let patterns = Set.empty |> Set.add allZero
    loop 0 Set.empty patterns 

let run fileName = 
    let lines = readLines fileName
    let machines = lines |> List.map parseMachine
    machines |> List.sumBy solvePart1 |> printfn "%d"
    machines |> List.sumBy solvePart2 |> printfn "%d"

run "input.txt"
