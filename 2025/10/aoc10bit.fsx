// Advent of Code 2025. Day 10: Factory.
// dotnet fsi aoc10bit.fsx

open System
open System.IO

type Machine =
    { lights : int
      buttons : int array }

let middleString (s : string) = s[1 .. s.Length - 2]

let middleArray (arr : 'a array) = arr[1 .. arr.Length - 2]

let bit pos = 1 <<< pos

let parseLights (s : string) : int = 
    middleString s 
    |> Seq.toArray 
    |> Array.indexed
    |> Array.choose (fun (i, ch)-> if ch = '#' then Some (bit i) else None) 
    |> Array.reduce (^^^)

let parseNumbers (s : string) = 
    s.Split "," |> Array.map int

let parseButton (s : string) = 
    middleString s |> parseNumbers |> Array.map bit |> Array.reduce (|||)

let parseMachine (s : string) = 
    let parts = s.Split " "
    let lights = parts |> Array.head |> parseLights
    let buttons = middleArray parts |> Array.map parseButton
    { lights = lights; buttons = buttons }
    
let readLines = 
    File.ReadAllLines
    >> Array.filter (fun line -> line <> String.Empty)
    >> Array.toList

let pushAll (buttons : int array) (pattern : int) : int array = 
    buttons |> Array.map ((^^^) pattern)

let solve (m : Machine) = 
    let lights = m.lights 
    let buttons = m.buttons
    let rec loop (pushCount : int) (seen : Set<int>) (patterns : Set<int>) = 
        if Set.contains lights patterns then pushCount
        else
            let patterns' = patterns |> Set.toArray |> Array.collect (pushAll buttons) |> Set.ofArray
            printfn "patterns' %d" patterns'.Count
            let shrunk = Set.difference patterns' seen
            printfn "shrunk %d" shrunk.Count
            let seen' = Set.union seen patterns 
            printfn "seen' %d" seen'.Count
            loop (pushCount + 1) seen' patterns'
    let patterns = Set.empty |> Set.add 0
    loop 0 Set.empty patterns 

let run fileName = 
    let lines = readLines fileName
    let machines = lines |> List.map parseMachine
    machines |> List.sumBy solve |> printfn "%d"

run "input.txt"
