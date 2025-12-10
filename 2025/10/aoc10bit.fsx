// Advent of Code 2025. Day 10: Factory.
// dotnet fsi aoc10bit.fsx

open System
open System.IO

let middleString (s : string) = s[1 .. s.Length - 2]

let middleArray (arr : 'a array) = arr[1 .. arr.Length - 2]

let bit pos = 1 <<< pos

let parseLights (s : string) = 
    middleString s 
    |> Seq.toArray 
    |> Array.indexed
    |> Array.choose (fun (i, ch)-> if ch = '#' then Some (bit i) else None) 
    |> Array.reduce (^^^)

let parseNumbers (s : string) = 
    s.Split "," |> Array.map int

let parseButton (s : string) = 
    middleString s |> parseNumbers |> Array.map bit |> Array.reduce (|||)

let parse (s : string) = 
    let parts = s.Split " "
    let lights = parts |> Array.head |> parseLights
    let buttons = middleArray parts |> Array.map parseButton
    lights, buttons
    
let readLines = 
    File.ReadAllLines
    >> Array.filter (fun line -> line <> String.Empty)
    >> Array.toList

let pushAll (buttons : int array) (pattern : int) = 
    buttons |> Array.map ((^^^) pattern)

let solve (lights, buttons) = 
    let rec loop pushCount patterns = 
        if Set.contains lights patterns then pushCount
        else
            patterns 
            |> Set.toArray |> Array.collect (pushAll buttons) |> Set.ofArray 
            |> loop (pushCount + 1) 
    Set.empty |> Set.add 0 |> loop 0  

let run fileName = 
    fileName |> readLines |> List.map parse |> List.sumBy solve |> printfn "%d"

run "input.txt"
