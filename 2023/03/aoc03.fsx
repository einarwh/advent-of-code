// Advent of Code 2023. Day 3: Gear Ratios
// dotnet fsi aoc03.fsx

open System
open System.IO
open System.Text.RegularExpressions

type Box = {
    xMin : int
    yMin : int
    xMax : int
    yMax : int
}

let toSymbolBox (x, y) = 
    { xMin = x-1
      yMin = y-1
      xMax = x+1
      yMax = y+1 }

let toNumberBox ((x, y), s : string) = 
    { xMin = x
      yMin = y
      xMax = x+s.Length-1
      yMax = y }

let parse pattern line = 
    Regex.Matches(line, pattern) 
    |> Seq.map (fun m -> (m.Index, m.Value))
    |> Seq.toList

let parseSymbolsAtLine (y : int) (line : string) = 
    line
    |> parse "[^\d\.]"
    |> List.map (fun (x, s) -> (toSymbolBox (x, y), s))

let parseNumbersAtLine (y : int) (line : string) = 
    line
    |> parse "\d+"
    |> List.map (fun (x, s) -> (toNumberBox ((x, y), s), s))

let overlapping (box1 : Box) (box2 : Box) : bool = 
    box1.xMax >= box2.xMin && box2.xMax >= box1.xMin &&  
    box1.yMax >= box2.yMin && box2.yMax >= box1.yMin 

let checkNumber (symbolBoxes : Box list) (numberBox, _) : bool = 
    symbolBoxes |> List.exists (overlapping numberBox)

let gearRatio (numBoxList : (Box * string) list) (symbolBox, _) : int = 
    let parts = 
        numBoxList |> List.filter (fun (box, _) -> overlapping box symbolBox)
    match parts with 
    | [(_, str1); (_, str2)] -> (int str1) * (int str2) 
    | _ -> 0 

let readLines = 
    File.ReadAllLines
    >> Array.filter (fun line -> line <> String.Empty)

let run fileName = 
    let lines = readLines fileName |> Array.toList
    let parseWith parser = List.mapi parser >> List.collect id 
    let symbolsAndBoxes = lines |> parseWith parseSymbolsAtLine
    let symbolBoxes = symbolsAndBoxes |> List.map fst
    let numbersAndBoxes = lines |> parseWith parseNumbersAtLine
    numbersAndBoxes
    |> List.filter (checkNumber symbolBoxes)
    |> List.map (snd >> int)
    |> List.sum 
    |> printfn "%d" 
    symbolsAndBoxes 
    |> List.map (gearRatio numbersAndBoxes)
    |> List.sum
    |> printfn "%d"

"input" |> run 
