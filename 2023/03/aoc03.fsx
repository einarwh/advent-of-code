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

let parseNumbers (line : string) = 
    Regex.Matches(line, "\d+") 
    |> Seq.map (fun m -> (m.Index, m.Value))
    |> Seq.toList

let parseSymbols (line : string) = 
    Regex.Matches(line, "[^\d\.]") 
    |> Seq.map (fun m -> (m.Index, m.Value))
    |> Seq.toList

let parseSymbolsAtLine (i : int) (line : string) = 
    line
    |> parseSymbols
    |> List.map (fun (x, s) -> (toSymbolBox (x, i), s))

let parseNumbersAtLine (i : int) (line : string) = 
    line
    |> parseNumbers
    |> List.map (fun (x, s) -> (toNumberBox ((x, i), s), s))

let overlapping (box1 : Box) (box2 : Box) : bool = 
    box1.xMax >= box2.xMin && box2.xMax >= box1.xMin &&  
    box1.yMax >= box2.yMin && box2.yMax >= box1.yMin 

let readLines = 
    File.ReadAllLines
    >> Array.filter (fun line -> line <> String.Empty)

let checkNumber (symbolBoxes : Box list) (numberBox, _) : bool = 
    symbolBoxes |> List.exists (overlapping numberBox)

let gearRatio (numBoxList : (Box * string) list) (symbolBox, _) : int64 = 
    let parts = 
        numBoxList |> List.filter (fun (box, num) -> overlapping box symbolBox)
    match parts with 
    | [(_, str1); (_, str2)] -> (int64 str1) * (int64 str2) 
    | _ -> 0 

let run fileName = 
    let lines = readLines fileName
    let symbolsAndBoxes = 
        lines 
        |> Array.toList
        |> List.mapi parseSymbolsAtLine
        |> List.collect id
    let symbolBoxes = 
        symbolsAndBoxes |> List.map fst
    let numbersAndBoxes = 
        lines 
        |> Array.toList
        |> List.mapi parseNumbersAtLine 
        |> List.collect id
    let partNumbers = 
        numbersAndBoxes
        |> List.filter (checkNumber symbolBoxes)
        |> List.map (snd >> int)    
    partNumbers 
    |> List.sum 
    |> printfn "%d" 
    symbolsAndBoxes 
    |> List.map (gearRatio numbersAndBoxes)
    |> List.sum
    |> printfn "%d"

"input" |> run 
