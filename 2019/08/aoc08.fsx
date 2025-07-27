// Advent of Code 2019. Day 08: Space Image Format.
// dotnet fsi aoc08.fsx

open System
open System.IO

let countDigit d = Array.filter ((=) d) >> Array.length

let selectPixels (layers : int array array) : int array = 
    let selectPixel index : int = 
        layers |> Array.map (fun l -> l[index]) |> Array.find (fun n -> n < 2)
    let columnCount = layers[0].Length
    [|0 .. columnCount - 1|] |> Array.map selectPixel

let run (width, height) fileName = 
    let text = File.ReadAllText(fileName).Trim()
    let layers = text |> Seq.toArray |> Array.map (fun c -> int (c.ToString())) |> Array.chunkBySize (width*height) 
    let sorted = layers |> Array.sortBy (countDigit 0)
    let head = sorted[0]
    countDigit 1 head * countDigit 2 head |> printfn "%d"
    let pixels = layers |> selectPixels
    let chunked = pixels |> Array.chunkBySize width
    let foo = chunked |> Array.map (fun row -> row |> Array.map (fun pixel -> if pixel = 1 then '#' else ' ') |> fun ps -> new string(ps)) |> String.concat "\n"
    printfn "%s" foo

    0

// run (3, 2) "sample.txt"
run (25, 6) "input.txt"
