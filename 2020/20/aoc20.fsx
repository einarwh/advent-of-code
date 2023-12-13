// Advent of Code 2020. Day 20: Jurassic Jigsaw
// dotnet fsi aoc20.fsx

open System
open System.IO

module String = 

    let rev = Seq.toList >> List.rev >> List.toArray >> String

let readChunks fileName = 
    let text = File.ReadAllText fileName 
    text.TrimEnd().Split("\n\n") |> Array.toList 

let parseTileNumber (s : string) = 
    let ss = s.Substring(0, s.Length - 1).Split(" ")
    int <| ss[1]

let parseChunk (chunk : string) = 
    match chunk.Split("\n") |> Array.toList with 
    | h :: rows ->
        let top = List.head rows 
        let bot = List.last rows 
        let lastIndex = String.length top - 1
        let getColumn ix =  List.map (fun (r : string) -> r[ix]) >> List.toArray >> String
        let left = rows |> getColumn 0 
        let right = rows |> getColumn lastIndex
        let lst = [top; left; bot; right]
        let mirrored = lst |> List.map String.rev 
        let combined = lst @ mirrored 
        (parseTileNumber h, combined)
    | _ -> failwith "oof"

let isUnique (lookup : Map<string, int>) side =
    1 = Map.find side lookup

let isCornerTile lookup (_, sides) = 
    let uniqueSides = (sides |> List.filter (isUnique lookup) |> List.length)
    4 = uniqueSides

let rotateCcw lines = 
    let len = lines |> List.head |> String.length
    [0 .. len - 1]
    |> List.map (fun i -> lines |> List.map (fun r -> r[len - 1 - i]) |> List.toArray |> String)

let flipHorizontal lines = 
    lines |> List.rev 

let flipVertical lines = 
    lines |> List.map String.rev 

let run fileName =
    let chunks = readChunks fileName
    let chunk0 = chunks |> List.head 
    let chunk0List = chunk0.Split("\n") |> Array.toList
    printfn "original:"
    chunk0List |> List.iter (printfn "%A")
    printfn "rotate ccw:"
    chunk0List |> rotateCcw |> List.iter (printfn "%A")
    printfn "flip horizontal:"
    chunk0List |> flipHorizontal |> List.iter (printfn "%A")
    printfn "flip vertical:"
    chunk0List |> flipVertical |> List.iter (printfn "%A")

    let tiles = chunks |> List.map parseChunk
    let lookup = tiles |> List.collect snd |> List.countBy id |> Map.ofList
    let cornerTilesNumbers = 
        tiles |> List.filter (isCornerTile lookup) |> List.map (fst >> int64)
    cornerTilesNumbers |> List.reduce (*) |> printfn "%d"

"sample" |> run
