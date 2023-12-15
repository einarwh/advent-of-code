// Advent of Code 2020. Day 20: Jurassic Jigsaw
// dotnet fsi aoc20.fsx

open System
open System.IO

[<AutoOpen>]
module Types =

    type Tile = {
        Number : int 
        Lines : char list list
    }

module Tile = 

    let sideLength (tile : Tile) = tile.Lines |> List.head |> List.length 

    let rotateCcw (tile : Tile) = 
        let rot (lines : char list list) = 
            let lastIndex = (lines |> List.head |> List.length) - 1
            [0 .. lastIndex]
            |> List.map (fun i -> lines |> List.map (fun r -> r[lastIndex - i]))
        { tile with Lines = rot tile.Lines }

    let flipHorizontal (tile : Tile) = 
        { tile with Lines = tile.Lines |> List.rev }

    let flipVertical (tile : Tile) = 
        { tile with Lines = tile.Lines |> List.map (List.rev) }

    let north (tile : Tile) = tile.Lines |> List.head

    let south (tile : Tile) = tile.Lines |> List.last

    let west (tile : Tile) = 
        tile.Lines |> List.map (List.item 0)

    let east (tile : Tile) = 
        let lastIndex = (tile |> sideLength) - 1
        tile.Lines |> List.map (List.item lastIndex)

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

let countUnique lookup = 
    List.filter (isUnique lookup) >> List.length

let isEdgeTile lookup (_, sides) = 
    2 = countUnique lookup sides 

let isCornerTile lookup (_, sides) = 
    4 = countUnique lookup sides 

let isInnerTile lookup (_, sides) = 
    0 = countUnique lookup sides

let rotateCcw lines = 
    let len = lines |> List.head |> String.length
    [0 .. len - 1]
    |> List.map (fun i -> lines |> List.map (fun r -> r[len - 1 - i]) |> List.toArray |> String)

let flipHorizontal lines = 
    lines |> List.rev 

let flipVertical lines = 
    lines |> List.map String.rev 

let rec times n fn = 
    if n > 0 then fn >> times (n - 1) fn
    else id 

let rotations lines = 
    [ 0 .. 3 ] |> List.map (fun i -> lines |> times i rotateCcw) 

let placeTiles (dim : int) = ()

let run fileName =
    let chunks = readChunks fileName
    let chunk0 = chunks |> List.head 
    let chunk0List = chunk0.Split("\n") |> Array.toList
    let headless = chunk0List |> List.tail
    printfn "original:"
    headless |> List.iter (printfn "%A")
    printfn "rotate ccw:"
    headless |> rotateCcw |> List.iter (printfn "%A")
    printfn "flip horizontal:"
    headless |> flipHorizontal |> List.iter (printfn "%A")
    printfn "flip vertical:"
    headless |> flipVertical |> List.iter (printfn "%A")

    printfn "...."
    headless |> rotations |> List.iter (fun r -> printfn ">>>"; r |> List.iter (printfn "%A"))

    let tiles = chunks |> List.map parseChunk
    let lookup = tiles |> List.collect snd |> List.countBy id |> Map.ofList
    lookup |> Map.toList |> List.iter (printfn "%A")
    let foo = tiles |> List.map (fun (t, sides) -> (t, sides |> List.filter (isUnique lookup) |> List.length))
    foo |> List.iter (printfn "%A")
    let dimensions = tiles |> List.length |> float |> sqrt |> int
    printfn "dim %d" dimensions
    let cornerTiles = 
        tiles |> List.filter (isCornerTile lookup)
    let edgeTiles = 
        tiles |> List.filter (isEdgeTile lookup)
    let innerTiles = 
        tiles |> List.filter (isInnerTile lookup)

    let tile = {
        Number = 2311
        Lines = [
            ['.';'.';'#';'#';'.';'#';'.';'.';'#';'.']
            ['#';'#';'.';'.';'#';'.';'.';'.';'.';'.']
            ['#';'.';'.';'.';'#';'#';'.';'.';'#';'.']
            ['#';'#';'#';'#';'.';'#';'.';'.';'.';'#']
            ['#';'#';'.';'#';'#';'.';'#';'#';'#';'.']
            ['#';'#';'.';'.';'.';'#';'.';'#';'#';'#']
            ['.';'#';'.';'#';'.';'#';'.';'.';'#';'#']
            ['.';'.';'#';'.';'.';'.';'.';'#';'.';'.']
            ['#';'#';'#';'.';'.';'.';'#';'.';'#';'.']
            ['.';'.';'#';'#';'#';'.';'.';'#';'#';'#']
        ]
    }

    printfn "tile %A" tile 

    printfn "rotated %A" (Tile.rotateCcw tile)

    printfn "north: %A" (Tile.north tile)

    printfn "west: %A" (Tile.west tile)

    printfn "south: %A" (Tile.south tile)

    printfn "east: %A" (Tile.east tile)

    // let imageMap = placeTiles dim tiles
    printfn "%A" cornerTiles 
    let firstCornerTile = cornerTiles |> List.head 
    let cornerTilesNumbers = 
        tiles |> List.filter (isCornerTile lookup) |> List.map (fst >> int64)
    cornerTilesNumbers |> List.reduce (*) |> printfn "%d"

"sample" |> run
