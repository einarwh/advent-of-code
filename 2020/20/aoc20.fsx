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

    let rec private times n fn = 
        if n > 0 then fn >> times (n - 1) fn
        else id 

    let create number lines = { Number = number; Lines = lines }

    let sideLength (tile : Tile) = 
        tile.Lines |> List.head |> List.length 

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

    let sides (tile : Tile) = 
        [ north tile; west tile; south tile; east tile ]

    let sidePermutations (tile : Tile) = 
        let s = sides tile
        let r = s |> List.map (List.rev)
        s @ r 

    let rotations (tile : Tile) = 
        let n = tile 
        let w = n |> rotateCcw
        let s = w |> rotateCcw 
        let e = s |> rotateCcw
        let basic = [ n; w; s; e ]
        let flipped = basic |> List.map flipHorizontal 
        basic @ flipped

let readChunks fileName = 
    let text = File.ReadAllText fileName 
    text.TrimEnd().Split("\n\n") |> Array.toList 

let parseTileNumber (s : string) = 
    let ss = s.Substring(0, s.Length - 1).Split(" ")
    int <| ss[1]

let parseChunk (chunk : string) = 
    match chunk.Split("\n") |> Array.toList with 
    | h :: rows ->
        let number = parseTileNumber h 
        let lines = rows |> List.map (Seq.toList)
        Tile.create number lines 
    | _ -> failwith "oof"

let isUnique lookup side =
    1 = Map.find side lookup

let countUnique lookup = 
    List.filter (isUnique lookup) >> List.length

let isEdgeTile lookup tile = 
    2 = countUnique lookup (Tile.sidePermutations tile)

let isCornerTile lookup tile = 
    4 = countUnique lookup (Tile.sidePermutations tile) 

let isInnerTile lookup tile = 
    0 = countUnique lookup (Tile.sidePermutations tile)

let rotateCcw lines = 
    let len = lines |> List.head |> String.length
    [0 .. len - 1]
    |> List.map (fun i -> lines |> List.map (fun r -> r[len - 1 - i]) |> List.toArray |> String)


let toCornerNW lookup tile = 
    let rec loop tile = 
        if tile |> Tile.west |> isUnique lookup && tile |> Tile.north |> isUnique lookup then 
            tile 
        else 
            tile |> Tile.rotateCcw |> loop 
    if isCornerTile lookup tile then loop tile else tile 

let findTile (target : char list) (selector : Tile -> char list) (candidates : Tile list) = 
    let rec loop remaining used = 
        match remaining with 
        | [] -> failwith "Not found?" 
        | tile :: rest -> 
            let maybe = Tile.rotations tile |> List.tryFind (fun t -> target = selector t)
            match maybe with 
            | Some t -> (t, (List.rev used) @ rest)
            | None -> loop rest (tile :: used)
    loop candidates []

let rec placeTiles (dim : int) (prev : int * int) (pos : int * int) (lookup : Map<char list, int>) (cornerTiles : Tile list) (edgeTiles : Tile list) (innerTiles : Tile list) (map : Map<int * int, Tile>) = 
    let lastIndex = dim - 1
    match pos with 
    | (0, 0) -> // NW corner 
        let tile = cornerTiles |> List.head |> toCornerNW lookup
        let map = map |> Map.add pos tile 
        placeTiles dim pos (1, 0) lookup (cornerTiles |> List.tail) edgeTiles innerTiles map
    | (x, 0) when x = dim - 1 -> // NE corner 
        let prevTile = map |> Map.find prev 
        let target = prevTile |> Tile.east 
        let (tile, restTiles) = findTile target Tile.west cornerTiles 
        let map = map |> Map.add pos tile 
        placeTiles dim pos (0, 1) lookup restTiles edgeTiles innerTiles map 
    | (0, y) when y = dim - 1 -> // SW corner 
        let prevTile = map |> Map.find prev 
        let target = prevTile |> Tile.east 
        let (tile, restTiles) = findTile target Tile.west cornerTiles 
        let map = map |> Map.add pos tile 
        placeTiles dim pos (1, y) lookup restTiles edgeTiles innerTiles map 
    | (x, y) when x = dim - 1 && y = dim - 1 -> 
        let prevTile = map |> Map.find prev 
        let target = prevTile |> Tile.east 
        let (tile, restTiles) = findTile target Tile.west cornerTiles 
        let map = map |> Map.add pos tile 
        // Done!
        map
    | (x, 0) -> // N edge 
        let prevTile = map |> Map.find prev 
        let target = prevTile |> Tile.east 
        let (tile, restTiles) = findTile target Tile.west edgeTiles 
        let map = map |> Map.add pos tile 
        placeTiles dim pos (x + 1, 0) lookup cornerTiles restTiles innerTiles map 
    | (0, y) -> // W edge 
        let prevTile = map |> Map.find prev 
        let target = prevTile |> Tile.east 
        let (tile, restTiles) = findTile target Tile.west edgeTiles 
        let map = map |> Map.add pos tile 
        placeTiles dim pos (1, y) lookup cornerTiles restTiles innerTiles map 
    | (x, y) when x = dim - 1 -> // E edge 
        let prevTile = map |> Map.find prev 
        let target = prevTile |> Tile.east 
        let (tile, restTiles) = findTile target Tile.west edgeTiles 
        let map = map |> Map.add pos tile 
        placeTiles dim pos (0, y + 1) lookup cornerTiles restTiles innerTiles map 
    




let run fileName =
    let chunks = readChunks fileName
    let tiles = chunks |> List.map parseChunk
    let lookup = tiles |> List.collect (Tile.sidePermutations) |> List.countBy id |> Map.ofList
    let cornerTiles = 
        tiles |> List.filter (isCornerTile lookup)
    let edgeTiles = 
        tiles |> List.filter (isEdgeTile lookup)
    let innerTiles = 
        tiles |> List.filter (isInnerTile lookup)
    let numberOfTiles = tiles |> List.length
    let dim = numberOfTiles |> float |> sqrt |> int

    printfn "dim: %d" dim
    printfn "corner tiles: %A" (cornerTiles |> List.length)
    printfn "edge tiles: %A" (edgeTiles |> List.length)
    printfn "inner tiles: %A" (innerTiles |> List.length)

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
    // printfn "%A" cornerTiles 
    // let firstCornerTile = cornerTiles |> List.head 
    // let cornerTilesNumbers = 
    //     tiles |> List.filter (isCornerTile lookup) |> List.map (fst >> int64)
    // cornerTilesNumbers |> List.reduce (*) |> printfn "%d"

"sample" |> run
