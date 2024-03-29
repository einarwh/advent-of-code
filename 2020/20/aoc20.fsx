// Advent of Code 2020. Day 20: Jurassic Jigsaw
// dotnet fsi aoc20.fsx

open System.IO

[<AutoOpen>]
module Types =

    type Tile = {
        Number : int 
        Lines : char list list
    }

let rotateLinesCcw (lines : char list list) = 
    let lastIndex = (lines |> List.head |> List.length) - 1
    [0 .. lastIndex]
    |> List.map (fun i -> lines |> List.map (fun r -> r[lastIndex - i]))

let flipLinesVertical (lines : char list list) = 
    lines |> List.map (List.rev)

let rotationsForLines (lines : char list list) = 
    let n = lines 
    let w = n |> rotateLinesCcw
    let s = w |> rotateLinesCcw 
    let e = s |> rotateLinesCcw
    let basic = [ n; w; s; e ]
    let flipped = basic |> List.map flipLinesVertical 
    basic @ flipped

module Tile = 

    let create number lines = { Number = number; Lines = lines }

    let sideLength (tile : Tile) = 
        tile.Lines |> List.head |> List.length 

    let rotateCcw (tile : Tile) = 
        { tile with Lines = rotateLinesCcw tile.Lines }

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
        let flipped = basic |> List.map flipVertical 
        basic @ flipped

    let number (tile : Tile) = tile.Number

    let lines (tile : Tile) = tile.Lines

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

let borderless (lines : char list list) = 
    lines[.. lines.Length - 2] 
    |> List.tail
    |> List.map (fun line -> line[.. line.Length - 2] |> List.tail)

let isUnique lookup side =
    1 = Map.find side lookup

let countUnique lookup = 
    List.filter (isUnique lookup) >> List.length

let isCornerTile lookup tile = 
    4 = countUnique lookup (Tile.sidePermutations tile) 

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
        | [] -> failwith "not found?" 
        | tile :: rest -> 
            let rotations = tile |> Tile.rotations
            let matches = rotations |> List.filter (fun t -> target = selector t)
            match matches with 
            | [] -> loop rest (tile :: used)
            | [t] -> (t, (List.rev used) @ rest) 
            | _ -> failwith "too many matches!"
    loop candidates []

let chooseStartTile lookup (tiles : Tile list) = 
    let cornerTiles = tiles |> List.filter (isCornerTile lookup)
    let tile = cornerTiles |> List.head 
    let nw = tile |> toCornerNW lookup
    let rest = tiles |> List.filter (fun t -> t.Number <> nw.Number)
    (nw, rest)

let placeTiles (lookup : Map<char list, int>) (tiles : Tile list) = 
    let numberOfTiles = tiles |> List.length
    let dim = numberOfTiles |> float |> sqrt |> int
    let lastIndex = dim - 1
    let rec loop (x, y) tiles map = 
        let prevSelector = if x = 0 then Tile.south else Tile.east 
        let nextSelector = if x = 0 then Tile.north else Tile.west 
        let nextPos = if x = lastIndex then (0, y + 1) else (x + 1, y)
        let prevPos = if x = 0 then (0, y - 1) else (x - 1, y)
        let prevTile = Map.find prevPos map
        let target = prevTile |> prevSelector 
        let (tile, restTiles) = findTile target nextSelector tiles
        let map = map |> Map.add (x, y) tile 
        if (x = lastIndex && y = lastIndex) then map 
        else 
            loop nextPos restTiles map 
    let (startTile, restTiles) = chooseStartTile lookup tiles 
    let map = Map.empty |> Map.add (0, 0) startTile
    loop (1, 0) restTiles map

let combineTiles (tiles : Tile list) =
    let linesOfTiles = tiles |> List.map (Tile.lines >> borderless)
    let rowCount = linesOfTiles |> List.head |> List.length 
    [ 0 .. rowCount - 1 ]
    |> List.map (fun rowIndex -> linesOfTiles |> List.map (List.item rowIndex) |> List.concat)

let combineMap (imageMap : Map<int * int, Tile>) = 
    let dim = imageMap |> Map.count |> float |> sqrt |> int
    let range = [ 0 .. dim - 1 ]
    range 
    |> List.collect (fun y -> range |> List.map (fun x -> Map.find (x, y) imageMap) |> combineTiles)

let checkPattern (indexes : int list) (chars : char list) = 
    indexes |> List.forall (fun ix -> chars[ix] = '#')

let rec findMonsters (top : char list) (mid : char list) (low: char list) = 
    let monsterLength = 20
    let pat1 = [18]
    let pat2 = [0;5;6;11;12;17;18;19]
    let pat3 = [1;4;7;10;13;16]
    let rec loop (ix : int) (top : char list) (mid : char list) (low : char list) found = 
        if top.Length >= monsterLength then 
            let found = 
                if checkPattern pat1 top && checkPattern pat2 mid && checkPattern pat3 low then 
                    ix :: found 
                else found 
            loop (ix + 1) (List.tail top) (List.tail mid) (List.tail low) found 
        else 
            found 
    loop 0 top mid low []

let findSeaMonsters (lines : char list list) = 
    let rec loop (y : int) (lines : char list list) positions = 
        match lines with 
        | a :: b :: c :: rest -> 
            let xs = findMonsters a b c 
            let ps = xs |> List.map (fun x -> (x, y))
            loop (y + 1) (b :: c :: rest) (ps @ positions)
        | _ -> positions
    loop 0 lines []

let countPounds (lines : char list list) = 
    lines |> List.map (fun line -> line |> List.filter ((=) '#') |> List.length) |> List.sum

let solvePart1 (tiles : Tile list) = 
    let lookup = tiles |> List.collect (Tile.sidePermutations) |> List.countBy id |> Map.ofList
    tiles 
    |> List.filter (isCornerTile lookup)
    |> List.map (Tile.number >> int64)
    |> List.reduce (*)

let solvePart2 (tiles : Tile list) = 
    let lookup = tiles |> List.collect (Tile.sidePermutations) |> List.countBy id |> Map.ofList
    let imageMap = placeTiles lookup tiles 
    let lines = combineMap imageMap
    let pounds = countPounds lines
    let rec loop permutations = 
        match permutations with 
        | [] -> failwith "alas no sea monsters"
        | lines :: rest -> 
            let seaMonsterPositions = lines |> findSeaMonsters
            if seaMonsterPositions |> List.length > 0 then 
                let roughness = pounds - 15 * List.length seaMonsterPositions
                roughness
            else loop rest 
    let permutations = rotationsForLines lines 
    loop permutations

let run fileName =
    let chunks = readChunks fileName
    let tiles = chunks |> List.map parseChunk
    tiles |> solvePart1 |> printfn "%d"
    tiles |> solvePart2 |> printfn "%d" 

"input" |> run
