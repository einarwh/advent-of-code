// Advent of Code 2023. Day 10: Pipe Maze
// dotnet fsi aoc10.fsx

open System
open System.IO

module Array2D = 
    let tryGet (a : 'a[,]) index1 index2 = 
        let first = index1 >= 0 && index1 < a.GetLength(0)
        let second = index2 >= 0 && index2 < a.GetLength(1)
        if first && second then 
            Some <| Array2D.get a index1 index2
        else 
            None 

    let toList (a : 'a[,]) : 'a list list = 
        let index1List = [0 .. a.GetLength(0) - 1]
        let index2List = [0 .. a.GetLength(1) - 1]
        index1List 
        |> List.map (fun index1 -> index2List |> List.map (fun index2 -> Array2D.get a index1 index2))
        
let northPipes = ['S';'|'; 'L'; 'J']
let westPipes = ['S';'-'; 'J'; '7']
let southPipes = ['S';'|'; '7'; 'F']
let eastPipes = ['S';'-'; 'L'; 'F']

let updateField field y line = 
    line |> Seq.iteri (fun x ch -> Array2D.set field y x ch)

let rec updateWallField wallField field tiles = 
    match tiles with 
    | [] -> ()
    | (x, y) :: t -> 
        let pipe = Array2D.get field y x
        Array2D.set wallField y x pipe
        updateWallField wallField field t 

let findStartPos (field : char[,]) = 
    let xlen = field.GetLength(1)
    let rec loop y x = 
        match Array2D.get field y x with 
        | 'S' -> (x, y)
        | _ -> 
            let (x', y') = 
                if x + 1 = xlen then (0, y + 1) else (x + 1, y)
            loop y' x' 
    loop 0 0 

let findConnections (field : char[,]) (x, y) = 
    let current = Array2D.get field y x
    let tryConnect pos pipes p = if List.contains p pipes then Some pos else None
    let check (x, y) sourcePipes targetPipes =
        if List.contains current sourcePipes then 
            Array2D.tryGet field y x
            |> Option.bind (tryConnect (x, y) targetPipes)
        else None
    [ check (x, y - 1) northPipes southPipes
      check (x - 1, y) westPipes eastPipes 
      check (x, y + 1) southPipes northPipes 
      check (x + 1, y) eastPipes westPipes ]
    |> List.choose id 

let findLoop (field : char[,]) = 
    let rec fn (x, y) positions = 
        match Array2D.get field y x with 
        | 'S' -> positions
        | _ ->
            let prev = List.head positions
            let conns = 
                findConnections field (x, y) 
                |> List.filter ((<>) prev)
            match conns with 
            | [next] -> fn next ((x, y) :: positions)
            | _ -> failwith "poof"
    let startPos = findStartPos field
    let startConns = findConnections field startPos
    match startConns with 
    | [a; _] ->
        fn a [startPos]
    | _ -> failwith "oof"

let countInside (s : string) = 
    let rec fn inside count lst =
        match lst with 
        | [] -> count 
        | h :: t -> 
            match h with 
            | '|' -> 
                fn (not inside) count t 
            | _ ->
                let inc = if inside then 1 else 0
                fn inside (count + inc) t  
    fn false 0 (s |> Seq.toList) 

let replaceS field = 
    let startPos = findStartPos field
    let startConns = findConnections field startPos
    let toOffset (xStart, yStart) (x, y) = 
        (x - xStart, y - yStart)
    match startConns with 
    | [a; b] ->
        let pipe = 
            match (toOffset startPos a, toOffset startPos b) with 
            | ( 0, -1), (-1,  0) -> 'J' // N, W
            | ( 0, -1), ( 0,  1) -> '|' // N, S
            | ( 0, -1), ( 1,  0) -> 'L' // N, E
            | (-1,  0), ( 0,  1) -> '7' // W, S
            | (-1,  0), ( 1,  0) -> '-' // W, E
            | ( 0,  1), ( 1,  0) -> 'F' // S, E
            | _ -> failwith "booh"
        let (xStart, yStart) = startPos
        Array2D.set field yStart xStart pipe
    | _ -> failwith "baah"

let replace (oldValue : string) (newValue : string) (s : string) = 
    s.Replace(oldValue, newValue)

let readLines = 
    File.ReadAllLines >> Array.filter ((<>) String.Empty)

let run fileName = 
    let lines = readLines fileName |> Array.toList
    let width = lines |> List.head |> Seq.length 
    let height = lines |> List.length 
    let field = Array2D.create height width ' '
    lines |> List.mapi (updateField field) |> ignore
    let loop = findLoop field 
    loop |> List.length |> (fun tiles -> tiles / 2) |> printfn "%d"
    replaceS field
    let wallField = Array2D.create height width ' '
    updateWallField wallField field loop 
    Array2D.toList wallField  
    |> List.map (List.toArray >> String)
    |> List.map (replace "-" "")
    |> List.map (replace "LJ" "")
    |> List.map (replace "L7" "|")
    |> List.map (replace "FJ" "|")
    |> List.map (replace "F7" "")
    |> List.sumBy countInside
    |> printfn "%d"

"input" |> run 
