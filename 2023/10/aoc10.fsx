// Advent of Code 2023. 
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

let northPipes = ['S';'|'; 'L'; 'J']
let westPipes = ['S';'-'; 'J'; '7']
let southPipes = ['S';'|'; '7'; 'F']
let eastPipes = ['S';'-'; 'L'; 'F']

let updateField field y line = 
    line |> Seq.iteri (fun x ch -> Array2D.set field y x ch)

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

let countSteps (field : char[,]) = 
    let rec loop steps prev (x, y) = 
        match Array2D.get field y x with 
        | 'S' -> steps / 2
        | _ ->
            let conns = 
                findConnections field (x, y) 
                |> List.filter ((<>) prev)
            match conns with 
            | [next] -> loop (steps + 1) (x, y) next
            | _ -> failwith "poof"
    let startPos = findStartPos field
    let startConns = findConnections field startPos
    match startConns with 
    | [a; _] ->
        loop 1 startPos a
    | _ -> failwith "oof"

let readLines = 
    File.ReadAllLines
    >> Array.filter (fun line -> line <> String.Empty)

let run fileName = 
    let lines = readLines fileName |> Array.toList
    let width = lines |> List.head |> Seq.length 
    let height = lines |> List.length 
    let field = Array2D.create width height '.'
    lines |> List.mapi (updateField field) |> ignore
    countSteps field |> printfn "%d"

"input" |> run 
