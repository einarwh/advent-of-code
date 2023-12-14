// Advent of Code 2023. Day 14: Parabolic Reflector Dish
// dotnet fsi aoc14.fsx

open System
open System.IO

let readLines =
    File.ReadAllLines >> Array.filter ((<>) String.Empty)

let rollNorthStep (s1, s2) = 
    let line1 = s1 |> Seq.toList
    let line2 = s2 |> Seq.toList
    let zipped = List.zip line1 line2 
    let rolled = zipped |> List.map (fun (a, b) -> if a = '.' && b = 'O' then (b, a) else (a, b))
    let s1' = rolled |> List.map fst |> List.toArray |> String
    let s2' = rolled |> List.map snd |> List.toArray |> String
    (s1', s2')

let rec rollNorth (lines : string list) : string list = 
    match lines with 
    | a :: b :: rest -> 
        let (a', b') = rollNorthStep (a, b)
        a' :: rollNorth (b' :: rest)
    | _ -> lines 

let rec tiltNorth (current : string list) : string list = 
    let tilted = rollNorth current 
    if tilted = current then current else tiltNorth tilted 

let countRocks (line : string) : int = 
    line |> Seq.filter ((=) 'O') |> Seq.length 

let calculateLoad (lines : string list) = 
    let len = lines |> List.length 
    lines |> List.mapi (fun i line -> (len - i) * countRocks line) |> List.sum
    
let run fileName =
    let lines = readLines fileName |> Array.toList
    let tilted = lines |> tiltNorth
    tilted |> calculateLoad |> printfn "%d"

"input" |> run
