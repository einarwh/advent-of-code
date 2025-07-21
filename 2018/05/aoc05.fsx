// Advent of Code 2018. Day 5: Alchemical Reduction
// dotnet fsi aoc05.fsx

open System
open System.IO

let readText = File.ReadAllText >> (fun s -> s.Trim())

let diff ch1 ch2 = abs (int ch1 - int ch2)

let rec shrink chars = 
    match chars with 
    | a :: b :: rest -> 
        if diff a b = 32 then shrink rest 
        else a :: shrink (b :: rest)
    | _ -> chars

let reduce (s : string) = 
    let rec loop chars = 
        let shrunk = shrink chars 
        if List.length shrunk = List.length chars then chars
        else loop shrunk 
    s |> Seq.toList |> loop |> List.toArray |> String

let remove (s : string) (lc, uc) = 
    s.Replace(String [|lc|], "").Replace(String [|uc|], "")

let shortest (s : string) = 
    List.zip [ 'a' .. 'z' ] [ 'A' .. 'Z' ] 
    |> List.map (remove s >> reduce)
    |> List.sortBy (String.length)
    |> List.head 

let run fileName = 
    let txt = readText fileName 
    txt |> reduce |> String.length |> printfn "%d"
    txt |> shortest |> String.length |> printfn "%d" 
    
"input.txt" |> run 
