// Advent of Code 2025. Day 05: Cafeteria.
// dotnet fsi aoc05.fsx

open System.IO

let parseRanges (s : string) = 
    match s.Split "-" with 
    | [|s1; s2|] -> int64 s1, int64 s2 
    | _ -> failwith "?"

let parse (s : string) = 
    let chunk1, chunk2 = 
        match s.Split "\n\n" with 
        | [|s1; s2|] -> s1.Trim(), s2.Trim()
        | _ -> failwith "?"
    let ranges = chunk1.Split "\n" |> Array.map parseRanges 
    let ingredients = chunk2.Split "\n" |> Array.map int64 
    ranges, ingredients

let fresh ingredient (start, stop)  = 
    start <= ingredient && ingredient <= stop 
    
let count ranges = 
    let rec loop fresh ingredient ranges = 
        match ranges with 
        | [] -> fresh 
        | (s, e) :: rest -> 
            if ingredient < s then 
                loop fresh s ranges 
            else if ingredient > e then 
                loop fresh ingredient rest 
            else 
                loop (fresh + e - ingredient + 1L) (e + 1L) ranges 
    ranges |> List.sort |> loop 0L 0L  

let run fileName = 
    let ranges, ingredients = fileName |> File.ReadAllText |> parse 
    ingredients 
    |> Array.filter (fun it -> ranges |> Array.exists (fresh it)) |> Array.length |> printfn "%d"
    ranges 
    |> Array.toList |> count |> printfn "%d"

run "input.txt"
