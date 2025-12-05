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

let rec combine (ranges : (int64 * int64) list) = 
    match ranges with 
    | (a, b) :: (c, d) :: rest ->
        if b + 1L >= c then combine ((a, max b d) :: rest) 
        else (a, b) :: combine ((c, d) :: rest) 
    | _ -> ranges 

let run fileName = 
    let ranges, ingredients = fileName |> File.ReadAllText |> parse 
    ingredients 
    |> Array.filter (fun it -> ranges |> Array.exists (fresh it)) 
    |> Array.length 
    |> printfn "%d"
    ranges 
    |> Array.toList |> List.sort |> combine |> List.sumBy (fun (start, stop) -> stop - start + 1L) 
    |> printfn "%d"

run "input.txt"
