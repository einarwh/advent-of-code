// Advent of Code 2025. Day 02: Gift Shop.
// dotnet fsi aoc02.fsx

open System.IO

let parseRange (s : string) = 
    match s.Split "-" with 
    | [|s1; s2|] -> int64 s1, int64 s2 
    | _ -> failwith "?"

let parseLine (s : string) = 
    s.Split "," 
    |> Array.toList 
    |> List.map parseRange  

let isSimplePattern (s : string) = 
    let len = s.Length / 2 
    s.Substring(0, len) = s.Substring len

let rec check (p : string) (s : string) = 
    if s.Length = 0 then 
        true
    else if s.Length < p.Length then 
        false  
    else 
        s.Substring(0, p.Length) = p && check p (s.Substring p.Length)

let isAnyPattern (s : string) = 
    [1 .. s.Length / 2] |> List.exists (fun r -> check (s.Substring(0, r)) s)

let findInvalid pattern (start, stop) = 
    [ start .. stop ] |> List.filter (string >> pattern)

let sumInvalid pattern ranges = 
    ranges |> List.collect (findInvalid pattern) |> List.sum 

let run fileName = 
    let ranges = fileName |> File.ReadAllText |> parseLine 
    ranges |> sumInvalid isSimplePattern |> printfn "%d"
    ranges |> sumInvalid isAnyPattern |> printfn "%d"

run "input.txt"
