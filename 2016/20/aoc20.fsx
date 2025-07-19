// Advent of Code 2016. Day 20: Firewall Rules.
// dotnet fsi aoc20.fsx

open System
open System.IO

let parse (s : string) : (int64*int64) option =
    match s.Split "-" with 
    | [|a; b|] -> Some (int64 a, int64 b)
    | _ -> None

let findLowestNonBlockedIp ranges = 
    let rec find (current : int64) (ranges : (int64*int64) list) = 
        match ranges with 
        | [] -> current 
        | (first, last) :: rest ->
            if first < current then 
                if last > current then 
                    find (last + 1L) rest 
                else 
                    find current rest 
            else if first = current then 
                find (last + 1L) rest 
            else 
                current 
    find 0 ranges 

let readLines = 
    File.ReadAllLines
    >> Array.filter (fun line -> line <> String.Empty)
    >> Array.toList

let run fileName = 
    let lines = readLines fileName
    let ranges = lines |> List.choose parse |> List.sort 
    // ranges |> List.iter (fun (a, b) -> printfn "%d-%d" a b)
    ranges |> findLowestNonBlockedIp |> printfn "%A"

run "input.txt"
