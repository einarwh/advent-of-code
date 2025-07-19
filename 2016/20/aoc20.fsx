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

let combineRanges (ranges : (int64*int64) list) : (int64*int64) list = 
    let rec fn acc (currentFirst, currentLast) ranges = 
        // printfn "combining... %A %A %A" acc (currentFirst, currentLast) ranges
        match ranges with 
        | [] -> ((currentFirst, currentLast) :: acc) |> List.rev 
        | (first, last) :: rest -> 
            if first < currentFirst then 
                failwith "not sorted?"
            else 
                if first > currentLast + 1L then 
                    // printfn "GAP"
                    // Gap! 
                    fn ((currentFirst, currentLast) :: acc) (first, last) rest
                else 
                    fn acc (currentFirst, max currentLast last) rest 
    match ranges with 
    | [] -> failwith "?"
    | h :: t -> fn [] h t

let readLines = 
    File.ReadAllLines
    >> Array.filter (fun line -> line <> String.Empty)
    >> Array.toList

let run fileName = 
    let lines = readLines fileName
    let ranges = lines |> List.choose parse |> List.sort 
    let combined = ranges |> combineRanges
    combined |> List.head |> snd |> (fun last -> last + 1L) |> printfn "%d"
    let sumBlocked = combined |> List.map (fun (a, b) -> b - a + 1L) |> List.sum
    let total = 4294967295L + 1L
    let sumAllowed = total - sumBlocked
    sumAllowed |> printfn "%d"

run "input.txt"
