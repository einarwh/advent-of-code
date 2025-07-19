// Advent of Code 2016. Day 16: Dragon Checksum.
// dotnet fsi aoc16.fsx

open System
open System.IO

let reverse (s : string) = 
    s |> Seq.toArray |> Array.rev |> fun cs -> new string(cs)

let toggle (s : string) = 
    s |> Seq.toArray |> Array.map (fun c -> if c = '0' then '1' else '0') |> fun cs -> new string(cs) 

let dragon (a : string) = 
    let b = a |> reverse |> toggle 
    a + "0" + b

let fill (length : int) (s : string) = 
    let rec fn (s : string) = 
        if s.Length < length then 
            s |> dragon |> fn 
        else 
            s.Substring(0, length) 
    fn s

let checksum (s : string) = 
    let rec shrink (acc : char list) (cs : char list) = 
        match cs with 
        | [] -> acc |> List.rev 
        | a :: b :: rest -> 
            let c = if a = b then '1' else '0'
            shrink (c :: acc) rest 
        | _ -> failwith "only shrink list of even length"
    let rec fn (cs : char list) =
        if List.length cs % 2 = 1 then new string (cs |> List.toArray)
        else 
            fn (shrink [] cs)
    s |> Seq.toList |> fn

let run fillCount fileName = 
    let text = File.ReadAllText(fileName).Trim()
    text |> fill fillCount |> checksum |> printfn "%s"

run 272 "input.txt"
run 35651584 "input.txt"
