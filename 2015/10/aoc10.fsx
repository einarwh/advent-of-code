// Advent of Code 2015. Day 10: Elves Look, Elves Say.
// dotnet fsi aoc10.fsx

open System
open System.IO

let grp (chars : char list) = 
    let rec fn (acc : (int*char) list) (pairs : (int*char) list) : (int*char) list = 
        match pairs with 
        | [] -> acc |> List.rev 
        | [it] -> fn (it :: acc) [] 
        | (n1, c1) :: (n2, c2) :: rest ->
            if c1 = c2 then 
                fn acc ((n1 + n2, c1) :: rest)
            else 
                fn ((n1, c1) :: acc) ((n2, c2) :: rest)
    chars |> List.map (fun ch -> (1, ch)) |> fn []

let lookAndSay (s : string) = 
    let desc (n, c) = 
        n.ToString() + c.ToString()
    let g = s |> Seq.toList |> grp
    g |> List.map desc |> String.concat ""

let apply times s = 
    let rec loop count s = 
        if count < times then 
            loop (count + 1) (lookAndSay s)
        else 
            s 
    loop 0 s 

let run fileName = 
    let text = File.ReadAllText(fileName).Trim()
    text |> apply 40 |> String.length |> printfn "%d"
    text |> apply 50 |> String.length |> printfn "%d"

run "input.txt"
