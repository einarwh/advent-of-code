// Advent of Code 2023. 
// dotnet fsi aoc08.fsx

open System
open System.IO

let parse (s : string) = 
    let a = s.Substring(0, 3)
    let b = s.Substring(7, 3)
    let c = s.Substring(12, 3)
    (a, (b, c))

let readLines = 
    File.ReadAllLines >> Array.filter ((<>) String.Empty)

let solve (instructions : string) (map : Map<string, string*string>) = 
    let rec loop steps pos = 
        if pos = "ZZZ" then steps
        else 
            let (left, right) = map[pos]
            let next = 
                let insNo = steps % String.length instructions
                match instructions[insNo] with 
                | 'R' -> right 
                | 'L' -> left 
                | _ -> failwith "Wrong"
            loop (steps + 1) next 
    loop 0 "AAA"

let run fileName = 
    let lines = readLines fileName |> Array.toList
    match lines with 
    | h :: t -> 
        let map = t |> List.map parse |> Map.ofList
        solve h map |> printfn "%d"
    | _ -> failwith "Wrong"

"input" |> run 
