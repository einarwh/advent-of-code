// Advent of Code 2022. 
// Day 5: Supply Stacks.
// dotnet fsi aoc05.fsx

open System.IO
open System.Text.RegularExpressions

type Command = {
    amount : int
    source : int 
    target : int 
}

type Stacks = string list array

let parseStackCount (s : string) = 
    Regex.Split(s.Trim(), "\s+") |> Array.last |> int

let parseStacks (s : string) =
    let stackLines = s.Split("\n") |> Array.toList |> List.rev
    match stackLines with 
    | [] -> failwith "not enough lines"
    | h :: t -> 
        let stackCount = h |> parseStackCount 
        stackCount |> printfn "%d"
        t |> printfn "Stacks input: %A"

let parseCommand (s : string) = 
    let pattern = "^move (\d+) from (\d+) to (\d+)$"
    let m = Regex.Match(s, pattern)
    if m.Success then
        let read (ix : int) = m.Groups.[ix].Value |> int
        { amount = read 1
          source = read 2
          target = read 3 }
    else 
        s |> sprintf "failed to parse %s" |> failwith  


let parseCommands (s : string) =
    let commandLines = s.Split("\n") |> Array.toList 
    commandLines |> List.map parseCommand |> printfn "Commands input: %A"

let run (text : string) = 
    let ss = text.Trim().Split("\n\n") 
    let stacksInput = ss.[0]
    let commandsInput = ss.[1]
    parseStacks stacksInput
    parseCommands commandsInput

"sample"
|> File.ReadAllText 
|> run 
