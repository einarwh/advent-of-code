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

type Stack = char list

type Stacks = Stack array

let parseStackCount (s : string) = 
    Regex.Split(s.Trim(), "\s+") |> Array.last |> int

let tryGetChar (offset : int) (s : string) =
    if offset < s.Length then 
        match s.[offset] with 
        | ' ' -> None
        | ch -> 
            Some ch
    else None 

let rec parseStack (lines : string list) (offset : int)  = 
    let rec fn values lines = 
        match lines with 
        | [] -> values 
        | h :: t ->
            let maybeCh = tryGetChar offset h 
            let values' = maybeCh :: values
            t |> fn values'
    fn [] lines |> List.choose id

let parseStacks (s : string) : Stack array =
    let stackLines = s.Split("\n") |> Array.toList |> List.rev
    match stackLines with 
    | [] -> failwith "not enough lines"
    | h :: t -> 
        let stackCount = h |> parseStackCount 
        let offsets = [ 1 .. stackCount ] |> List.map (fun i -> (i-1) * 4 + 1)
        let stacks = offsets |> List.map (parseStack t) |> List.toArray
        stacks

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
    commandLines |> List.map parseCommand

let move (sourceIndex : int) (targetIndex : int) (stacks : Stacks) : Stacks = 
    match stacks.[sourceIndex] with 
    | [] -> failwith "underflow"
    | top :: rest -> 
        stacks.[sourceIndex] <- rest 
        stacks.[targetIndex] <- (top :: stacks.[targetIndex])
        stacks

let runCommand (command : Command) (stacks : Stacks) = 
    let rec moveAmount (amount : int) (sourceIndex : int) (targetIndex) (stacks : Stacks) = 
        if amount > 0 then 
            moveAmount (amount - 1) sourceIndex targetIndex (move sourceIndex targetIndex stacks)
        else 
            stacks
    moveAmount command.amount (command.source - 1) (command.target - 1) stacks

let rec runCommands (commandNo : int) stacks commands = 
    match commands with 
    | [] -> stacks
    | cmd :: rest ->
        rest |> runCommands (commandNo + 1) (runCommand cmd stacks) 

let tops (stacks : Stacks) : char array =
    stacks |> Array.map (List.head)

let run (text : string) = 
    let ss = text.TrimEnd().Split("\n\n") 
    let stacksInput = ss.[0]
    let commandsInput = ss.[1]
    let stacks = parseStacks stacksInput
    let commands = parseCommands commandsInput
    let stacks' = runCommands 0 stacks commands
    stacks' |> tops |> (fun cs -> System.String(cs)) |> printfn "%s"

"input"
|> File.ReadAllText 
|> run 
