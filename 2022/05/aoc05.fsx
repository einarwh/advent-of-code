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

type Crates = char list 

type Stacks = Stack array

type CrateMover = Command -> Stacks -> Stacks

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

let takeAmount (amount : int) (stack : Stack) : (Crates * Stack) = 
    let rec fn (amount : int) (taken : Crates) (stack : Stack) = 
        if amount > 0 then 
            match stack with 
            | [] -> failwith "takeAmount underflow"
            | top :: rest -> 
                rest |> fn (amount - 1) (top :: taken) 
        else 
            (taken, stack)
    fn amount [] stack

let moveAmount9000 (amount : int) (sourceIndex : int) (targetIndex : int) (stacks : Stacks) : Stacks = 
    match takeAmount amount stacks.[sourceIndex] with 
    | (taken, stack) -> 
        stacks.[sourceIndex] <- stack 
        stacks.[targetIndex] <- (taken @ stacks.[targetIndex])
        stacks

let moveAmount9100 (amount : int) (sourceIndex : int) (targetIndex : int) (stacks : Stacks) : Stacks = 
    match takeAmount amount stacks.[sourceIndex] with 
    | (taken, stack) -> 
        stacks.[sourceIndex] <- stack 
        stacks.[targetIndex] <- (List.rev taken @ stacks.[targetIndex])
        stacks

let runCommand9000 (command : Command) (stacks : Stacks) = 
    moveAmount9000 command.amount (command.source - 1) (command.target - 1) stacks

let runCommand9100 (command : Command) (stacks : Stacks) = 
    moveAmount9100 command.amount (command.source - 1) (command.target - 1) stacks

let rec runCommands (commandNo : int) (mover : CrateMover) stacks commands = 
    match commands with 
    | [] -> stacks
    | cmd :: rest ->
        rest |> runCommands (commandNo + 1) mover (mover cmd stacks) 

let printTopCrates (stacks : Stacks) = 
    stacks |> Array.map (List.head) |> (fun cs -> System.String(cs)) |> printfn "%s"

let run (text : string) = 
    let ss = text.TrimEnd().Split("\n\n") 
    let stacksInput = ss.[0]
    let commandsInput = ss.[1]
    let stacks = parseStacks stacksInput
    let commands = parseCommands commandsInput
    let stacks9000 = runCommands 0 runCommand9000 (stacks |> Array.copy) commands
    let stacks9100 = runCommands 0 runCommand9100 (stacks |> Array.copy) commands
    stacks9000 |> printTopCrates
    stacks9100 |> printTopCrates

"input"
|> File.ReadAllText 
|> run 
