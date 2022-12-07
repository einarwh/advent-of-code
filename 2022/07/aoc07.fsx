
open System.IO
open System.Text.RegularExpressions

type Command = 
    | ChangeDirectory of string 
    | ListDirectory 

type FileSystemEntry = 
    | File of (string * int) 
    | Directory of (string * FileSystemEntry list)

let parseChangeDirectoryCommand (line : string) : Command option = 
    let pattern = "^\$ cd (.+)$"
    let m = Regex.Match(line, pattern)
    if m.Success then
        m.Groups.[1].Value |> ChangeDirectory |> Some
    else 
        None 

let parseListCommand (line : string) : Command option = 
    let pattern = "^\$ ls"
    let m = Regex.Match(line, pattern)
    if m.Success then
        ListDirectory |> Some
    else 
        None

let consumeChangeDirectoryCommand (lines : string list) : string list = 
    match lines with 
    | [] -> failwith "expected cd command"
    | h :: t -> 
        parseChangeDirectoryCommand h |> ignore 
        t 

let consumeListCommand (lines : string list) : string list = 
    match lines with 
    | [] -> failwith "expected ls command"
    | h :: t -> 
        parseListCommand h |> ignore 
        t 

let consumeListContent (lines : string list) = 
    let rec consume acc lines = 
        match lines with 
        | [] -> (acc, lines) 
        | h :: rest -> 
            match parseChangeDirectoryCommand h with 
            | Some cd -> (acc, lines)
            | None -> 
                printfn "Consumed %s" h 
                consume (h :: acc) rest 
    let (consumed, remaining) = consume [] lines 
    remaining

let consumeDirectory (lines : string list) = 
    lines 
    |> consumeChangeDirectoryCommand 
    |> consumeListCommand
    |> consumeListContent

let run (lines : string list) = 
    consumeDirectory lines

"sample"
|> File.ReadAllLines 
|> Array.toList
|> run 
|> printfn "%A"
