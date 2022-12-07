
open System
open System.IO
open System.Text.RegularExpressions

type Line = 
    | EnterDirectoryLine of string 
    | ExitDirectoryLine 
    | ListLine 
    | FileLine of (string * Int64)
    | DirLine of string

type FileSystemEntry = 
    | FileEntry of (string * Int64)
    | DirEntry of (string * FileSystemEntry list)

type Dir = Dir of (string * Int64 * Dir list)

let tryParseChangeDirectoryLine (s : string) : Line option =
    let pattern = "^\$ cd (.+)$"
    let m = Regex.Match(s, pattern)
    if m.Success then
        let dirName = m.Groups.[1].Value 
        if dirName = ".." then
            Some ExitDirectoryLine
        else 
            Some (EnterDirectoryLine dirName)
    else 
        None 

let tryParseListLine (s : string) : Line option = 
    let pattern = "^\$ ls"
    let m = Regex.Match(s, pattern)
    if m.Success then
        Some ListLine
    else 
        None 

let tryParseFileLine (s : string) : Line option = 
    let pattern = "^(\d+) (.+)$"
    let m = Regex.Match(s, pattern)
    if m.Success then
        let fileSize = m.Groups.[1].Value |> int
        let fileName = m.Groups.[2].Value 
        Some (FileLine (fileName, fileSize))
    else 
        None 

let tryParseDirLine (s : string) : Line option = 
    let pattern = "^dir ([a-z]+)$"
    let m = Regex.Match(s, pattern)
    if m.Success then
        let dirName = m.Groups.[1].Value 
        Some (DirLine dirName)
    else 
        None 

let tryParseLine (s : string) : Line option = 
    s 
    |> tryParseChangeDirectoryLine 
    |> Option.orElse (tryParseListLine s)
    |> Option.orElse (tryParseFileLine s)
    |> Option.orElse (tryParseDirLine s) 

let consumeEnterDirectoryLine (lines : Line list) : (string * Line list) = 
    match lines with 
    | EnterDirectoryLine dirName :: lines -> 
        (dirName, lines)
    | _ -> failwith "Expected cd command."

let consumeListLine (lines : Line list) : Line list = 
    match lines with 
    | ListLine :: lines -> 
        lines 
    | _ -> failwith "Expected ls command."

let consumeListOutput (lines : Line list) : (Line list * Line list) = 
    let rec consume acc lines = 
        match lines with 
        | [] -> (acc, lines) 
        | h :: t -> 
            match h with 
            | FileLine _ 
            | DirLine _ -> consume (h :: acc) t 
            | ExitDirectoryLine 
            | EnterDirectoryLine _ -> (acc, lines)
            | _ -> failwith <| sprintf "Unexpected line %A" h
    consume [] lines

let toFileEntry (line : Line) : FileSystemEntry option = 
    match line with 
    | FileLine (name, size) -> Some (FileEntry (name, size))
    | _ -> None

let rec consumeDirectory (lines : Line list) : (FileSystemEntry * Line list) = 
    let (dirName, lines) = consumeEnterDirectoryLine lines 
    let lines = consumeListLine lines 
    let (output, lines) = consumeListOutput lines 
    let files = output |> List.choose toFileEntry
    let (children, lines) = consumeChildDirectories lines 
    (DirEntry (dirName, files @ children), lines)
and consumeChildDirectories (lines : Line list) : (FileSystemEntry list * Line list) = 
    let rec consume acc lines = 
        match lines with 
        | [] -> (acc, lines) 
        | EnterDirectoryLine d :: _ -> 
            let (entry, lines) = consumeDirectory lines 
            consume (entry :: acc) lines
        | ExitDirectoryLine :: lines -> 
            (acc, lines)
        | h :: _ -> failwith <| sprintf "Unexpected line: %A" h
    consume [] lines

let getSize dir = 
    match dir with 
    | Dir (_, size, _) -> size

let rec getFileSystemEntrySize (entry : FileSystemEntry) = 
    match entry with 
    | FileEntry (_, size) -> 
        size
    | DirEntry (_, entries) -> 
        entries |> List.sumBy getFileSystemEntrySize

let rec toDir (entry : FileSystemEntry) : Dir option = 
    match entry with 
    | DirEntry (name: string, entries) ->
        let childDirs = entries |> List.choose toDir
        let size = entries |> List.sumBy getFileSystemEntrySize
        Some (Dir (name, size, childDirs))
    | _ -> None

let toRootDir (rootEntry : FileSystemEntry) : Dir = 
    match toDir rootEntry with 
    | None -> failwith "?"
    | Some dir -> dir

let rec getDirSizes (dir : Dir) : Int64 list = 
    match dir with 
    | Dir (_, size, childDirs) ->
        let childSizes = childDirs |> List.collect getDirSizes
        size :: childSizes

let run root = 
    let dirSizes = root |> getDirSizes
    dirSizes
    |> List.filter (fun s -> s <= 100000L)
    |> List.sum
    |> printfn "Sum of small dirs: %d"
    let total = 70000000L
    let needed = 30000000L
    let used = match root with | Dir (_, size, _) -> size
    let free =  total - used 
    let missing = needed - free
    dirSizes
    |> List.filter (fun s -> s >= missing)
    |> List.rev 
    |> List.head
    |> printfn "Size of dir to delete: %d"

"input"
|> File.ReadAllLines 
|> Array.toList
|> List.choose tryParseLine
|> consumeDirectory
|> (fun (root, _) -> toRootDir root)
|> run 
