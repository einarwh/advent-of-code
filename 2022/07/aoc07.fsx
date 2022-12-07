
open System.IO

type Command = 
    | ChangeDirectory of string 
    | ListDirectory 

type FileSystemEntry = 
    | File of (string * int) 
    | Directory of (string * FileSystemEntry list)

let run lines = 
    lines |> printfn "%A"

"sample"
|> File.ReadAllLines 
|> run 
