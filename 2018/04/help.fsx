open System
open System.IO

let readLines = 
    File.ReadAllLines >> Array.filter ((<>) String.Empty) 

let writeLines fileName content =
    File.WriteAllLines(fileName, content)

let run fileName = 
    fileName 
    |> readLines 
    |> Array.sort
    |> writeLines "output.txt"

"input.txt" |> run 
