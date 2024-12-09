// Advent of Code 2024. Day 09
// dotnet fsi aoc09.fsx

open System
open System.IO

type DiskFile = 
    { fileId : int
      startIndex : int 
      endIndex : int 
      blocks : int }

type FreeSpace = 
    { startIndex: int 
      endIndex : int
      blocks : int }

type DiskEntry = 
    | DiskFileEntry of DiskFile 
    | FreeSpaceEntry of FreeSpace 

let trim (input : string) = input.Trim()

let run fileName = 
    let text = File.ReadAllText fileName |> trim 
    text |> printfn "%A"

run "sample"
