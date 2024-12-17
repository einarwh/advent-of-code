// Advent of Code 2024. Day 17
// dotnet fsi aoc17.fsx

open System
open System.IO

type Computer = {
    regA : int64
    regB : int64
    regC : int64
    pointer : int64
    program : int64 array 
}

let trim (input : string) = input.Trim()

let split (splitter : string) (input : string) = input.Split(splitter)

let parseRegister (s : string) : int64 = 
    s |> split ": " |> Array.item 1 |> int64

let parseProgram (s : string) : int64 array = 
    s |> split ": " |> Array.item 1 |> split "," |> Array.map int64

let parseComputer (arr : string array) = 
    { regA = arr.[0] |> parseRegister 
      regB = arr.[1] |> parseRegister 
      regC = arr.[2] |> parseRegister 
      pointer = 0 
      program = arr.[4] |> parseProgram }

let run fileName = 
    let text = File.ReadAllText fileName |> trim |> split "\n"
    let computer = parseComputer text 
    computer |> printfn "%A"
    0

run "sample"
