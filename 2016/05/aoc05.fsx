// Advent of Code 2016. Day 05: How About a Nice Game of Chess?
// dotnet fsi aoc05.fsx

open System
open System.IO
open System.Text
open System.Security.Cryptography

let readText fileName = 
    File.ReadAllText(fileName).Trim()

let readLines = 
    File.ReadAllLines
    >> Array.filter (fun line -> line <> String.Empty)
    >> Array.toList

let toHexHash (input : string) = 
    use md5 = MD5.Create()
    input |> Encoding.ASCII.GetBytes |> md5.ComputeHash |> Convert.ToHexString

let rec hack (charsFound : char list) (charsLeft : int) (index : int) (doorId : string) = 
    if charsLeft > 0 then 
        let input = doorId + index.ToString()
        let hash = toHexHash input
        if hash.StartsWith "00000" then 
            // printfn "%d" index
            let ch = Char.ToLower(hash[5])
            printf "%c" ch
            hack (ch :: charsFound) (charsLeft - 1) (index + 1) doorId 
        else 
            hack charsFound charsLeft (index + 1) doorId 
    else 
        charsFound |> List.rev |> fun chars -> new string(chars |> List.toArray) |> fun s -> s.ToLower()

let run fileName = 
    let text = readText fileName
    hack [] 8 0 text 
    |> printfn "%s"

run "sample"
