// Advent of Code 2016. Day 05: How About a Nice Game of Chess?
// dotnet fsi aoc05.fsx

open System
open System.IO
open System.Linq
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

let hack1 (doorId : string) = 
    let rec fn (charsFound : char list) (charsLeft : int) (index : int) (doorId : string) = 
        if charsLeft > 0 then 
            let input = doorId + index.ToString()
            let hash = toHexHash input
            if hash.StartsWith "00000" then 
                let ch = Char.ToLower(hash[5])
                printf "%c" ch
                fn (ch :: charsFound) (charsLeft - 1) (index + 1) doorId 
            else 
                fn charsFound charsLeft (index + 1) doorId 
        else 
            printfn ""
    fn [] 8 0 doorId

let hack2 (doorId : string) = 
    let rec fn (chars : char array) (charsLeft : int) (index : int) (doorId : string) = 
        if charsLeft > 0 then 
            let input = doorId + index.ToString()
            let hash = toHexHash input
            if hash.StartsWith "00000" then 
                let pos = hash[5] |> Char.GetNumericValue |> int 
                let ch = Char.ToLower(hash[6])
                let left = 
                    if pos >= 0 && pos < 8 && chars[pos] = '_' then 
                        chars[pos] <- ch
                        new string(chars) |> printfn "%s"
                        charsLeft - 1
                    else 
                        charsLeft 
                fn chars left (index + 1) doorId
            else 
                fn chars charsLeft (index + 1) doorId 
        else 
            printfn ""
    let chars = Enumerable.Repeat('_', 8).ToArray();
    fn chars chars.Length 0 doorId

let run fileName = 
    let text = readText fileName
    // hack1 text
    hack2 text

run "input"
