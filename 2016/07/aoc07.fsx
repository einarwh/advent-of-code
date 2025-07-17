// Advent of Code 2016. Day 07: Internet Protocol Version 7.
// dotnet fsi aoc07.fsx

open System
open System.IO

let isAbba (s : string) = 
    (s[0] <> s[1]) && (s[0] = s[3]) && (s[1] = s[2])

let containsAbba (s : string) = 
    let rec fn (s : string) = 
        if s.Length < 4 then false
        else 
            let ss = s.Substring(0, 4)
            if isAbba ss then true 
            else 
                fn (s.Substring(1))
    fn s 

let readText fileName = 
    File.ReadAllText(fileName).Trim()

let readLines = 
    File.ReadAllLines
    >> Array.filter (fun line -> line <> String.Empty)
    >> Array.toList

let checkAbba (s : string) = 
    if containsAbba s then printfn "%s contains ABBA" s else printfn "%s does not contain ABBA" s

let checkTls (s : string) = 
    let rec check (strs : string list) (insideBrackets : bool)
    let ss = s.Split([|'['; ']'|]) |> List.ofArray
    printfn "%A" ss

let run fileName = 
    let lines = readLines fileName
    lines |> printfn "%A"
    let text = readText fileName
    text |> printfn "%s"
    checkAbba "aaaa"
    checkAbba "abba"
    checkAbba "oxox"
    checkAbba "oxxo"
    checkAbba "ooxxoo"
    checkAbba "acba"
    "emzopymywhhxulxuctj[dwwvkzhoigmbmnf]nxgbgfwqvrypqxppyq[qozsihnhpztcrpbdc]rnhnakmrdcowatw[rhvchmzmyfxlolwe]uysecbspabtauvmixa" |> checkTls

run "input"
