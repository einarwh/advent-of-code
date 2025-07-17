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

let readLines = 
    File.ReadAllLines
    >> Array.filter (fun line -> line <> String.Empty)
    >> Array.toList

let checkTls (s : string) = 
    let rec check (hypernet : bool) (abbaOutside : bool) (strs : string list) = 
        match strs with 
        | [] -> 
            abbaOutside 
        | h :: t ->
            if containsAbba h then 
                if hypernet then 
                    false 
                else 
                    check (not hypernet) true t 
            else 
                check (not hypernet) abbaOutside t 
    let ss = s.Split([|'['; ']'|]) |> List.ofArray
    check false false ss 

let run fileName = 
    let lines = readLines fileName
    lines |> List.filter checkTls |> List.length |> printfn "%d"

run "input"
