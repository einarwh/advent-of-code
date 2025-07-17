// Advent of Code 2016. Day 07: Internet Protocol Version 7.
// dotnet fsi aoc07.fsx

open System
open System.IO

let isAbba (s : string) = 
    (s[0] <> s[1]) && (s[0] = s[3]) && (s[1] = s[2])

let isAba (s : string) = 
    (s[0] <> s[1]) && (s[0] = s[2]) 

let containsAbba (s : string) = 
    let rec fn (s : string) = 
        if s.Length < 4 then false
        else 
            let ss = s.Substring(0, 4)
            if isAbba ss then true 
            else 
                fn (s.Substring(1))
    fn s 

let findAbas (s : string) : string list = 
    let rec fn (abas : string list) (s : string) = 
        if s.Length < 3 then abas 
        else 
            let candidate = s.Substring(0, 3)
            let rest = s.Substring(1)
            if isAba candidate then 
                fn (candidate :: abas) rest 
            else 
                fn abas rest 
    fn [] s 

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

let checkSsl (s : string) = 
    let toBab (aba : string) = new string([|aba[1];aba[0];aba[1]|])
    let ss = s.Split([|'['; ']'|]) |> List.ofArray
    let supernetSequences = 
        ss |> List.mapi (fun i x -> (i, x)) |> List.choose (fun (i, x) -> if i % 2 = 0 then Some x else None)
    let hypernetSequences = 
        ss |> List.mapi (fun i x -> (i, x)) |> List.choose (fun (i, x) -> if i % 2 = 1 then Some x else None)
    let abas = supernetSequences |> List.collect findAbas |> Set.ofList
    let babs = hypernetSequences |> List.collect findAbas |> Set.ofList
    abas |> Set.exists (fun aba -> babs |> Set.contains (toBab aba))

let run fileName = 
    let lines = readLines fileName
    lines |> List.filter checkTls |> List.length |> printfn "%d"
    lines |> List.filter checkSsl |> List.length |> printfn "%d"

run "input"
