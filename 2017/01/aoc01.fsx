// Advent of Code 2017. Day 01: Inverse Captcha.
// dotnet fsi aoc01.fsx

open System
open System.IO

let rot = function
  | [] -> []
  | h::t -> t @ [h]

let rec times n fn = 
  if n > 0 then fn >> times (n - 1) fn else id

let solve fn lst = 
  List.zip lst (fn lst)
  |> List.choose (fun (x, y) -> if x = y then Some x else None)
  |> List.sum

let solve1 = solve rot 
    
let solve2 lst = solve (times (List.length lst / 2) rot) lst
    
let readText fileName = 
    File.ReadAllText(fileName).Trim()

let run fileName = 
    let text = readText fileName
    let digits = text |> Seq.map (fun c -> int c - int '0') |> Seq.toList 
    digits |> solve1 |> printfn "%A"
    digits |> solve2 |> printfn "%A"

run "input.txt"
