// Advent of Code 2017. Day 08: I Heard You Like Registers.
// Solution for part 1 only.
// dotnet fsi aoc08.fsx

open System
open System.IO

let comparison = function 
  | "==" -> (=)
  | "!=" -> (<>)
  | ">" -> (>)
  | ">=" -> (>=)
  | "<" -> (<)
  | "<=" -> (<=)
  | x -> failwith <| sprintf "comparison %s" x

let change = function 
  | "inc" -> (+)
  | "dec" -> (-)
  | x -> failwith <| sprintf "change %s" x

let rec lookup name = function 
    | [] -> 0
    | (n, v)::t -> if n = name then v else lookup name t

let rec update n change = function 
  | [] -> [ n, change 0 ]
  | (n', v) :: t ->
    if n = n' then (n, change v) :: t
    else (n', v) :: update n change t
  
let readLine (line : string) =
  match line.Split() with 
  | [|n;op;v;_;nc;c;vc|] -> 
    let p rs = (comparison c) (lookup nc rs) (int vc) 
    fun rs ->
      if p rs then
        update n (fun x -> (change op) x (int v)) rs
      else rs
  | x -> failwith <| sprintf "line %s" line

let solve lines =
  let solver = Seq.map readLine >> Seq.reduce (>>)
  [] |> solver lines |> Seq.maxBy (fun (_, v) -> v)

let readLines = 
    File.ReadAllLines
    >> Array.filter (fun line -> line <> String.Empty)
    >> Array.toList

let run fileName = 
    let lines = readLines fileName
    solve lines |> printfn "%A"

run "input.txt"
