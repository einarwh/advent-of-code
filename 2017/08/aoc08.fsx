// Advent of Code 2017. Day 08: I Heard You Like Registers.
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
    | (n, v, _)::t -> if n = name then v else lookup name t

let rec update n change = function 
  | [] ->
    let v = change 0 
    [ n, v, v ]
  | (n', v, mv) :: t ->
    if n = n' then 
        let v' = change v 
        let mv' = max v' mv 
        (n, change v, mv') :: t
    else (n', v, mv) :: update n change t
  
let readLine (line : string) =
  match line.Split() with 
  | [|n;op;v;_;nc;c;vc|] -> 
    let p (rs : (string*int*int) list) = (comparison c) (lookup nc rs) (int vc) 
    fun (rs : (string*int*int) list) ->
      if p rs then
        update n (fun x -> (change op) x (int v)) rs
      else rs
  | x -> failwith <| sprintf "line %s" line

let readLines = 
    File.ReadAllLines
    >> Array.filter (fun line -> line <> String.Empty)
    >> Array.toList

let run fileName = 
    let lines = readLines fileName
    let solver = Seq.map readLine >> Seq.reduce (>>)
    let registry = [] |> solver lines
    registry |> Seq.map (fun (_, v, _) -> v) |> Seq.max |> printfn "%d"
    registry |> Seq.map (fun (_, _, mv) -> mv) |> Seq.max |> printfn "%d"

run "input.txt"
