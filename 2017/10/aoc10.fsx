// Advent of Code 2017. Day 10: Knot Hash.
// dotnet fsi aoc10.fsx

open System
open System.IO

let rev n list = 
  let rec help n fst list = 
    if n = 0 then fst @ list
    else
      match list with 
      | [] -> failwith "not enough elements"
      | h :: t -> help (n - 1) (h :: fst) t
  help n [] list  

let rec times n fn = 
  if n > 0 then fn >> times (n - 1) fn else id
  
let rot = function
  | [] -> []
  | h::t -> t @ [h]  
  
let rotl n = times n rot

let rotr n lst = times (List.length lst - n) rot lst

let step (pos, skip, list) length = 
  let list' = list |> rotl pos |> rev length |> rotr pos
  let pos' = (pos + length + skip) % List.length list 
  let skip' = skip + 1
  (pos', skip', list')

let solve list lengths = 
  match lengths |> List.fold step (0, 0, list) with 
  | (_, _, a::b::_) -> a * b
  | _ -> failwith "ouch"
  
let readText fileName = 
    File.ReadAllText(fileName).Trim()

let run fileName = 
    let text = readText fileName
    let numbers = text.Split "," |> Array.toList |> List.map int 
    numbers |> solve [ 0 .. 255 ] |> printfn "%d"

run "input.txt"
