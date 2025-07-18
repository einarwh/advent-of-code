// Advent of Code 2017. Day 17: Spinlock.
// dotnet fsi aoc17.fsx

open System
open System.IO

let find y buf = 
  let ix = List.findIndex ((=) y) buf
  let ix' = (1 + ix) % List.length buf
  List.item ix' buf

let insert v n lst =
  let rec loop n acc lst =
    if n = 0 then List.rev acc @ (v :: lst)
    else
      match lst with 
      | [] -> failwith "fell off!"
      | h :: t ->
        loop (n - 1) (h :: acc) t
  loop n [] lst

let rec run1 step current v maxval buffer = 
  if v < maxval then 
    let v' = v + 1
    let pos = (current + step) % List.length buffer
    let buffer' = insert v' (1 + pos) buffer
    run1 step (1 + pos) v' maxval buffer'
  else
    buffer |> find maxval

let rec run2 step current v maxval prev = 
  if v < maxval then 
    let v' = v + 1
    let pos = (current + step) % v'
    let prev' = if pos = 0 then v' else prev
    run2 step (1 + pos) v' maxval prev'
  else
    prev
    
let run fileName = 
    let steps = File.ReadAllText(fileName).Trim() |> int 
    run1 steps 0 0 2017 [0]  |> printfn "%d"
    run2 steps 0 0 50000000 0 |> printfn "%d"

run "input.txt"
