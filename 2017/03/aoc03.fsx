// Advent of Code 2017. Day 03: Spiral Memory.
// dotnet fsi aoc03.fsx

open System
open System.IO

let circle n = 
  Seq.initInfinite (fun i -> 2 * i + 1) 
  |> Seq.findIndex (fun x -> n <= x * x)  
  
let steps n =
  match circle n with 
  | 0 -> 0
  | c -> 
    let d = 2 * c
    let x = (n + c - 1) % d
    c + if x > c then d - x else x

let neighbours (x, y) = 
  let neighbourOffsets = 
    [(-1,-1); (0,-1); (1,-1); (1,0); (1,1); (0,1); (-1,1); (-1,0)]
  let result = 
    neighbourOffsets 
    |> List.map (fun (x', y') -> (x + x', y + y')) 
  result  

let pos2circle (x, y) = 
  max (abs x) (abs y)

let num2circle n = 
  Seq.initInfinite (fun i -> 2 * i + 1) 
  |> Seq.map (fun x -> x * x)
  |> Seq.findIndex (fun v -> n <= v)  

let gencircle n = 
  if n = 0 then [(0, 0)]
  else 
    let lst = [ -n .. n ]
    let lst1 = lst |> List.map (fun y -> (n, y)) |> List.tail 
    let lst2 = lst |> List.rev |> List.map (fun x -> (x, n)) |> List.tail 
    let lst3 = lst |> List.rev |> List.map (fun y -> (-n, y)) |> List.tail 
    let lst4 = lst |> List.map (fun x -> (x, -n)) |> List.tail
    lst1 @ lst2 @ lst3 @ lst4
  
let lastelement c = 
  let x = 2 * c + 1
  x * x

let getnumber pos =
  let c = pos2circle pos
  if c = 0 then 1
  else 
    let start = lastelement (c - 1)
    let circ = gencircle c 
    let index = circ |> List.findIndex ((=) pos)
    start + index + 1

let getpos n = 
  let c = num2circle n
  let start = lastelement (c - 1)
  let circ = gencircle c 
  let index = n - start - 1
  List.item index circ
  
let friends n = 
  n
  |> getpos
  |> neighbours
  |> List.map getnumber
  |> List.filter (fun n' -> n' < n)

let solve maxsum = 
    let rec loop fn n = 
        let fs = friends n
        let sum = fs |> List.map (fun f -> fn f) |> List.sum
        if sum > maxsum then 
            sum
        else 
            let fn' = fun x -> if x = n then sum else fn x
            loop fn' (n + 1)
    loop (fun x -> if x = 1 then 1 else 0) 2

let readText fileName = 
    File.ReadAllText(fileName).Trim()

let run fileName = 
    let text = readText fileName
    let input = int text 
    input |> steps |> printfn "%d"
    input |> solve |> printfn "%d" 

run "input.txt"
