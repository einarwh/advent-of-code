// Advent of Code 2017. Day 16: Permutation Promenade.
// dotnet fsi aoc16.fsx

open System
open System.IO

let rot = function
    | [] -> []
    | h::t -> t @ [h]

let rec times n fn = 
    if n > 0 then fn >> times (n - 1) fn else id

let spin n ps = times (List.length ps - n) rot ps

let rec partner a b = function 
    | [] -> []  
    | h :: t when h = a -> b :: partner a b t
    | h :: t when h = b -> a :: partner a b t
    | h :: t -> h :: partner a b t  

let exchange i j lst = 
    let (a, b) = List.item i lst, List.item j lst
    partner a b lst 

let splitdash (s : string) = 
    match s.Split([|'/'|], StringSplitOptions.None) with 
    | [|s1; s2|] -> (s1, s2)
    | _ -> failwith "?"

let readop (s : string) : char list -> char list =
    match s.[0], s.Substring(1) with 
    | 's', a -> spin (int a)
    | 'x', b -> 
        let (s1, s2) = splitdash b
        exchange (int s1) (int s2)
    | 'p', c -> 
        let (s1, s2) = splitdash c
        partner s1.[0] s2.[0]
    | _ -> failwith "?"

let readsteps filepath = 
  File.ReadAllText(filepath)
  |> (fun s -> s.Split(','))
  |> Seq.map readop

let tostr cs = new String(cs |> List.toArray)

let solve1 programs dance = 
  programs |> dance |> tostr

let solve2 n programs dance = 
  let rec loop seen input =
    let len = List.length seen 
    if len = n then input
    else 
      if seen |> List.exists (fun s -> s = input) then 
        seen |> List.item (len - (n % len) - 1)
      else 
        loop (input :: seen) (dance input)
  loop [] programs |> tostr    
    
let run fileName = 
    let dance = File.ReadAllText(fileName).Trim().Split(",") |> Seq.map readop |> Seq.reduce (>>)
    dance |> solve1 ['a' .. 'p'] |> printfn "%s"
    dance |> solve2 1000000000 ['a' .. 'p'] |> printfn "%s"

run "input.txt"
