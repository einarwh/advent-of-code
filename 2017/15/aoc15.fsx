// Advent of Code 2017. Day 15: Dueling Generators.
// dotnet fsi aoc15.fsx

open System
open System.IO

let genseq factor = 
  let state x = Some (x, x)
  Seq.unfold (fun prev -> state <| (prev * factor) % 2147483647L)

let mask x= 
  x &&& (int64 0xFFFF)

let solve count seqA seqB =
   Seq.zip (seqA |> Seq.map mask) (seqB |> Seq.map mask) 
   |> Seq.take count 
   |> Seq.choose (fun (x, y) -> if x = y then Some (x, y) else None) 
   |> Seq.length

let solve1 a b =
   let seqA = genseq 16807L a
   let seqB = genseq 48271L b
   solve 40000000 seqA seqB

let solve2 a b =
   let filter multiple = 
      Seq.choose (fun v -> if v % multiple = 0L then Some v else None)
   let seqA = genseq 16807L a |> filter 4L
   let seqB = genseq 48271L b |> filter 8L
   solve 5000000 seqA seqB

let parseNumber (s : string) = 
    s.Split(" ") |> Array.toList |> List.rev |> List.head |> int64 

let readLines = 
    File.ReadAllLines
    >> Array.filter (fun line -> line <> String.Empty)
    >> Array.toList

let run fileName = 
    let lines = readLines fileName
    match lines |> List.map parseNumber with 
    | [genA; genB] -> 
        solve1 genA genB |> printfn "%d"
        solve2 genA genB |> printfn "%d"
    | _ -> failwith "?"

run "input.txt"
