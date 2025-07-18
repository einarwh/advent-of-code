// Advent of Code 2017. Day 06: Memory Reallocation.
// dotnet fsi aoc06.fsx

open System
open System.IO

let redistribute arr =
  let m = Array.max arr
  let i = Array.findIndex (fun x -> x = m) arr
  arr.[i] <- 0
  let next i = (i + 1) % Array.length arr 
  let mutable j = i
  let mutable blocks = m
  while blocks > 0 do 
    blocks <- blocks - 1
    j <- next j
    arr.[j] <- arr.[j] + 1
  arr

let solve arr = 
  let rec loop seen ary = 
    if List.contains ary seen then 
      ary, List.length seen
    else 
      ary |> Array.copy |> redistribute |> loop (ary::seen) 
  loop [] arr

let readText fileName = 
    File.ReadAllText(fileName).Trim()

let run fileName = 
    let text = readText fileName
    let banks = text.Split "\t"  |> Array.map int
    let loopDistribution, steps = solve banks 
    steps |> printfn "%d"
    let _, cycles = solve loopDistribution
    cycles |> printfn "%d"

run "input.txt"
