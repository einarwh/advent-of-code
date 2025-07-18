// Advent of Code 2017. Day 05: A Maze of Twisty Trampolines, All Alike.
// dotnet fsi aoc05.fsx

open System
open System.IO

let solve inc offs = 
  let offsets = Array.copy offs 
  let rec loop ix steps = 
    if ix < 0 || ix >= Array.length offsets then 
      steps
    else 
      let off = offsets.[ix]
      offsets.[ix] <- off + inc off
      loop (ix + off) (steps + 1)
  loop 0 0

let solve1 = solve (fun x -> 1)
let solve2 = solve (fun x -> if x > 2 then -1 else 1) 
 
let readLines = 
    File.ReadAllLines
    >> Array.filter (fun line -> line <> String.Empty)

let run fileName = 
    let lines = readLines fileName
    let offsets = lines |> Array.map int 
    offsets |> solve1 |> printfn "%d"
    offsets |> solve2 |> printfn "%d"

run "input.txt"
