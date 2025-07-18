// Advent of Code 2017. Day 09: Stream Processing.
// Solution for part 2 only.
// dotnet fsi aoc09.fsx

open System
open System.IO

let readGarbage cs =
  let rec loop g = function
  | '>' :: t -> (g, t)
  | '!' :: _ :: t ->
    loop g t
  | _ :: t -> loop (g + 1) t
  | _ -> failwith "?"
  loop 0 cs

let rec readGroup cs =
  let rec loop g = function
    | '}' :: t -> g, t
    | '{' :: t ->
      let (g', t') = readGroup t
      loop (g + g') t'
    | '<' :: t ->
      let (g', t') = readGarbage t
      loop (g + g') t'
    | '!' :: _ :: t ->
      loop g t
    | _ :: t ->
      loop g t
    | _ -> failwith "?"
  loop 0 cs

let read = function
  | '{' :: t ->
    readGroup t |> fun (g, _) -> g
  | _ -> failwith "ouch"

// let count (s : string) =
//   s |> Seq.toList |> read

// let text = File.ReadAllText("C:/einarwh/temp/day9.txt")

// text |> count |> printfn "%A"


let readText fileName =
    File.ReadAllText(fileName).Trim()

let run fileName =
    let text = readText fileName
    // text |> printfn "%s"
    text |> Seq.toList |> read |> printfn "%A"

run "input.txt"
