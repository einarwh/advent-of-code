// Advent of Code 2017. Day 09: Stream Processing.
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

// let rec readGroup depth cs = 
//   let rec loop c = function 
//     | '}' :: t -> c, t
//     | '{' :: t -> 
//       let (c', t') = readGroup (depth + 1) t
//       loop (c + c') t'
//     | '<' :: t -> 
//       loop c <| readGarbage t
//     | '!' :: _ :: t ->
//       loop c t
//     | _ :: t ->
//       loop c t
//   loop depth cs

let rec readGroup depth cs =
  let rec loop c g = function
    | '}' :: t -> c, g, t
    | '{' :: t ->
      let (c', g', t') = readGroup (depth + 1) t
      loop (c + c') (g + g') t'
    | '<' :: t ->
      let (g', t') = readGarbage t
      loop c (g + g') t'
    | '!' :: _ :: t ->
      loop c g t
    | _ :: t ->
      loop c g t
    | _ -> failwith "?"
  loop depth 0 cs

let read = function
  | '{' :: t ->
    readGroup 1 t |> fun (c, g, _) -> (c, g)
  | _ -> failwith "ouch"

let readText fileName =
    File.ReadAllText(fileName).Trim()

let run fileName =
    let text = readText fileName
    // text |> printfn "%s"
    let (score, garbage) = text |> Seq.toList |> read
    score |> printfn "%d"
    garbage |> printfn "%d"

run "input.txt"
