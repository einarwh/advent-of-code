// Advent of Code 2017. Day 11: Hex Ed.
// dotnet fsi aoc11.fsx

open System
open System.IO

let walk (d, (nw, n, ne, se, s, sw)) = function 
  | "nw" ->
    if se > 0 then // opposite 
      (d, (nw, n, ne, se - 1, s, sw))
    else if ne > 0 then // clockwise
      (d, (nw, n + 1, ne - 1, se, s, sw))
    else if s > 0 then // counterclockwise
      (d, (nw, n, ne, se, s - 1, sw + 1))
    else 
      let d' = max d (nw + n + ne + se + s + sw + 1)
      (d', (nw + 1, n, ne, se, s, sw)) 
  | "n" ->
    if s > 0 then // opposite 
      (d, (nw, n, ne, se, s - 1, sw))
    else if se > 0 then // clockwise
      (d, (nw, n, ne + 1, se - 1, s, sw))
    else if sw > 0 then // counterclockwise
      (d, (nw + 1, n, ne, se, s, sw - 1))
    else 
      let d' = max d (nw + n + ne + se + s + sw + 1)
      (d', (nw, n + 1, ne, se, s, sw)) 
  | "ne" ->
    if sw > 0 then // opposite 
      (d, (nw, n, ne, se, s, sw - 1))
    else if s > 0 then // clockwise
      (d, (nw, n, ne, se + 1, s - 1, sw))
    else if nw > 0 then // counterclockwise
      (d, (nw - 1, n + 1, ne, se, s, sw))
    else 
      let d' = max d (nw + n + ne + se + s + sw + 1)
      (d', (nw, n, ne + 1, se, s, sw)) 
  | "se" ->
    if nw > 0 then // opposite 
      (d, (nw - 1, n, ne, se, s, sw))
    else if sw > 0 then // clockwise
      (d, (nw, n, ne, se, s + 1, sw - 1))
    else if n > 0 then // counterclockwise
      (d, (nw, n - 1, ne + 1, se, s, sw))
    else 
      let d' = max d (nw + n + ne + se + s + sw + 1)
      (d', (nw, n, ne, se + 1, s, sw)) 
  | "s" ->
    if n > 0 then // opposite 
      (d, (nw, n - 1, ne, se, s, sw))
    else if nw > 0 then // clockwise
      (d, (nw - 1, n, ne, se, s, sw + 1))
    else if ne > 0 then // counterclockwise
      (d, (nw, n, ne - 1, se + 1, s, sw))
    else 
      let d' = max d (nw + n + ne + se + s + sw + 1)
      (d', (nw, n, ne, se, s + 1, sw)) 
  | "sw" ->
    if ne > 0 then // opposite 
      (d, (nw, n, ne - 1, se, s, sw))
    else if n > 0 then // clockwise
      (d, (nw + 1, n - 1, ne, se, s, sw))
    else if se > 0 then // counterclockwise
      (d, (nw, n, ne, se - 1, s + 1, sw))
    else 
      let d' = max d (nw + n + ne + se + s + sw + 1)
      (d', (nw, n, ne, se, s, sw + 1)) 
  | _ -> failwith "?"

let run fileName = 
    let text = File.ReadAllText(fileName).Trim()
    let moves = text.Split "," |> Array.toList 
    let maxDistance, (nw, n, ne, se, s, sw) = moves |> List.fold walk (0, (0, 0, 0, 0, 0, 0)) 
    nw + n + ne + se + s + sw |> printfn "%d"
    maxDistance |> printfn "%d"

run "input.txt"
