// Advent of Code 2017. Day 11: Hex Ed.
// dotnet fsi aoc11.fsx

open System
open System.IO

let collapse (nw, n, ne) = function 
  | "nw" -> (nw + 1, n, ne)
  | "n" ->  (nw, n + 1, ne)
  | "ne" -> (nw, n, ne + 1)
  | "se" -> (nw - 1, n, ne)
  | "s" ->  (nw, n - 1, ne)
  | "sw" -> (nw, n, ne - 1)
  | _ ->    (nw, n, ne)

let collapse' (nw, n, ne, se, s, sw) = function 
  | "nw" -> (nw + 1, n, ne, se, s, sw)
  | "n" ->  (nw, n + 1, ne, se, s, sw)
  | "ne" -> (nw, n, ne + 1, se, s, sw)
  | "se" -> (nw, n, ne, se + 1, s, sw)
  | "s" ->  (nw, n, ne, se, s + 1, sw)
  | "sw" -> (nw, n, ne, se, s, sw + 1)
  | _ ->    (nw, n, ne, se, s, sw)    
    
let spread (nw, n, ne) = 
  let nw' = if nw > 0 then nw else 0
  let n' = if n > 0 then n else 0 
  let ne' = if ne > 0 then ne else 0
  let se = -nw
  let s = -n
  let sw = -ne
  let se' = if se > 0 then se else 0
  let s' = if s > 0 then s else 0 
  let sw' = if sw > 0 then sw else 0 
  (nw', n', ne', se', s', sw')

let moves = ["n"; "sw"; "se"; "se"; "ne"; "n"; "se"; "sw"; "s"; "n"; "sw"; "nw"; "n"; "se"; "n" ]

// nw, ne -> n
let make_n (nw, n, ne, se, s, sw) =
  if nw > 0 && ne > 0 then 
    let n' = min nw ne
    (nw - n', n + n', ne - n', se, s, sw)
  else (nw, n, ne, se, s, sw)

// n, se -> ne
let make_ne (nw, n, ne, se, s, sw) = 
  if n > 0 && se > 0 then 
    let ne' = min n se
    (nw, n - ne', ne + ne', se - ne', s, sw)
  else (nw, n, ne, se, s, sw)

// ne, s -> se
let make_se (nw, n, ne, se, s, sw) = 
  if ne > 0 && s > 0 then 
    let se' = min ne s
    (nw, n, ne - se', se + se', s - se', sw)
  else (nw, n, ne, se, s, sw)

// se, sw -> s
let make_s (nw, n, ne, se, s, sw) = 
  if se > 0 && sw > 0 then 
    let s' = min se sw
    (nw, n, ne, se - s', s + s', sw - s')
  else (nw, n, ne, se, s, sw)

// s, nw -> sw 
let make_sw (nw, n, ne, se, s, sw) = 
  if s > 0 && nw > 0 then 
    let sw' = min s nw
    (nw - sw', n, ne, se, s - sw', sw + sw')
  else (nw, n, ne, se, s, sw)

// sw, n -> nw    
let make_nw (nw, n, ne, se, s, sw) = 
  if sw > 0 && n > 0 then 
    let nw' = min sw n
    (nw + nw', n - nw', ne, se, s, sw - nw')
  else (nw, n, ne, se, s, sw)

let round (nw0, n0, ne0, se0, s0, sw0) = 
  let (nw1, n1, ne1, se1, s1, sw1) = make_n  (nw0, n0, ne0, se0, s0, sw0)
  let (nw2, n2, ne2, se2, s2, sw2) = make_ne (nw1, n1, ne1, se1, s1, sw1)
  let (nw3, n3, ne3, se3, s3, sw3) = make_se (nw2, n2, ne2, se2, s2, sw2)
  let (nw4, n4, ne4, se4, s4, sw4) = make_s  (nw3, n3, ne3, se3, s3, sw3)
  let (nw5, n5, ne5, se5, s5, sw5) = make_sw (nw4, n4, ne4, se4, s4, sw4)
  let (nw6, n6, ne6, se6, s6, sw6) = make_nw (nw5, n5, ne5, se5, s5, sw5)
  (nw6, n6, ne6, se6, s6, sw6)

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

let solve moves = 
  let (maxDistance, (nw, n, ne, se, s, sw)) = moves |> List.fold walk (0, (0, 0, 0, 0, 0, 0)) 
  (maxDistance, nw + n + ne + se + s + sw)

let readText fileName = 
    File.ReadAllText(fileName).Trim()

let run fileName = 
    let text = readText fileName
    let moves = text.Split "," |> Array.toList 
    let (maxDistance, (nw, n, ne, se, s, sw)) = moves |> List.fold walk (0, (0, 0, 0, 0, 0, 0)) 
    nw + n + ne + se + s + sw |> printfn "%d"
    maxDistance |> printfn "%d"

run "input.txt"
