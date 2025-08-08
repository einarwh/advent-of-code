// Advent of Code 2020. Day 1, part A.
// dotnet fsi aoc1a.fsx

open System
open System.IO

type Tree = 
  | Empty
  | Node of (int * Tree * Tree)

let rec insert value tree =
  match tree with
  | Empty -> Node (value, Empty, Empty)
  | Node (v, left, right) when value < v ->
    Node (v, insert value left, right)
  | Node (v, left, right) when value > v ->
    Node (v, left, insert value right)
  | _ -> tree

let rec contains value tree =
  match tree with
  | Empty -> false
  | Node (v, _, _) when value = v -> true
  | Node (v, left, _) when value < v -> contains value left  
  | Node (v, _, right) when value > v -> contains value right
  | _ -> false
  
let read path =
  path
  |> File.ReadAllLines
  |> Array.map (fun s -> Int32.Parse(s))
  
let createTree (values : int array) : Tree =
  let rec grow (index : int) (t : Tree) : Tree =
    if index < values.Length then
      let v = values.[index]
      t |> insert v |> grow (index + 1) 
    else
      t
  Empty |> grow 0 

let values = read "input.txt"
let tree = createTree values
let sortedValues = values |> Array.sort
let value = sortedValues |> Array.find (fun v -> (contains (2020 - v) tree))
printfn "%d" (value * (2020 - value))
