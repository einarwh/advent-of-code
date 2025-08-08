// Advent of Code 2020. Day 1, part B.
// dotnet fsi aoc1b.fsx

open System.IO

let read (path : string) : int array =
  path
  |> File.ReadAllLines
  |> Array.map int

let indexes (len : int) : (int * int * int) seq =
    seq {
        for a in [0 .. len - 1] do
            for b in [a + 1 .. len - 1] do
                for c in [b + 1 .. len - 1] do
                    yield (a, b, c)
    }
    
let check (values : int array) (a : int, b : int, c : int) : int option =
    let va = values.[a]
    let vb = values.[b]
    let vc = values.[c]
    if va + vb + vc = 2020 then Some (va * vb * vc) else None

let expenses = read "input.txt"
indexes (Array.length expenses)
|> Seq.choose (check expenses)
|> Seq.head
|> printfn "%d"
