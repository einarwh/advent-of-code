// Advent of Code 2024. Day 18: RAM Run.
// dotnet fsi aoc18.fsx

open System
open System.IO
open System.Collections.Generic

type Pos = (int*int)

type Distance = int

type PQ = PriorityQueue<Pos * Distance, Distance>

let trim (input : string) = input.Trim()

let split (splitter : string) (input : string) = input.Split(splitter)

let join (sep : string) (seq : string seq) = String.Join(sep, seq)

let charsToString (chars : char array) = new String(chars)

let parsePos (s : string) =
    match s |> split "," |> Array.map int with
    | [|a; b|] -> Some (a, b)
    | _ -> None

let readLines =
    File.ReadAllLines
    >> Array.filter (fun line -> line <> String.Empty)
    >> Array.toList

let getAllPositions dim =
    let rowCount = dim
    let colCount = dim
    [for x in [0..colCount-1] do for y in [0..rowCount-1] -> (x, y)]

let visualize dim corrupted =
    let range = [ 0 .. dim ]
    let createRow y =
        range
        |> List.map (fun x -> if Set.contains (x, y) corrupted then '#' else '.')
        |> List.toArray
        |> charsToString
    range
    |> List.map (createRow)
    |> join "\n"
    |> printfn "%s"

let getNeighbours (x, y) =
    [ (x, y - 1)
      (x - 1, y)
      (x, y + 1)
      (x + 1, y) ]

let solve dim corrupted =
    let startPos = (0, 0)
    let endPos = (dim, dim)
    let rec loop (visited, q : PQ) =
        // printfn "loop - visited %d - queue %d" (Set.count visited) (q.Count)
        if q.Count = 0 then None
        else
            let (pos, distance) = q.Dequeue()
            if pos = endPos then Some distance
            else
                let next =
                    getNeighbours pos
                    |> List.filter (fun (x, y) -> x >= 0 && x <= dim && y >= 0 && y <= dim)
                    |> List.filter (fun p -> not (Set.contains p visited))
                    |> List.filter (fun p -> not (Set.contains p corrupted))
                next |> List.iter (fun p -> q.Enqueue((p, distance + 1), distance + 1))
                let nextVisited = next |> Set.ofList |> Set.union visited
                loop (nextVisited, q)
    let queue = PQ()
    queue.Enqueue((startPos, 0), 0)
    loop (Set.empty |> Set.add startPos, queue)

let solve1 dim byteCount corrupted = 
    corrupted 
    |> List.take byteCount 
    |> Set.ofList 
    |> solve dim 
    |> Option.get 
    |> printfn "%d"

// let findBlocker dim0 

let run dim byteCount fileName =
    let lines = readLines fileName
    let corrupted = lines |> List.choose parsePos 
    // corrupted |> printfn "%A"
    // visualize dim corrupted
    solve1 dim byteCount corrupted

// run 6 12 "sample"
run 70 1024 "input"
