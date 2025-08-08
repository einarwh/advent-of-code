// Advent of Code 2023. Day 17: Clumsy Crucible.
// dotnet fsi aoc17.fsx

open System
open System.IO
open System.Collections.Generic

module Array2D =
    let getRowCount (ary : 'a[,]) = ary.GetLength(0)
    let getColumnCount (ary : 'a[,]) = ary.GetLength(1)

type Dir = N | W | S | E

type Path = {
    pos : (int * int) 
    dir : Dir 
    steps : int 
}

type PQ = PriorityQueue<Path * int, int>

let turnLeft dir = 
    match dir with 
    | N -> W
    | W -> S
    | S -> E 
    | E -> N 

let turnRight dir = 
    match dir with 
    | N -> E 
    | E -> S 
    | S -> W 
    | W -> N

let move dir (x, y) = 
    match dir with 
    | N -> (x, y - 1)
    | W -> (x - 1, y)
    | S -> (x, y + 1)
    | E -> (x + 1, y)

let readLines =
    File.ReadAllLines >> Array.filter ((<>) String.Empty)

let tryMove dir (path, heat) map (visited, queue : PQ) =  
    let yMax = (map |> Array2D.getRowCount) - 1
    let xMax = (map |> Array2D.getColumnCount) - 1
    let (x, y) = move dir path.pos 
    if x < 0 || x > xMax || y < 0 || y > yMax then 
        (visited, queue) 
    else 
        let steps = if dir = path.dir then path.steps else 0
        let next = {
            pos = (x, y)
            dir = dir 
            steps = steps + 1
        }
        if visited |> Set.contains next then 
            (visited, queue) 
        else
            let h = heat + Array2D.get map y x 
            queue.Enqueue((next, h), h)
            (visited |> Set.add next, queue)

let solve minSteps maxSteps map = 
    let yMax = (map |> Array2D.getRowCount) - 1 
    let xMax = (map |> Array2D.getColumnCount) - 1 
    let rec loop (visited, queue : PQ) = 
        if queue.Count = 0 then None 
        else 
            let (path, heat) = queue.Dequeue()
            match path.pos with 
            | (x, y) when x = xMax && y = yMax && path.steps >= minSteps -> 
                Some heat
            | _ -> 
                let maybeContinue = 
                    if path.steps < maxSteps then 
                       tryMove path.dir (path, heat) map 
                    else id
                let maybeTurn = 
                    if path.steps >= minSteps then 
                        tryMove (turnLeft path.dir) (path, heat) map 
                        >> tryMove (turnRight path.dir) (path, heat) map 
                    else id
                (visited, queue)
                |> maybeContinue 
                |> maybeTurn
                |> loop
    let queue = PQ()
    let path = {
        pos = (0, 0)
        dir = E 
        steps = 0
    }
    queue.Enqueue((path, 0), 0);
    match loop (Set.empty, queue) with 
    | Some heat -> heat 
    | None -> failwith "?"

let run fileName =
    let charAsInt ch = Char.GetNumericValue(ch) |> int
    let lines = readLines fileName |> Array.map (Seq.toArray >> Array.map charAsInt)
    let map = lines |> array2D
    solve 0 3 map |> printfn "%d"
    solve 4 10 map |> printfn "%d"

"input.txt" |> run