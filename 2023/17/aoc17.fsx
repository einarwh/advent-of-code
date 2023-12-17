// Advent of Code 2023. Day 17: The Floor Will Be Lava
// dotnet fsi aoc17.fsx

open System
open System.IO
open System.Collections.Generic

module Array2D =
    let getRowCount (ary : 'a[,]) = ary.GetLength(0)
    let getColumnCount (ary : 'a[,]) = ary.GetLength(1)

type Heat = int

type Pos = (int * int)

type Dir = N | W | S | E

type Path = {
    pos : Pos 
    dir : Dir 
    steps : int 
}

type PathWithHeat = (Path * Heat)

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

let move dir pos = 
    let (x, y) = pos
    match dir with 
    | N -> (x, y - 1)
    | W -> (x - 1, y)
    | S -> (x, y + 1)
    | E -> (x + 1, y)

let readLines : string -> string array =
    File.ReadAllLines >> Array.filter ((<>) String.Empty)

let tryMove (dir : Dir) (path, heat) (map : int[,]) (visited : Set<Path>) (queue : PriorityQueue<PathWithHeat, Heat>) =  
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
        if Set.contains next visited then 
            (visited, queue) 
        else
            let visited = visited |> Set.add next
            let h = heat + Array2D.get map y x 
            queue.Enqueue((next, h), h)
            (visited, queue)

let solve1 (map : int[,]) = 
    let yMax = (map |> Array2D.getRowCount) - 1 
    let xMax = (map |> Array2D.getColumnCount) - 1 
    let rec loop (queue : PriorityQueue<PathWithHeat, int>) visited : int option = 
        if queue.Count = 0 then None 
        else 
            let (path, heat) = queue.Dequeue()
            match path.pos with 
            | (x, y) when x = xMax && y = yMax -> 
                Some heat
            | _ -> 
                let (v, q) = 
                    if path.steps < 3 then 
                        tryMove path.dir (path, heat) map visited queue
                    else (visited, queue)
                let (v, q) = tryMove (turnLeft path.dir) (path, heat) map v q
                let (v, q) = tryMove (turnRight path.dir) (path, heat) map v q
                loop q v
                            
    let queue = PriorityQueue<PathWithHeat, Heat>()
    let visited = Set.empty 
    let path = {
        pos = (0, 0)
        dir = E 
        steps = 0
    }
    queue.Enqueue((path, 0), 0);
    match loop queue visited with 
    | Some heat -> heat 
    | None -> failwith "?"

let run fileName =
    let charAsInt ch = Char.GetNumericValue(ch) |> int
    let lines = readLines fileName |> Array.toList |> List.map (Seq.toList >> List.map charAsInt)
    let map = lines |> array2D
    solve1 map |> printfn "%A"

"input" |> run