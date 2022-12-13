// Advent of Code 2022. 
// Day 12: Monkey in the Middle.
// dotnet fsi aoc12.fsx

open System.IO
open System.Collections.Generic

type Pos = (int * int)

let findCharPos (ch : char) (lists : char list list) : Pos = 
    let rec fn (y : int) lists : Pos = 
        match lists with 
        | [] -> failwith <| sprintf "didn't find char %c" ch
        | h :: t -> 
            match List.tryFindIndex ((=) ch) h with 
            | Some x -> (x, y)
            | None -> fn (y + 1) t
    fn 0 lists

let findStartPos = findCharPos 'S'

let findEndPos = findCharPos 'E'

let listPotentialNeighbours (pos : Pos) : Pos list = 
    match pos with 
    | (x, y) -> 
        [ (x - 1, y - 1)
          (x, y - 1)
          (x + 1, y - 1)
          (x - 1, y)
          (x + 1, y)
          (x - 1, y + 1)
          (x, y + 1)
          (x + 1, y + 1) ]

let listNeighbours (matrix : char [,]) ((x, y) : Pos) : Pos list = 
    let rowCount = Array2D.length1 matrix // ys
    let colCount = Array2D.length2 matrix // xs
    let currentHeight = Array2D.get matrix y x 
    let check (x, y) = 
        if x >= 0 && x < colCount && y >= 0 && y < rowCount then 
            let targetHeight = Array2D.get matrix y x 
            let diffHeight = (int targetHeight) - (int currentHeight) 
            diffHeight <= 1
        else 
            false 
    [ (x - 1, y - 1)
      (x, y - 1)
      (x + 1, y - 1)
      (x - 1, y)
      (x + 1, y)
      (x - 1, y + 1)
      (x, y + 1)
      (x + 1, y + 1) ]
    |> List.filter check

let solve (startPos : Pos) (endPos : Pos) (lists : char list list) = 
    let matrix = array2D lists
    let rowCount = Array2D.length1 matrix // ys
    let colCount = Array2D.length2 matrix // xs
    let rec fn (steps : int) (current : Pos) (visited : Set<Pos>) (unvisited : PriorityQueue<Pos, int>) = 
        let neighbours = listNeighbours matrix current 
        let unvisitedNeighbours = 
            neighbours |> List.filter (fun n -> not <| Set.contains n visited)
        


    let posx =
        [ for y in 0 .. rowCount - 1 do
            for x in 0 .. colCount - 1 -> (x, y) ]
    posx |> printfn "%A"

    rowCount |> printfn "rows: %d"
    colCount |> printfn "cols: %d"
    startPos |> printfn "S: %A"
    endPos |> printfn "E: %A"
    matrix |> printfn "%A"

let run lists =
    let startPos = lists |> findStartPos
    let endPos = lists |> findEndPos
    solve startPos endPos lists

"sample"
|> File.ReadAllLines 
|> Array.toList 
|> List.map (Seq.toList)
|> run
