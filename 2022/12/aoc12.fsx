// Advent of Code 2022. 
// Day 12: Hill Climbing Algorithm.
// dotnet fsi aoc12.fsx

open System.IO

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

let findCharPositions (ch : char) (lists : char list list) : Pos list = 
    let fn (y : int) (chars : char list) : Pos list = 
        chars 
        |> List.mapi (fun ix c -> if c = ch then Some (ix, y) else None)
        |> List.choose id
    lists 
    |> List.mapi (fun iy lst -> fn iy lst)
    |> List.concat

let toElevation (ch : char) : char = 
    match ch with 
    | 'S' -> 'a'
    | 'E' -> 'z'
    | _ -> ch

let listLegalNeighbours (matrix : char [,]) ((x, y) : Pos) : Pos list = 
    let rowCount = Array2D.length1 matrix // ys
    let colCount = Array2D.length2 matrix // xs
    let currentHeight = Array2D.get matrix y x |> toElevation
    let check (x, y) = 
        if x >= 0 && x < colCount && y >= 0 && y < rowCount then 
            let targetHeight = Array2D.get matrix y x |> toElevation
            let diffHeight = (int targetHeight) - (int currentHeight) 
            let result = diffHeight <= 1
            result
        else 
            false 
    let result = 
        [ (x, y - 1)
          (x - 1, y)
          (x + 1, y)
          (x, y + 1) ]
        |> List.filter check
    result 

let solve (startPos : Pos) (endPos : Pos) (lists : char list list) = 
    let matrix = array2D lists
    let rec fn (distance : int) (positions : Set<Pos>) (visited : Set<Pos>) = 
        if Set.contains endPos visited then 
            Some (distance - 1)
        else if Set.count positions = 0 then 
            None
        else 
            let nextPositions = 
                positions
                |> Seq.collect (fun pos -> listLegalNeighbours matrix pos)
                |> Set.ofSeq 
                |> Set.filter (fun pos -> not <| Set.contains pos visited)
            fn (distance + 1) nextPositions (Set.union visited positions)
    let initSet = Set.empty |> Set.add startPos 
    fn 0 initSet Set.empty

let part1 lists = 
    let startPos = lists |> findStartPos
    let endPos = lists |> findEndPos
    match solve startPos endPos lists with 
    | Some distance -> distance |> printfn "Distance: %A"
    | None -> printfn "Failed to find a solution."

let part2 lists = 
    let startPos = lists |> findStartPos
    let aPositions = lists |> findCharPositions 'a'
    let startPositions = startPos :: aPositions
    let endPos = lists |> findEndPos
    let distances = 
        startPositions
        |> List.choose (fun p -> solve p endPos lists)
    distances |> List.min |> printfn "Distance: %A"

let run lists =
    lists |> part1 
    lists |> part2

"input"
|> File.ReadAllLines 
|> Array.toList 
|> List.map (Seq.toList)
|> run
