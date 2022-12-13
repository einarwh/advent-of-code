// Advent of Code 2022. 
// Day 12: Monkey in the Middle.
// dotnet fsi aoc12.fsx

#r "nuget: FSharpx.Collections"

open System.IO
open FSharpx.Collections

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

let run lists =
    lists |> findStartPos |> printfn "%A"
    lists |> findEndPos |> printfn "%A"
    let matrix = array2D lists

    matrix |> printfn "%A"

"sample"
|> File.ReadAllLines 
|> Array.toList 
|> List.map (Seq.toList)
|> run
