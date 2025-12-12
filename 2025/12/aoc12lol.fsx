// Advent of Code 2025. Day 12: Christmas Tree Farm.
// dotnet fsi aoc12lol.fsx

let parse (s : string) = 
    match s.Split ": " with 
    | [|a; b|] -> 
        let area = match a.Split "x" with | [|a; b|] -> int a / 3 * int b / 3 | _ -> 0
        area >= Array.sumBy int (b.Split " ")
    | _ -> false

"input.txt" |> System.IO.File.ReadAllLines |> Array.filter parse |> Array.length |> printfn "%d"
