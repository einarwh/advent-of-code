open System.IO

let trees (lines : string array) (right : int, left : int) : int64 =
    lines
    |> Array.mapi (fun i line -> (i, line))
    |> Array.filter (fun (i, _) -> i % left = 0)
    |> Array.map (fun (i, line) -> if '#' = line.[(right * i) % line.Length] then 1L else 0L)
    |> Array.sum

let run (lines : string array) (configs : (int * int) list) =
    configs
    |> List.map (trees lines)
    |> List.reduce (*)
    |> printfn "%d"

[<EntryPoint>]
let main argv =
    let lines = File.ReadAllLines argv.[0]
    [ (3, 1) ] |> run lines
    [ (1, 1); (3, 1); (5, 1); (7, 1); (1, 2) ] |> run lines
    0 