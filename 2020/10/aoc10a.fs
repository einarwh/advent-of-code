open System.IO

let read (path : string) : int array =
    File.ReadAllLines path
    |> Array.filter (fun s -> s.Length > 0)
    |> Array.map int 

let part1 (input : int array): int =
    let device = 3 + (input |> Array.max)
    let counted = 
        input
        |> Array.append [|0; device|]
        |> Array.sort
        |> Array.pairwise
        |> Array.map (fun (a, b) -> b - a)
        |> Array.countBy id
    let choose targetDiff (diff, count) =
        if diff = targetDiff then Some count else None
    let ones = counted |> Array.pick (choose 1) 
    let threes = counted |> Array.pick (choose 3)
    ones * threes

[<EntryPoint>]
let main argv =
    let input = read argv.[0]
    input |> part1 |> printfn "%d"
    0 