open System.IO

[<EntryPoint>]
let main argv =
    let lines = File.ReadAllLines argv.[0]
    let len = (Array.head lines).Length
    lines
    |> Array.mapi (fun i line -> if '#' = line.[(3 * i) % len] then 1 else 0)
    |> Array.sum
    |> printfn "%d"
    0 