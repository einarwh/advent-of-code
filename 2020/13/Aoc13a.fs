open System.IO

[<EntryPoint>]
let main argv =
    let lines = File.ReadAllLines argv.[0]
    let timestamp = int lines.[0]
    lines.[1].Split(',')
    |> Array.toList
    |> List.filter (fun s -> s <> "x")
    |> List.map int
    |> List.map (fun b -> (b, b - timestamp % b))
    |> List.minBy snd
    |> (fun (bus, minutes) -> bus*minutes)
    |> printfn "%d"
    0 