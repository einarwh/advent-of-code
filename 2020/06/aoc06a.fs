open System.IO

[<EntryPoint>]
let main argv =
    let text = File.ReadAllText argv.[0]
    text.Split("\n\n")
    |> Array.map (fun s -> s.Replace("\n", "") |> Seq.distinct |> Seq.length)
    |> Array.sum
    |> printfn "%d"
    0 