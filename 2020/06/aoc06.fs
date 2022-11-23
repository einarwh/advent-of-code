open System.IO

let anyone (s : string) : int =
    s.Replace("\n", "") |> Seq.distinct |> Seq.length

let everyone (s : string) : int =
    s.Split("\n")
    |> Array.map Set.ofSeq
    |> Array.reduce Set.intersect 
    |> Set.count

let countBy (counter : string -> int) =
    Array.map counter >> Array.sum >> printfn "%d"

[<EntryPoint>]
let main argv =
    let text = File.ReadAllText argv.[0]
    let groups = text.Trim().Split("\n\n")
    groups |> countBy anyone 
    groups |> countBy everyone 
    0 