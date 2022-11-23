open System.IO

let countGroup reducer =
    Array.map Set.ofSeq >> Array.reduce reducer >> Set.count
    
let any = countGroup Set.union

let all = countGroup Set.intersect

let count counter =
    Array.map counter >> Array.sum >> printfn "%d"

[<EntryPoint>]
let main argv =
    let text = File.ReadAllText argv.[0]
    let groups = text.Trim().Split("\n\n") |> Array.map (fun s -> s.Split("\n"))
    groups |> count any
    groups |> count all 
    0 