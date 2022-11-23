open System.IO

let rec findTime (time, inc) (offset, bus) =
    if (time + offset) % bus = 0L then
        (time, inc * bus)
    else
        findTime (time + inc, inc) (offset, bus)

[<EntryPoint>]
let main argv =
    let lines = File.ReadAllLines argv.[0]
    let line = lines.[1]
    let buses =
        line.Split(',')
        |> Array.toList
        |> List.mapi (fun i b -> (i, b))
        |> List.filter (fun (_, b) -> b <> "x")
        |> List.map (fun (i, b) -> (int64 i, int64 b))
    match buses with
    | [] -> failwith "no buses"
    | (_, firstBus) :: remainingBuses ->
        remainingBuses
        |> List.fold findTime (0L, firstBus)
        |> fst
        |> printfn "%d"    
    0 