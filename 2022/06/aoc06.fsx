
open System.IO

let isMarker count cs = 
    count = (cs |> Set.ofSeq |> Set.count)

let findMarker count = 
    Seq.windowed count 
    >> Seq.mapi (fun i cs -> if isMarker count cs then Some (i + count) else None)
    >> Seq.choose id
    >> Seq.head 

let run cs = 
    cs |> findMarker 4 |> printfn "%d"
    cs |> findMarker 14 |> printfn "%d"

"input"
|> File.ReadAllText
|> run
