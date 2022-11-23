open System.IO

type Adapter = int 
type Count = int64

let read (path : string) : Adapter array =
    File.ReadAllLines path
    |> Array.filter (fun s -> s.Length > 0)
    |> Array.map int 

let part1 (input : Adapter array) : int =
    let device = 3 + Array.max input
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

let tryToConnect (current : Adapter) (remaining : Adapter list) (diff : int) : (Adapter * Adapter list) option =
    match remaining |> List.skipWhile (fun a -> a < current + diff) with
    | [] -> None
    | h :: t -> if h = current + diff then Some (h, t) else None 
    
let arrangements (device : Adapter) (input : Adapter list) : Count =
    let rec arr (current : Adapter, remaining : Adapter list) (lookup : Map<Adapter, Count>) : (Count * Map<Adapter, Count>) =
        if current = device then
            (1L, Map.add device 1L lookup)
        else
            let folder (accCount : Count, map : Map<Adapter, Count>) (adapter : Adapter, rest : Adapter list) : (Count * Map<Adapter, Count>) =
                let (cnt, map') = 
                    match Map.tryFind adapter map with
                    | None ->
                        let (c, m) = arr (adapter, rest) map
                        (c, Map.add adapter c m)
                    | Some c -> (c, map)
                (accCount + cnt, map')
            [ 1 .. 3 ]
            |> List.choose (tryToConnect current remaining)
            |> List.fold folder (0L, lookup)
    arr (0, input) Map.empty |> fst 

let part2 (input : Adapter array) : Count =
    let device = 3 + Array.max input
    input
    |> Array.append [|device|]
    |> Array.sort
    |> Array.toList
    |> arrangements device 

[<EntryPoint>]
let main argv =
    let input = read argv.[0]
    input |> part1 |> printfn "Part 1: %d"
    input |> part2 |> printfn "Part 2: %d"
    0 