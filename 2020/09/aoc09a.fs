open System.IO

let findPairs (preceding : int64 list) (num : int64) : (int64 * int64) list =
    [ for i in preceding do
          for j in preceding do
              if j > i && i + j = num then yield (i, j)]

let invalid preceding : int64 -> bool =
    findPairs preceding >> List.isEmpty
    
let update (preceding : int64 list) (num : int64): int64 list =
    (List.tail preceding) @ [num]
            
let partition (preambleLength : int) (input : int64 list) : (int64 list * int64 list) =
    let preamble = input |> List.take preambleLength
    let numbers = input |> List.skip preambleLength
    (preamble, numbers)
    
let toValues : string array -> int64 list = 
    Array.toList >> List.filter (fun (s : string) -> s.Length > 0) >> List.map int64

let rec findFirstInvalid (preceding : int64 list) (numbers : int64 list) : int64 option =
    match numbers with
    | [] -> None
    | n :: rest ->
        if invalid preceding n then
            Some n
        else 
            findFirstInvalid (update preceding n) rest
    
[<EntryPoint>]
let main argv =
    let (preamble, numbers) =
        File.ReadAllLines argv.[0]
        |> toValues
        |> partition 25     
    findFirstInvalid preamble numbers |> printfn "%A"
    0 