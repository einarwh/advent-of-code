open System.IO

let lines = "input" |> File.ReadAllLines
let len = (Array.head lines).Length

lines
|> Array.mapi (fun i line -> if '#' = line.[(3 * i) % len] then 1 else 0)
|> Array.sum
|> printfn "%d"
