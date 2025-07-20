// Advent of Code 2015. Day 12
// dotnet fsi aoc12.fsx

open System
open System.IO
open System.Text.Json

let sumNumbers (skipRed : bool) (element : JsonElement) : int = 
    let isRed (e : JsonElement) : bool = 
        e.ValueKind = JsonValueKind.String && e.GetString() = "red"
    let rec sum (e : JsonElement) = 
        match e.ValueKind with 
        | JsonValueKind.Object -> 
            let elements = e.EnumerateObject() |> Seq.map (fun prop -> e.GetProperty(prop.Name))
            if skipRed && elements |> Seq.exists isRed then 0 
            else
                elements |> Seq.sumBy sum 
        | JsonValueKind.Array -> 
            e.EnumerateArray() |> Seq.sumBy sum
        | JsonValueKind.Number -> 
            e.GetInt32()
        | _ -> 
            0
    sum element

let run fileName = 
    let text = File.ReadAllText(fileName).Trim()
    let jsonDoc = JsonDocument.Parse(text)
    let rootElement = jsonDoc.RootElement
    rootElement |> sumNumbers false |> printfn "%d"
    rootElement |> sumNumbers true |> printfn "%d"

run "input.txt"
