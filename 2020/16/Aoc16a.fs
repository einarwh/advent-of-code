open System.IO
open System.Text.RegularExpressions

type Rule = (int * int)

let parseRuleLine (line : string) : Rule list =
    let m = Regex.Match(line, ": (\d+)-(\d+) or (\d+)-(\d+)")
    if m.Success then
        let get (ix : int) = int m.Groups.[ix].Value
        [ (get 1, get 2); (get 3, get 4) ]
    else
        []
    
let parseRuleText (text : string) : Rule list =
    text.Trim().Split('\n') |> Array.toList |> List.collect parseRuleLine
    
let checkRule (value : int)  (minValue : int, maxValue : int) : bool =
    minValue <= value && value <= maxValue
    
let checkAllRules (rules : Rule list) (value : int) : bool =
    rules
    |> List.map (checkRule value)
    |> List.reduce (||)

let parseTicketLine (line : string) : int list =
    line.Split(',') |> Array.toList |> List.map int 

let parseNearbyText (text : string) : int list =
    text.Trim().Split('\n') |> Array.toList |> List.tail |> List.collect parseTicketLine

[<EntryPoint>]
let main argv =
    let text = File.ReadAllText argv.[0]
    let parts = text.Replace("\r", "").Split("\n\n")
    let rules = parts.[0] |> parseRuleText
    let values = parts.[2] |> parseNearbyText
    values
    |> List.filter (checkAllRules rules >> not)
    |> List.sum
    |> printfn "%d" 
    0 