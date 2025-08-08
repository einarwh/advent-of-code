// Advent of Code 2020. Day 16: Ticket Translation.
// dotnet fsi aoc16.fsx

open System.IO
open System.Text.RegularExpressions

type Name = string

type Rule = {
    name : Name
    predicate : int -> bool
}

type Candidates = Set<Name> 

type Ticket = Ticket of int list

type Field =
    | Solved of Name
    | Unsolved of Candidates
    
let checkInterval (minValue : int, maxValue : int) (value : int) : bool =
    minValue <= value && value <= maxValue

let checkValue (a, b) (c, d) value =
    checkInterval (a, b) value || checkInterval (c, d) value

let parseRuleLine (line : string) : Rule option =
    let m = Regex.Match(line, "^(.+): (\d+)-(\d+) or (\d+)-(\d+)$")
    if m.Success then
        let getStr (ix : int) = m.Groups.[ix].Value
        let getInt (ix : int) = getStr ix |> int
        let rule = {
            name = getStr 1
            predicate = checkValue (getInt 2, getInt 3) (getInt 4, getInt 5)
        }
        Some rule 
    else
        None
    
let parseRules (text : string) : Rule list =
    text.Trim().Split('\n') |> Array.toList |> List.choose parseRuleLine
    
let checkRule (value : int)  (minValue : int, maxValue : int) : bool =
    minValue <= value && value <= maxValue
    
let checkAllRules (rules : Rule list) (value : int) : bool =
    rules
    |> List.map (fun rule -> rule.predicate value)
    |> List.reduce (||)

let parseTicketLine (line : string) : Ticket =
    line.Split(',') |> Array.toList |> List.map int |> Ticket

let parseNearbyTickets (text : string) : Ticket list =
    text.Trim().Split('\n')
    |> Array.toList
    |> List.tail
    |> List.map parseTicketLine

let parseMyTicket (text : string) : Ticket =
    text.Trim().Split('\n')
    |> Array.toList
    |> List.tail
    |> List.map parseTicketLine
    |> List.head

let part1 (text : string) =
    let parts = text.Replace("\r", "").Split("\n\n")
    let rules = parts.[0] |> parseRules
    let nearbyTickets = parts.[2] |> parseNearbyTickets
    nearbyTickets
    |> List.collect (fun (Ticket t) -> t)
    |> List.filter (checkAllRules rules >> not)
    |> List.sum
    |> printfn "%d"
    
let checkTicket (rules : Rule list) (Ticket values) : Candidates list =
    let findCandidates value : Candidates =
        rules
        |> List.choose (fun rule -> if rule.predicate value then Some rule.name else None)
        |> Set.ofList
    values
    |> List.map findCandidates

let findUnsolvedSingles (fieldList : Field list) : Candidates =
    let chooser (field : Field) : Name option =
        match field with
        | Solved _ -> None
        | Unsolved cs ->
            if Set.count cs = 1 then Some (Seq.head cs)
            else None 
    fieldList |> List.choose chooser |> Set.ofList

let solve (fieldCandidatesList : Candidates list) : Name list =
    let rec loop (fieldList : Field list) : Field list =
        let mapper singles field =
            match field with
            | Solved solved -> Solved solved
            | Unsolved unsolved ->
                if Set.count unsolved = 1 then
                    Solved <| Seq.head unsolved
                else
                    Unsolved <| Set.difference unsolved singles
        let unsolvedSingles = findUnsolvedSingles fieldList
        if Set.count unsolvedSingles = 0 then
            fieldList
        else
            fieldList |> List.map (mapper unsolvedSingles) |> loop
    let solved = fieldCandidatesList |> List.map Unsolved |> loop
    solved |> List.map (fun f -> match f with | Solved name -> name | Unsolved _ -> failwith "Unsolvable.")

let departures (Ticket ticket) fields =
    List.zip fields (List.map int64 ticket)
    |> List.choose (fun (f : string, v) -> if f.StartsWith("departure") then Some v else None)
    |> List.reduce (*)

let part2 (text : string) =
    let valid rules (Ticket values) = values |> List.forall (checkAllRules rules)
    let parts = text.Replace("\r", "").Split("\n\n")
    let rules = parts.[0] |> parseRules
    let myTicket = parts.[1] |> parseMyTicket
    let fieldCount = match myTicket with Ticket t -> List.length t
    let nearbyTickets = parts.[2] |> parseNearbyTickets
    let validNearbyTickets = nearbyTickets |> List.filter (valid rules)
    let validTickets = myTicket :: validNearbyTickets
    let checkedTickets = validTickets |> List.map (checkTicket rules)
    let fields = 
        [0 .. fieldCount - 1]
        |> List.map (fun fieldNo -> checkedTickets |> List.map (List.item fieldNo) |> Set.intersectMany)
        |> solve
    departures myTicket fields |> printfn "%d"

let run fileName =
    let text = File.ReadAllText fileName
    part1 text
    part2 text

"input.txt" |> run 