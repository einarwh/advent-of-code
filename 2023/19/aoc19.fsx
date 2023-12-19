// Advent of Code 2023. Day 19: Aplenty
// dotnet fsi aoc19.fsx

open System
open System.IO
open System.Text.RegularExpressions

type Part = {
    x : int
    m : int
    a : int
    s : int
}

type Label = string 

type Rule = 
    | Cond of (Part -> bool) * Label 
    | Goto of Label

type Workflow = {
    label : Label 
    rules : Rule list 
}

let parseSelector (s : string) : Part -> int = 
    match s with 
    | "x" -> fun (r : Part) -> r.x
    | "m" -> fun (r : Part) -> r.m
    | "a" -> fun (r : Part) -> r.a
    | "s" -> fun (r : Part) -> r.s
    | _ -> failwith <| sprintf "%s?" s

let parseCheck (target : string) (s : string) = 
    let pattern = "^(\w+)(.)(\d+)$"
    let m = Regex.Match(s, pattern)
    if m.Success then
        let selector = m.Groups[1].Value |> parseSelector
        let compStr = m.Groups[2].Value 
        let limit = m.Groups[3].Value |> int
        let check = 
            if compStr = "<" then fun r -> selector r < limit 
            else fun r -> selector r > limit 
        Cond (check, target)
    else 
        failwith <| sprintf "%s?" s

let parseCond (s : string) = 
    let ss = s.Split(':')
    parseCheck ss[1] ss[0]

let parseGoto (s : string) = 
    Goto s

let parseRule (s : string) = 
    if s.Contains(':') then parseCond s else parseGoto s 

// px{a<2006:qkq,m>2090:A,rfg}
let parseWorkflow (s : string) = 
    let pattern = "^(\w+)\{(.+)\}$"
    let m = Regex.Match(s, pattern)
    if m.Success then
        let label = m.Groups[1].Value 
        let rulesStr = m.Groups[2].Value 
        let strs = rulesStr.Split(",")
        let rules = strs |> Array.toList |> List.map parseRule
        Some (label, { label = label; rules = rules })
    else 
        None 

let parseWorkflows (s : string) : Map<Label, Workflow> = 
    s.Split("\n") 
    |> Array.toList 
    |> List.choose parseWorkflow 
    |> Map.ofList

let parsePart (s : string) : Part option= 
    let pattern = "^\{x=(\d+),m=(\d+),a=(\d+),s=(\d+)\}$"
    let result = Regex.Match(s, pattern)
    if result.Success then
        let x = int (result.Groups[1].Value)
        let m = int (result.Groups[2].Value) 
        let a = int (result.Groups[3].Value) 
        let s = int (result.Groups[4].Value)
        Some { x = x; m = m; a = a; s = s }
    else 
        None 

let parseParts (s : string) : Part list = 
    s.Split("\n") |> Array.toList |> List.choose parsePart 

let runWorkflow (workflow : Workflow) (part : Part) = 
    let rec loop rules = 
        match rules with 
        | [] -> failwith "?"
        | r :: rest ->
            match r with 
            | Cond (check, target) -> 
                if check part then target 
                else loop rest 
            | Goto target -> target
    loop workflow.rules

let runWorkflows (workflows : Map<Label, Workflow>) (part : Part) : int = 
    let rec loop current = 
        let wf = Map.find current workflows
        let next = runWorkflow wf part 
        match next with 
        | "A" -> part.x + part.m + part.a + part.s
        | "R" -> 0 
        | _ -> loop next 
    loop "in"

let readChunks fileName = 
    let text = File.ReadAllText fileName 
    text.TrimEnd().Split("\n\n") |> Array.toList 

let run fileName =
    let chunks = readChunks fileName
    match chunks with 
    | [s1; s2] -> 
        let workflows : Map<Label, Workflow> = parseWorkflows s1
        let parts : Part list = parseParts s2
        parts |> List.sumBy (runWorkflows workflows) |> printfn "%d"
    | _ -> failwith "?" 

"input" |> run
