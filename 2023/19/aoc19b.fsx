// Advent of Code 2023. Day 19: Aplenty
// dotnet fsi aoc19b.fsx

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

type Category = string

type Constraint = 
    | Over of (int * Category)
    | Under of (int * Category)

type Step = 
    | Split of (Constraint * Label)
    | Next of Label

type Workflow = {
    label : string 
    rules : Step list 
}

let parseConstraint (s : string) = 
    let parse target (s : string) = 
        let pattern = "^(\w+)(.)(\d+)$"
        let m = Regex.Match(s, pattern)
        if m.Success then
            let category = m.Groups[1].Value 
            let compStr = m.Groups[2].Value 
            let limit = m.Groups[3].Value |> int
            let c = 
                if compStr = "<" then Under (limit, category)
                else Over (limit, category) 
            Split (c, target)
        else 
            failwith <| sprintf "%s?" s
    let ss = s.Split(':')
    parse ss[1] ss[0]

let parseRule (s : string) : Step = 
    if s.Contains(':') then parseConstraint s else Next s 

let parseWorkflow (s : string) = 
    let pattern = "^(\w+)\{(.+)\}$"
    let m = Regex.Match(s, pattern)
    if m.Success then
        let label = m.Groups[1].Value 
        let rulesStr = m.Groups[2].Value 
        let strs = rulesStr.Split(",")
        let rules = strs |> Array.toList |> List.map parseRule
        (label, { label = label; rules = rules })
    else 
        failwith <| sprintf "%s?" s 

let parseWorkflows (s : string) = 
    s.Split("\n") 
    |> Array.toList 
    |> List.map parseWorkflow 
    |> Map.ofList

let parsePart (s : string) = 
    let pattern = "^\{x=(\d+),m=(\d+),a=(\d+),s=(\d+)\}$"
    let result = Regex.Match(s, pattern)
    if result.Success then
        let x = int (result.Groups[1].Value)
        let m = int (result.Groups[2].Value) 
        let a = int (result.Groups[3].Value) 
        let s = int (result.Groups[4].Value)
        { x = x; m = m; a = a; s = s }
    else 
        failwith <| sprintf "%s?" s 

let parseParts (s : string) = 
    s.Split("\n") |> Array.toList |> List.map parsePart 

let runWorkflow workflow part = 
    let rec loop steps = 
        match steps with 
        | [] -> failwith "?"
        | r :: rest ->
            match r with 
            | Cond (check, target) -> if check part then target else loop rest 
            | Goto target -> target
    loop workflow.rules

let tryWorkflows workflows = 
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
        let workflows = parseWorkflows s1
        ()
    | _ -> failwith "?" 

"input" |> run
