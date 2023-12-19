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

type Rule = 
    | Cond of (Part -> bool) * string 
    | Goto of string

type Workflow = {
    label : string 
    categorize : Part -> string 
}

let parseSelector (s : string) = 
    match s with 
    | "x" -> fun r -> r.x
    | "m" -> fun r -> r.m
    | "a" -> fun r -> r.a
    | "s" -> fun r -> r.s
    | _ -> failwith <| sprintf "%s?" s

let parseCheck target (s : string) : Part -> (Part -> string) -> string = 
    let pattern = "^(\w+)(.)(\d+)$"
    let m = Regex.Match(s, pattern)
    if m.Success then
        let selector = m.Groups[1].Value |> parseSelector
        let compStr = m.Groups[2].Value 
        let limit = m.Groups[3].Value |> int
        if compStr = "<" then 
            fun r n -> if selector r < limit then target else fn r
        else 
            fun r n -> if selector r > limit then target else fn r
    else 
        failwith <| sprintf "%s?" s

let parseCond (s : string) : (Part -> string) -> Part -> string = 
    let ss = s.Split(':')
    parseCheck ss[1] ss[0]

let parseGoto (s : string) : Part -> (Part -> string) -> string = 
    fun _ _ -> s

let parseFunction (s : string) = 
    if s.Contains(':') then parseCond s else Goto s 

let parseWorkflow (s : string) : Part -> string = 
    let pattern = "^(\w+)\{(.+)\}$"
    let m = Regex.Match(s, pattern)
    if m.Success then
        let label = m.Groups[1].Value 
        let rulesStr = m.Groups[2].Value 
        let strs = rulesStr.Split(",")
        let fns = strs |> Array.toList |> List.map parseRule
        let folder = 
        let fn = List.foldBack folder fns id 
        (label, { label = label; categorize = rules })
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
    let rec loop rules = 
        match rules with 
        | [] -> failwith "?"
        | r :: rest ->
            match r with 
            | Cond (check, target) -> if check part then target else loop rest 
            | Goto target -> target
    loop workflow.rules

let runWorkflows workflows part = 
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
        let parts = parseParts s2
        parts |> List.sumBy (runWorkflows workflows) |> printfn "%d"
    | _ -> failwith "?" 

"input" |> run
