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

type Range = {
    minimum : int 
    maximum : int
}

type Bounds = {
    xRange : Range
    mRange : Range
    aRange : Range
    sRange : Range
}

type Category = X | M | A | S 

type Comparison = GreaterThan | LessThan

type Rule = 
    | Check of (Category * Comparison * int * Label)
    | Next of Label

let parseCategory (s : string) = 
    match s with 
    | "x" -> X
    | "m" -> M
    | "a" -> A
    | "s" -> S
    | _ -> failwith <| sprintf "%s?" s

let parseCheck target (s : string) = 
    let m = Regex.Match(s, "^(\w+)(.)(\d+)$")
    if m.Success then
        let category = m.Groups[1].Value |> parseCategory
        let comparison = if m.Groups[2].Value = "<" then LessThan else GreaterThan
        let limit = m.Groups[3].Value |> int
        Check (category, comparison, limit, target)
    else 
        failwith <| sprintf "%s?" s

let parseRule (s : string) = 
    if s.Contains(':') then 
        let ss = s.Split(':')
        parseCheck ss[1] ss[0]
    else Next s 

let parseWorkflow (s : string) = 
    let m = Regex.Match(s, "^(\w+)\{(.+)\}$")
    if m.Success then
        let label = m.Groups[1].Value 
        let rules = m.Groups[2].Value.Split(",") |> Array.toList |> List.map parseRule
        (label, rules)
    else 
        failwith <| sprintf "%s?" s 

let parseWorkflows (s : string) = 
    s.Split("\n") |> Array.toList |> List.map parseWorkflow |> Map.ofList

let parsePart (s : string) = 
    let result = Regex.Match(s, "^\{x=(\d+),m=(\d+),a=(\d+),s=(\d+)\}$")
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

let runWorkflow wf part = 
    let check category comparison limit part = 
        let v = 
            match category with 
            | X -> part.x 
            | M -> part.m 
            | A -> part.a 
            | S -> part.s 
        match comparison with 
        | GreaterThan -> v > limit 
        | LessThan -> v < limit 
    let rec loop rules = 
        match rules with 
        | [] -> failwith "?"
        | r :: rest ->
            match r with 
            | Check (category, comparison, limit, target) -> 
                if check category comparison limit part then target else loop rest 
            | Next target -> target
    loop wf

let runWorkflows workflows part = 
    let rec loop current = 
        let wf = Map.find current workflows
        let next = runWorkflow wf part 
        match next with 
        | "A" -> part.x + part.m + part.a + part.s
        | "R" -> 0 
        | _ -> loop next 
    loop "in"

let part1 workflows parts = 
    parts |> List.sumBy (runWorkflows workflows) |> printfn "%d"

let combineRanges range1 range2 = 
    { minimum = max range1.minimum range2.minimum
      maximum = min range1.maximum range2.maximum }

let updateBounds category range bounds = 
    match category with 
    | X -> { bounds with xRange = combineRanges range bounds.xRange }
    | M -> { bounds with mRange = combineRanges range bounds.mRange }
    | A -> { bounds with aRange = combineRanges range bounds.aRange }
    | S -> { bounds with sRange = combineRanges range bounds.sRange }

let findBounds workflows = 
    let rec loop rules bounds = 
        match rules with 
        | [] -> failwith "?"
        | rule :: rest -> 
            match rule with 
            | Check (category, comparison, limit, target) -> 
                let (leftRange, rightRange) = 
                    match comparison with 
                    | GreaterThan -> 
                        ({ minimum = limit + 1; maximum = 4000 }, { minimum = 1; maximum = limit })
                    | LessThan -> 
                        ({ minimum = 1; maximum = limit - 1 }, { minimum = limit; maximum = 4000 })
                let leftBounds = bounds |> updateBounds category leftRange
                let leftResult = 
                    match target with 
                    | "A" -> [ leftBounds ]
                    | "R" -> []
                    | _ -> 
                        let leftSteps = Map.find target workflows 
                        loop leftSteps leftBounds 
                let rightBounds = bounds |> updateBounds category rightRange
                let rightResult = loop rest rightBounds
                leftResult @ rightResult
            | Next label -> 
                match label with 
                | "A" -> [ bounds ]
                | "R" -> [] 
                | _ -> 
                    let steps = Map.find label workflows
                    loop steps bounds 
    let initialSteps = Map.find "in" workflows
    let defaultRange = { minimum = 1; maximum = 4000 }
    let initialBounds = {
        xRange = defaultRange
        mRange = defaultRange
        aRange = defaultRange
        sRange = defaultRange
    }
    loop initialSteps initialBounds

let calculatePossibilities (bounds : Bounds) = 
    let countRange range = 
        int64 range.maximum - int64 range.minimum + 1L
    [ bounds.xRange; bounds.mRange; bounds.aRange; bounds.sRange ]
    |> List.map countRange
    |> List.reduce (*)

let part2 workflows  = 
    workflows |> findBounds |> List.sumBy calculatePossibilities |> printfn "%d"

let readChunks fileName = 
    let text = File.ReadAllText fileName 
    text.TrimEnd().Split("\n\n") |> Array.toList 

let run fileName =
    match readChunks fileName with 
    | [ workflowInput; partsInput ] -> 
        let workflows = parseWorkflows workflowInput
        let parts = parseParts partsInput
        part1 workflows parts
        part2 workflows
    | _ -> failwith "?" 

"input" |> run
