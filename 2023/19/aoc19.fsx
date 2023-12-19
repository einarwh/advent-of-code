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

type SplitInfo = {  
    category : Category
    leftRange : Range 
    rightRange : Range 
    label : Label
}

type Step = 
    | Split of SplitInfo
    | Next of Label

let parseSelector (s : string) = 
    match s with 
    | "x" -> fun r -> r.x
    | "m" -> fun r -> r.m
    | "a" -> fun r -> r.a
    | "s" -> fun r -> r.s
    | _ -> failwith <| sprintf "%s?" s

let parseCheck target (s : string) = 
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

let parseRule (s : string) = 
    if s.Contains(':') then parseCond s else Goto s 

let parseWorkflow (s : string) = 
    let pattern = "^(\w+)\{(.+)\}$"
    let m = Regex.Match(s, pattern)
    if m.Success then
        let label = m.Groups[1].Value 
        let rulesStr = m.Groups[2].Value 
        let strs = rulesStr.Split(",")
        let rules = strs |> Array.toList |> List.map parseRule
        (label, rules)
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

let runWorkflow wf part = 
    let rec loop rules = 
        match rules with 
        | [] -> failwith "?"
        | r :: rest ->
            match r with 
            | Cond (check, target) -> if check part then target else loop rest 
            | Goto target -> target
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

let part1 workflowInput partsInput = 
    let workflows = parseWorkflows workflowInput
    let parts = parseParts partsInput
    parts |> List.sumBy (runWorkflows workflows) |> printfn "%d"

let combineRanges range1 range2 = 
    { minimum = max range1.minimum range2.minimum
      maximum = min range1.maximum range2.maximum }

let updateBounds (category : Category) (range : Range) (bounds : Bounds) = 
    match category with 
    | X -> { bounds with xRange = combineRanges range bounds.xRange }
    | M -> { bounds with mRange = combineRanges range bounds.mRange }
    | A -> { bounds with aRange = combineRanges range bounds.aRange }
    | S -> { bounds with sRange = combineRanges range bounds.sRange }

let parseCategory (s : string) = 
    match s with 
    | "x" -> X
    | "m" -> M
    | "a" -> A
    | "s" -> S
    | _ -> failwith <| sprintf "%s?" s

let parseConstraint (s : string) = 
    let parse target (s : string) = 
        let pattern = "^(\w+)(.)(\d+)$"
        let m = Regex.Match(s, pattern)
        if m.Success then
            let category = m.Groups[1].Value |> parseCategory
            let compStr = m.Groups[2].Value 
            let limit = m.Groups[3].Value |> int
            let (left, right) = 
                if compStr = "<" then 
                    ({ minimum = 1; maximum = limit - 1 }, { minimum = limit; maximum = 4000 })
                else 
                    ({ minimum = limit + 1; maximum = 4000 }, { minimum = 1; maximum = limit })
            let info = {  
                category = category
                leftRange = left  
                rightRange = right  
                label = target
            }
            Split info
        else 
            failwith <| sprintf "%s?" s
    let ss = s.Split(':')
    parse ss[1] ss[0]

let parseStep (s : string) : Step = 
    if s.Contains(':') then parseConstraint s else Next s 

let parseStepWorkflow (s : string) : (Label * Step list) = 
    let pattern = "^(\w+)\{(.+)\}$"
    let m = Regex.Match(s, pattern)
    if m.Success then
        let label = m.Groups[1].Value 
        let rulesStr = m.Groups[2].Value 
        let strs = rulesStr.Split(",")
        let rules = strs |> Array.toList |> List.map parseStep
        (label, rules)
    else 
        failwith <| sprintf "%s?" s 

let parseStepsMap (s : string) : Map<Label, Step list> = 
    s.Split("\n") 
    |> Array.toList 
    |> List.map parseStepWorkflow 
    |> Map.ofList

let findBounds (stepsMap : Map<Label, Step list>) = 
    let rec loop (steps : Step list) (bounds : Bounds) = 
        match steps with 
        | [] -> failwith "?"
        | step :: rest -> 
            match step with 
            | Split info -> 
                let leftBounds = bounds |> updateBounds info.category info.leftRange
                let leftResult = 
                    match info.label with 
                    | "A" -> [ leftBounds ]
                    | "R" -> []
                    | _ -> 
                        let leftSteps = Map.find info.label stepsMap 
                        loop leftSteps leftBounds 
                let rightBounds = bounds |> updateBounds info.category info.rightRange
                let rightResult = loop rest rightBounds
                leftResult @ rightResult
            | Next label -> 
                match label with 
                | "A" -> [ bounds ]
                | "R" -> [] 
                | _ -> 
                    let steps = Map.find label stepsMap
                    loop steps bounds 
    let initialSteps = Map.find "in" stepsMap
    let defaultRange = { minimum = 1; maximum = 4000 }
    let initialBounds = {
        xRange = defaultRange
        mRange = defaultRange
        aRange = defaultRange
        sRange = defaultRange
    }
    loop initialSteps initialBounds

let calculatePossibilities (bounds : Bounds) = 
    let countRange { minimum = low; maximum = high } = 
        int64 high - int64 low + 1L
    [ bounds.xRange; bounds.mRange; bounds.aRange; bounds.sRange ]
    |> List.map countRange
    |> List.reduce (*)

let part2 workflowInput = 
    let map = parseStepsMap workflowInput
    let boundsList = findBounds map
    boundsList |> List.sumBy calculatePossibilities |> printfn "%d"

let readChunks fileName = 
    let text = File.ReadAllText fileName 
    text.TrimEnd().Split("\n\n") |> Array.toList 

let run fileName =
    let chunks = readChunks fileName
    match chunks with 
    | [workflowInput; partsInput] -> 
        part1 workflowInput partsInput
        part2 workflowInput
    | _ -> failwith "?" 

"input" |> run
