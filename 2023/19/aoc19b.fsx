// Advent of Code 2023. Day 19: Aplenty
// dotnet fsi aoc19b.fsx

open System
open System.IO
open System.Text.RegularExpressions

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

type Label = string

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

type Workflow = Step list 

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

let readChunks fileName = 
    let text = File.ReadAllText fileName 
    text.TrimEnd().Split("\n\n") |> Array.toList 

let calculatePossibilities (bounds : Bounds) = 
    let countRange { minimum = low; maximum = high } = 
        int64 high - int64 low + 1L
    [ bounds.xRange; bounds.mRange; bounds.aRange; bounds.sRange ]
    |> List.map countRange
    |> List.reduce (*)

let run fileName =
    let chunks = readChunks fileName
    match chunks with 
    | [s1; s2] -> 
        let map = parseStepsMap s1
        let boundsList = findBounds map
        boundsList |> List.sumBy calculatePossibilities |> printfn "%d"
    | _ -> failwith "?" 

"input" |> run
