// Advent of Code 2024. Day 12: Garden Groups.
// dotnet fsi aoc12plant.fsx

open System
open System.IO
open System.Diagnostics

module Garden = 
    let inBounds (a : 'a[,]) (x, y) = 
        let first = y >= 0 && y < a.GetLength(0)
        let second = x >= 0 && x < a.GetLength(1)
        first && second
    let tryGet (a : 'a[,]) (x, y) = 
        if inBounds a (x, y) then Some (Array2D.get a y x) else None
    let get (a : 'a[,]) (x, y) = 
        Array2D.get a y x
    let positions (a : 'a[,]) = 
        let rowCount = a.GetLength(0)
        let colCount = a.GetLength(1)
        [for x in [0..colCount-1] do for y in [0..rowCount-1] -> (x, y)]
    let fromList (lst : 'a list list) = 
        let width = lst |> List.head |> List.length 
        let height = lst |> List.length 
        Array2D.init height width (fun y x -> lst |> List.item y |> List.item x)

type Pos = (int * int)

type Side = { startPos : Pos; endPos : Pos }

type Border = Enter of Side | Exit of Side 

let readLines = 
    File.ReadAllLines
    >> Array.filter (fun line -> line <> String.Empty)
    >> Array.toList

let rec fill garden ch (x, y) plot = 
    if (x, y) |> Garden.inBounds garden then 
        if Set.contains (x, y) plot then plot 
        else 
            if ch = Garden.get garden (x, y) then 
                plot 
                |> Set.add (x, y)
                |> fill garden ch ((x - 1), y) 
                |> fill garden ch ((x + 1), y) 
                |> fill garden ch (x, (y - 1)) 
                |> fill garden ch (x, (y + 1))
            else plot 
    else plot 

let fillPlot garden (x, y)  = 
    let ch = Garden.get garden (x, y)
    fill garden ch (x, y) Set.empty

let findPlots garden =
    let rec loop startPositions plots visited = 
        match startPositions with 
        | [] -> plots 
        | pos :: remaining -> 
            if Set.contains pos visited then 
                loop remaining plots visited 
            else 
                let plot = fillPlot garden pos 
                loop remaining (plot :: plots) (Set.union visited plot)
    let startPositions = Garden.positions garden 
    loop startPositions [] Set.empty

let neighbours (x, y) = 
  [ ((x - 1), y); ((x + 1), y); (x, (y - 1)); (x, (y + 1)) ]

let rec fillPlant (possiblePositionsLeft : Set<Pos>) (positionsToAdd : Set<Pos>) (plot : Set<Pos>) = 
    if Set.count positionsToAdd = 0 then 
        (plot, possiblePositionsLeft)
    else 
        let nextPlot = Set.union plot positionsToAdd
        let nextPositionsToAdd = 
            positionsToAdd
            |> Set.toList  
            |> List.collect neighbours 
            |> Set.ofList 
            |> Set.filter (fun p -> (Set.contains p possiblePositionsLeft))
        let possible = Set.difference possiblePositionsLeft nextPositionsToAdd
        fillPlant possible nextPositionsToAdd nextPlot 

let fillPlantPlot (plantPositions : Set<Pos>) startPos = 
    let positionsToAdd = Set.empty |> Set.add startPos 
    fillPlant plantPositions positionsToAdd Set.empty

let findPlantPlots plant (positions : Pos list)  =
    printfn "findPlantPlots: %A (%d plants)" plant (List.length positions)
    let rec loop posList plots = 
        match posList with 
        | [] -> plots 
        | pos :: remaining -> 
            let remSet = Set.ofList remaining
            let (plot, updatedRemSet) = fillPlantPlot remSet pos 
            let rem = updatedRemSet |> Set.toList
            loop rem (plot :: plots)
    loop positions []

let findAllPlots garden = 
    let positionsWithPlant = garden |> Garden.positions |> List.map (fun pos -> (pos, Garden.get garden pos))
    let selectByPlant p = 
        positionsWithPlant |> List.choose (fun (pos, plant) -> if plant = p then Some pos else None)
    let getPlots plant = 
        plant |> selectByPlant |> findPlantPlots plant
    let aPlots = getPlots 'A'
    let bPlots = getPlots 'B'
    let cPlots = getPlots 'C'
    let dPlots = getPlots 'D'
    let ePlots = getPlots 'E'
    let fPlots = getPlots 'F'
    let gPlots = getPlots 'G'
    let hPlots = getPlots 'H'
    let iPlots = getPlots 'I'
    let jPlots = getPlots 'J'
    let kPlots = getPlots 'K'
    let lPlots = getPlots 'L'
    let mPlots = getPlots 'M'
    let nPlots = getPlots 'N'
    let oPlots = getPlots 'O'
    let pPlots = getPlots 'P'
    let qPlots = getPlots 'Q'
    let rPlots = getPlots 'R'
    let sPlots = getPlots 'S'
    let tPlots = getPlots 'T'
    let uPlots = getPlots 'U'
    let vPlots = getPlots 'V'
    let wPlots = getPlots 'W'
    let xPlots = getPlots 'X'
    let yPlots = getPlots 'Y'
    let zPlots = getPlots 'Z'
    let plots = [ aPlots; bPlots; cPlots; dPlots; ePlots; fPlots; gPlots; hPlots; iPlots; jPlots; kPlots; lPlots; mPlots; nPlots; oPlots; pPlots; qPlots; rPlots; sPlots; tPlots; uPlots; vPlots; wPlots; xPlots; yPlots; zPlots; ] |> List.concat
    plots 

let calculateArea plot = Set.count plot 

let countBorders garden (x, y) = 
    let isBorder plotPlant pos = 
        match Garden.tryGet garden pos with 
        | None -> true 
        | Some plant -> plant <> plotPlant
    let plotPlant = Garden.get garden (x, y)
    [ (x, y-1); (x-1, y); (x, y+1); (x+1, y) ]
    |> List.filter (isBorder plotPlant)
    |> List.length 

let rec combine borders = 
    match borders with 
    | [] -> []
    | first :: rest -> 
        let fitSide firstSide side = 
            side.startPos = firstSide.endPos || firstSide.startPos = side.endPos
        let fit first border = 
            match (first, border) with 
            | (Enter firstSide, Enter side) -> fitSide firstSide side 
            | (Exit firstSide, Exit side) -> fitSide firstSide side 
            | _ -> false
        match rest |> List.tryFind (fit first) with 
        | None -> 
            first :: combine rest 
        | Some border -> 
            let filtered = rest |> List.filter ((<>) border)
            let combineSides side1 side2 = 
                if side2.startPos = side1.endPos then 
                    { startPos = side1.startPos; endPos = side2.endPos }
                else 
                    { startPos = side2.startPos; endPos = side1.endPos }
            let combined = 
                match (first, border) with 
                | (Enter firstSide, Enter side) -> 
                    Enter (combineSides firstSide side)
                | (Exit firstSide, Exit side) -> 
                    Exit (combineSides firstSide side)
                | _ -> failwith "?"
            combine (combined :: filtered)

let getBordersForPlot garden plot = 
    let checkBorder before after endPos = 
        match (plot |> Set.contains before, plot |> Set.contains after) with 
        | false, true -> Some <| Enter ({ startPos = after; endPos = endPos })
        | true, false -> Some <| Exit ({ startPos = after; endPos = endPos })
        | _ -> None 
    let findVerticalBorders plot y0 = 
        let xs = plot |> Set.filter (fun (x, y) -> y = y0) |> Set.map fst
        let xMin = xs |> Set.minElement
        let xMax = xs |> Set.maxElement
        let xRange = [ (xMin - 1) .. (xMax + 1) ]
        xRange |> List.pairwise |> List.choose (fun (x1, x2) -> checkBorder (x1, y0) (x2, y0) (x2, y0 + 1))
    let findHorizontalBorders plot x0 = 
        let ys = plot |> Set.filter (fun (x, y) -> x = x0) |> Set.map snd
        let yMin = ys |> Set.minElement
        let yMax = ys |> Set.maxElement
        let yRange = [ (yMin - 1) .. (yMax + 1) ]
        yRange |> List.pairwise |> List.choose (fun (y1, y2) -> checkBorder (x0, y1) (x0, y2) (x0+1, y2))
    let xs = plot |> Set.map fst
    let ys = plot |> Set.map snd
    let xMin = xs |> Set.minElement
    let xMax = xs |> Set.maxElement
    let yMin = ys |> Set.minElement
    let yMax = ys |> Set.maxElement
    let verticalBorders = [ yMin .. yMax ] |> List.collect (findVerticalBorders plot) |> combine
    let horizontalBorders = [ xMin .. xMax ] |> List.collect (findHorizontalBorders plot) |> combine
    verticalBorders @ horizontalBorders

let calculateWithDiscount garden plot = 
    plot |> getBordersForPlot garden |> List.length

let calculateWithoutDiscount garden plot = 
    plot |> Set.toList |> List.sumBy (countBorders garden) 

let fenceCost discount garden plot = 
    let a = calculateArea plot
    let calculatePerimeter = if discount then calculateWithDiscount else calculateWithoutDiscount
    let p = calculatePerimeter garden plot 
    a * p

let outputResult maybeExpected result = 
    match maybeExpected with 
    | None -> 
        printfn "%d" result 
    | Some expected -> 
        if result = expected then 
            printfn "%d ✓" result 
        else 
            printfn "%d vs %d ✗" result expected

let run fileName maybeExpectedPart1 maybeExpectedPart2 = 
    printfn "%s" fileName
    let sw = Stopwatch.StartNew()
    let lines = readLines fileName |> List.map Seq.toList
    let garden = Garden.fromList lines 
    let plots = findAllPlots garden 
    let result1 = plots |> List.sumBy (fenceCost false garden) 
    outputResult maybeExpectedPart1 result1
    let result2 = plots |> List.sumBy (fenceCost true garden) 
    outputResult maybeExpectedPart2 result2
    printfn "%f ms" (sw.Elapsed.TotalMilliseconds)
    printfn ""

run "sample" (Some 140) (Some 80)       
run "sample-xo" (Some 772) (Some 436)    
run "sample-larger" (Some 1930) (Some 1206) 
run "sample-e" None (Some 236) 
run "sample-abba" None (Some 368)
run "input" None None
