// Advent of Code 2024. Day 12: Garden Groups.
// dotnet fsi aoc12.fsx

open System
open System.IO

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

let rec fill (garden : char[,]) (ch : char) (x : int, y : int) (plot : Set<int * int>) = 
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

let fillPlot (garden : char[,]) (x, y)  = 
    let ch = Garden.get garden (x, y)
    fill garden ch (x, y) Set.empty

let findPlots (garden : char[,]) =
    let rec loop (startPositions : (int*int) list) (plots : Set<int*int> list) (visited : Set<int*int>) = 
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

let calculateArea plot = Set.count plot 

let countBorders (garden : char[,]) (x, y) : int = 
    let isBorder plotPlant pos = 
        match Garden.tryGet garden pos with 
        | None -> true 
        | Some plant -> plant <> plotPlant
    let plotPlant = Garden.get garden (x, y)
    [ (x, y-1); (x-1, y); (x, y+1); (x+1, y) ]
    |> List.filter (isBorder plotPlant)
    |> List.length 

let isHorizontalSide { startPos=(x1, y1); endPos=(x2, y2) } = 
    y1 = y2

let isHorizontal border = 
    match border with 
    | Enter side -> isHorizontalSide side 
    | Exit side -> isHorizontalSide side 

let isVerticalSide { startPos=(x1, y1); endPos=(x2, y2) } = 
    x1 = x2

let isVertical border = 
    match border with 
    | Enter side -> isVerticalSide side 
    | Exit side -> isVerticalSide side 

let rec combine (borders : Border list) : Border list = 
    // printfn "---"
    // printfn "Combine\n %A" borders
    match borders with 
    | [] -> []
    | first :: rest -> 
        let fitSide (firstSide : Side) (side : Side) = 
            side.startPos = firstSide.endPos || firstSide.startPos = side.endPos
        let fit (first : Border) (border : Border) = 
            match (first, border) with 
            | (Enter firstSide, Enter side) -> fitSide firstSide side 
            | (Exit firstSide, Exit side) -> fitSide firstSide side 
            | _ -> false
        match rest |> List.tryFind (fit first) with 
        | None -> 
            // printfn "No match for %A" first
            first :: combine rest 
        | Some border -> 
            // printfn "Found match for %A: %A" first border
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

let sideLength { startPos=(x1, y1); endPos=(x2, y2) } = 
    if x1 = x2 then abs (y1 - y2)
    else abs (x1 - x2)

let combineAll (borders : Border list) : Border list = 
    let horizontals = borders |> List.filter isHorizontal |> combine
    let verticals = borders |> List.filter isVertical |> combine
    let combined = horizontals @ verticals
    combined

let getBordersForPlot (garden : char[,]) (plot : Set<int*int>) : Border list = 
    let xs = plot |> Set.map fst
    let ys = plot |> Set.map snd
    let xMin = xs |> Set.minElement
    let xMax = xs |> Set.maxElement
    let yMin = ys |> Set.minElement
    let yMax = ys |> Set.maxElement
    // printfn "Plot: %A" plot 
    // printfn "xMin: %d" xMin
    // printfn "xMax: %d" xMax
    // printfn "yMin: %d" yMin
    // printfn "yMax: %d" yMax
    let xRange = [ (xMin - 1) .. (xMax + 1) ]
    let yRange = [ (yMin - 1) .. (yMax + 1) ]
    let checkBorder before after endPos : Border option = 
        match (plot |> Set.contains before, plot |> Set.contains after) with 
        | false, true -> Some <| Enter ({ startPos = after; endPos = endPos })
        | true, false -> Some <| Exit ({ startPos = after; endPos = endPos })
        | _ -> None 
    let findVerticalBorders plot y0 = 
        // printfn "findVerticalBorders for y0: %d" y0
        let xs = plot |> Set.filter (fun (x, y) -> y = y0) |> Set.map fst
        let xMin = xs |> Set.minElement
        let xMax = xs |> Set.maxElement
        let xRange = [ (xMin - 1) .. (xMax + 1) ]
        // printfn "x-range: %A" xRange
        xRange |> List.pairwise |> List.choose (fun (x1, x2) -> checkBorder (x1, y0) (x2, y0) (x2, y0 + 1))
    let findHorizontalBorders plot x0 = 
        // printfn "findHorizontalBorders for x0: %d" x0
        let ys = plot |> Set.filter (fun (x, y) -> x = x0) |> Set.map snd
        let yMin = ys |> Set.minElement
        let yMax = ys |> Set.maxElement
        let yRange = [ (yMin - 1) .. (yMax + 1) ]
        // printfn "y-range: %A" yRange
        yRange |> List.pairwise |> List.choose (fun (y1, y2) -> checkBorder (x0, y1) (x0, y2) (x0+1, y2))
    let verticalBorders = [ yMin .. yMax ] |> List.collect (findVerticalBorders plot)
    let horizontalBorders = [ xMin .. xMax ] |> List.collect (findHorizontalBorders plot)
    verticalBorders @ horizontalBorders

let calculateWithDiscount (garden : char[,]) (plot : Set<int*int>) = 
    let borders : Border list = plot |> getBordersForPlot garden
    // printfn "Border count. %A" (List.length borders)
    let combined = borders |> combineAll
    // printfn "Combined count. %A" (List.length combined)
    combined |> List.length
    // let bar = 
    //     foo 
    //     |> combineAll 
    //     |> List.length
    // foo

let calculateWithoutDiscount (garden : char[,]) (plot : (int*int) list) = 
    plot |> List.sumBy (countBorders garden) 

let calculatePerimeter (discount : bool) (garden : char[,]) (plot : Set<int*int>) = 
    let plotLst = plot |> Set.toList 
    if discount then calculateWithDiscount garden plot
    else calculateWithoutDiscount garden plotLst

let fenceCost (garden : char[,]) (plot : Set<int*int>) = 
    let a = calculateArea plot
    let p = calculatePerimeter true garden plot 
    // printfn "(a:%d) * (p:%d) = %d" a p (a * p)
    a * p

let run fileName = 
    let lines = readLines fileName |> List.map Seq.toList
    let garden = Garden.fromList lines 
    let foo = fillPlot garden (0, 0)
    let allPlots = findPlots garden 
    // plots |> List.length |> printfn "%d"
    // plots |> List.map (fenceCost garden) |> printfn "%A"
    // printfn "%s" fileName
    allPlots |> List.sumBy (fenceCost garden) |> printfn "%A"
    // [(0, 0); (1, 0); (2, 0); (3, 0)]
    // |> Set.ofList
    // |> calculateWithDiscount garden
    // |> printfn "%d"
    // // |> List.iter (printfn "%A")

    // [(0, 0); (1, 0); (2, 0); (3, 0)]
    // |> calculateWithDiscount garden

// run "sample"

// printfn "\nActual"
// printfn "======"
run "sample"        // 80
run "sample-xo"     // 436
run "sample-larger" // 1206
run "sample-e"      // 236
run "sample-abba"   // 368
run "input"         // ???

printfn "\n\nExpected"
printfn "========="
[80;436;1206;236;368] |> List.iter (printfn "%d")
