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

let isHorizontal { startPos=(x1, y1); endPos=(x2, y2) } = 
    y1 = y2

let isVertical { startPos=(x1, y1); endPos=(x2, y2) } = 
    x1 = x2

let rec combine (borders : Side list) : Side list = 
    // printfn "---"
    // printfn "Combine\n %A" borders
    match borders with 
    | [] -> []
    | first :: rest -> 
        // 
        match rest |> List.tryFind (fun border -> border.startPos = first.endPos || first.startPos = border.endPos) with 
        | None -> 
            // printfn "No match for %A" first
            first :: combine rest 
        | Some border -> 
            // printfn "Found match for %A: %A" first border
            let filtered = rest |> List.filter ((<>) border)
            // printfn "Original length: \n%d" (List.length rest)
            // printfn "Filtered length: \n%d" (List.length filtered)
            let combined = 
                if border.startPos = first.endPos then 
                    { startPos = first.startPos; endPos = border.endPos }
                else 
                    { startPos = border.startPos; endPos = first.endPos }
            // printfn "Combined side\n%A" combined
            combine (combined :: filtered)

let sideLength { startPos=(x1, y1); endPos=(x2, y2) } = 
    if x1 = x2 then abs (y1 - y2)
    else abs (x1 - x2)

let combineAll (borders : Side list) : Side list = 
    let horizontals = borders |> List.filter isHorizontal |> combine
    let verticals = borders |> List.filter isVertical |> combine
    let combined = horizontals @ verticals
    combined

let getBorders (garden : char[,]) (x, y) : Side list = 
    let tryGetBorder plotPlant (pos, border)  = 
        match Garden.tryGet garden pos with 
        | None -> Some border 
        | Some plant -> 
            if plant <> plotPlant then Some border else None
    let plotPlant = Garden.get garden (x, y)
    [ ((x, y-1), { startPos = (x, y); endPos = (x+1, y) })
      ((x-1, y), { startPos = (x, y); endPos = (x, y + 1) })
      ((x, y+1), { startPos = (x, y+1); endPos = (x+1, y+1) })
      ((x+1, y), { startPos = (x+1, y); endPos = (x+1, y+1) }) ]
    |> List.choose (tryGetBorder plotPlant)

let calculateWithDiscount (garden : char[,]) (plot : (int*int) list) = 
    let foo = 
        plot 
        |> List.collect (getBorders garden)
        |> combineAll 
        |> List.length
    foo

let calculateWithoutDiscount (garden : char[,]) (plot : (int*int) list) = 
    plot |> List.sumBy (countBorders garden) 

let calculatePerimeter (discount : bool) (garden : char[,]) (plot : Set<int*int>) = 
    let plotLst = plot |> Set.toList 
    if discount then calculateWithDiscount garden plotLst
    else calculateWithoutDiscount garden plotLst

let fenceCost (garden : char[,]) (plot : Set<int*int>) = 
    let a = calculateArea plot
    let p = calculatePerimeter true garden plot 
    printfn "(a:%d) * (p:%d) = %d" a p (a * p)
    a * p

let run fileName = 
    let lines = readLines fileName |> List.map Seq.toList
    let garden = Garden.fromList lines 
    let foo = fillPlot garden (0, 0)
    let plots = findPlots garden 
    // plots |> List.length |> printfn "%d"
    // plots |> List.map (fenceCost garden) |> printfn "%A"
    printfn "%s" fileName
    plots |> List.sumBy (fenceCost garden) |> printfn "%A"
    // [(0, 0); (1, 0); (2, 0); (3, 0)]
    // |> calculateWithDiscount garden

printfn "\nActual"
printfn "======"
run "sample"        // 80
run "sample-xo"     // 436
run "sample-larger" // 1206
run "sample-e"      // 236
run "sample-abba"   // 368

printfn "\n\nExpected"
printfn "========="
[80;436;1206;236;368] |> List.iter (printfn "%d")
