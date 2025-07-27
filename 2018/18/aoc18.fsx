// Advent of Code 2018. Day 18: Settlers of The North Pole.
// dotnet fsi aoc18.fsx

open System
open System.IO

type Acre = Ground | Trees | Lumberyard 

module Area =
    let tryGet (area : Acre[,]) (x, y) =
        let yMax = area.GetLength 0 - 1
        let xMax = area.GetLength 1 - 1
        if x < 0 || x > xMax || y < 0 || y > yMax then None 
        else 
            Some (Array2D.get area y x)
    let get (area : Acre[,]) (x, y) =
        Array2D.get area y x
    let set (area : Acre[,]) (x, y) (value : Acre) =
        Array2D.set area y x value
    let fromNestedList (lst : Acre list list) =
        let width = lst |> List.head |> List.length
        let height = lst |> List.length
        Array2D.init height width (fun y x -> lst |> List.item y |> List.item x)
    let toNestedList (area : Acre[,]) =
        let yRange = [ 0 .. area.GetLength(0) - 1 ]
        let xRange = [ 0 .. area.GetLength(1) - 1 ]
        yRange
        |> List.map (fun y -> xRange |> List.map (fun x -> get area (x, y)))
    let getNestedPositions (area : Acre[,]) =
        let yRange = [ 0 .. area.GetLength(0) - 1 ]
        let xRange = [ 0 .. area.GetLength(1) - 1 ]
        yRange
        |> List.map (fun y -> xRange |> List.map (fun x -> x, y))
    let toIndexedList (area : Acre[,]) =
        let yRange = [ 0 .. area.GetLength(0) - 1 ]
        let xRange = [ 0 .. area.GetLength(1) - 1 ]
        yRange
        |> List.map (fun y -> xRange |> List.map (fun x -> (x, y), get area (x, y)))
        |> List.concat

let visualize area =
    let selectChar acre =
        match acre with 
        | Ground -> '.'
        | Trees -> '|'
        | Lumberyard -> '#'
    let lines = area |> Area.toNestedList |> List.map (List.map selectChar)
    lines |> List.map (fun chars -> new String(List.toArray chars)) |> String.concat "\n" |> printfn "%s"

let getNeighbours (area : Acre[,]) (x, y) = 
    [ (x-1, y-1); (x, y-1); (x+1, y-1)
      (x-1, y); (x+1, y)
      (x-1, y+1); (x, y+1); (x+1, y+1) ]
    |> List.choose (Area.tryGet area)

let countAcre (acres : Acre list) (acre : Acre) = 
    acres |> List.filter ((=) acre) |> List.length 

let choose (area : Acre[,]) (x, y) : Acre = 
    let neighbours = getNeighbours area (x, y)
    match Area.get area (x, y) with 
    | Ground -> if countAcre neighbours Trees >= 3 then Trees else Ground 
    | Trees -> if countAcre neighbours Lumberyard >= 3 then Lumberyard else Trees 
    | Lumberyard -> if countAcre neighbours Lumberyard > 0 && countAcre neighbours Trees > 0 then Lumberyard else Ground 

let step (area : Acre[,]) = 
    let positions = Area.getNestedPositions area 
    let next = positions |> List.map (fun row -> row |> List.map (choose area)) |> Area.fromNestedList
    printfn ""
    visualize next
    0

let evolve steps initialArea = 
    let rec loop (i : int) (area : Acre[,]) = 
        printfn ""
        printfn "Step %d" i
        visualize area
        if i < steps then 
            let positions = Area.getNestedPositions area 
            let next = positions |> List.map (fun row -> row |> List.map (choose area)) |> Area.fromNestedList
            loop (i + 1) next
        else 
            area
    loop 0 initialArea

let readLines = 
    File.ReadAllLines
    >> Array.filter (fun line -> line <> String.Empty)
    >> Array.toList

let run fileName = 
    let lines = readLines fileName
    let toAcre (ch : char) = 
        match ch with 
        | '.' -> Ground
        | '|' -> Trees
        | '#' -> Lumberyard
        | _ -> failwith "?"
    let area = lines |> List.map (fun line -> line |> Seq.toList |> List.map toAcre) |> Area.fromNestedList
    let area' = evolve 10 area
    let acres = area' |> Area.toNestedList |> List.concat 
    let trees = countAcre acres Trees 
    let lumberyards = countAcre acres Lumberyard
    trees * lumberyards |> printfn "%d"

run "input.txt"
