// Advent of Code 2018. Day 11: Chronal Charge.
// https://en.wikipedia.org/wiki/Summed-area_table
// dotnet fsi aoc11.fsx

open System.IO

let getHundredsDigit n = 
    (n / 100) % 10

let getPowerLevel serialNumber (x, y) = 
    let rackId = x + 10
    let d = getHundredsDigit <| (rackId * y + serialNumber) * rackId 
    d - 5

let lookup pos map = 
    Map.tryFind pos map |> Option.defaultValue 0 

let getSummedAreaMap serialNumber = 
    // I(x,y) = i(x,y) + I(x,y-1) + I(x-1,y) - I(x-1,y-1)
    let minIndex = 1
    let maxIndex = 300 
    let rec loop (x, y) map = 
        let i = getPowerLevel serialNumber (x, y)
        let v = i + lookup (x, y-1) map + lookup (x-1, y) map - lookup (x-1, y-1) map
        let map' = Map.add (x, y) v map 
        if x < maxIndex then 
            loop (x + 1, y) map' 
        else if y < maxIndex then 
            loop (minIndex, y + 1) map'
        else 
            map'
    loop (1, 1) Map.empty

let getTotalPower summedAreaMap size (x, y) = 
    // I(D) + I(A) - I(B) - I(C)
    let a = (x-1, y-1) 
    let b = (x-1+size, y-1) 
    let c = (x-1, y-1+size)
    let d = (x-1+size, y-1+size) 
    lookup d summedAreaMap + lookup a summedAreaMap - lookup b summedAreaMap - lookup c summedAreaMap 

let findFuelCell summedAreaMap size = 
    let range = [1..300-size+1]
    [ for x in range do for y in range do yield (x, y) ]
    |> List.map (fun pos -> (pos, getTotalPower summedAreaMap size pos))
    |> List.sortByDescending snd 
    |> List.head

let findFuelCell3x3 summedAreaMap = 
    findFuelCell summedAreaMap 3 |> fst 

let findFuelCellAnySize summedAreaMap = 
    [1..300]
    |> List.map (fun size -> (findFuelCell summedAreaMap size, size))
    |> List.sortByDescending (fun ((_, pow), _) -> pow)
    |> List.map (fun (((x, y), _), size) -> (x, y, size))
    |> List.head

let run fileName = 
    let serialNumber = File.ReadAllText(fileName).Trim() |> int 
    let summedAreaMap = getSummedAreaMap serialNumber
    findFuelCell3x3 summedAreaMap |> fun (x, y) -> printfn "%d,%d" x y
    findFuelCellAnySize summedAreaMap |> fun (x, y, s) -> printfn "%d,%d,%d" x y s
    
run "input.txt"
