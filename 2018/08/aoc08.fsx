// Advent of Code 2018. Day 08: Memory Maneuver.
// dotnet fsi aoc08.fsx

open System
open System.IO

type Header = {
    childNodeCount : int 
    metadataEntryCount : int 
}

type Node = {
    header : Header 
    childNodes : Node list 
    metadataEntries : int list
}

let parse (s : string) = 
    s.Split(" ") |> Array.map int 

let rec buildNode (index : int) (data : int array) : int * Node = 
    printfn "build node @ index %d" index
    let childNodeCount = data[index]
    let metadataEntryCount = data[index + 1]
    printfn "reading child node count @ index %d: %d" index childNodeCount
    printfn "reading meta entry count @ index %d: %d" (index + 1) metadataEntryCount

    let header = {
        childNodeCount = childNodeCount
        metadataEntryCount = metadataEntryCount
    }
    let rec buildChildNodes (acc : Node list) (childNumber : int) (index : int) = 
        printfn "buildChildNodes child node #%d @ index %d" childNumber index
        if childNumber < childNodeCount then 
            printfn "try read child node #%d @ index %d" childNumber index
            let ix, child = buildNode index data 
            printfn "after reading child node #%d, index %d" childNumber index
            buildChildNodes (child :: acc) (childNumber + 1) ix
        else 
            printfn "done reading child node #%d @ index %d" childNumber index
            (index, acc |> List.rev)
    let readMetadataEntries (index : int) = 
        printfn "try read metadata @ index %d" index
        let entries = [ index .. index + metadataEntryCount - 1 ] |> List.map (fun i -> data[i])
        (index + metadataEntryCount - 1, entries)
    let ixAfterChildren, childNodes = buildChildNodes [] 0 (index + 2)
    printfn "after reading children @ index %d" ixAfterChildren
    let ixAfterMetadata, metadataEntries = readMetadataEntries ixAfterChildren 
    printfn "after reading metadata @ index %d" ixAfterMetadata
    let node = { header = header; childNodes = childNodes; metadataEntries = metadataEntries }
    (ixAfterMetadata + 1, node)

let run fileName = 
    let text = File.ReadAllText(fileName).Trim()
    text |> parse |> printfn "%A"
    text |> parse |> buildNode 0 |> printfn "%A"

run "sample.txt"
