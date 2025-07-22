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
    let childNodeCount = data[index]
    let metadataEntryCount = data[index + 1]
    let header = {
        childNodeCount = childNodeCount
        metadataEntryCount = metadataEntryCount
    }
    let rec buildChildNodes (acc : Node list) (childNumber : int) (index : int) =
        if childNumber < childNodeCount then
            let ix, child = buildNode index data
            buildChildNodes (child :: acc) (childNumber + 1) ix
        else
            (index, acc |> List.rev)
    let readMetadataEntries (index : int) =
        let entries = [ index .. index + metadataEntryCount - 1 ] |> List.map (fun i -> data[i])
        (index + metadataEntryCount - 1, entries)
    let ixAfterChildren, childNodes = buildChildNodes [] 0 (index + 2)
    let ixAfterMetadata, metadataEntries = readMetadataEntries ixAfterChildren
    let node = { header = header; childNodes = childNodes; metadataEntries = metadataEntries }
    (ixAfterMetadata + 1, node)

let rec sumMetadata (node : Node) =
    let sum1 = node.metadataEntries |> List.sum
    let sum2 = node.childNodes |> List.map sumMetadata |> List.sum
    sum1 + sum2

let rec nodeValue (node : Node) : int = 
    if node.childNodes |> List.isEmpty then 
        node.metadataEntries |> List.sum
    else 
        node.metadataEntries
        |> List.choose (fun i -> List.tryItem (i - 1) node.childNodes)
        |> List.map nodeValue 
        |> List.sum

let run fileName =
    let text = File.ReadAllText(fileName).Trim()
    let node = text |> parse |> buildNode 0 |> snd
    node |> sumMetadata |> printfn "%d"
    node |> nodeValue |> printfn "%d"

run "input.txt"
