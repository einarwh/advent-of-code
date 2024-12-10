// Advent of Code 2024. Day 09
// dotnet fsi aoc09.fsx

open System
open System.IO
open System.Collections.Generic

type FileId = int64
type Space = int 

type DiskEntry = 
    | File of (FileId * Space) 
    | Free of Space 

let trim (input : string) = input.Trim()

let toInt (ch : char) = int (ch - '0')

let createFileEntry fileId blocks = 
    (fileId, blocks) |> File

let createSpaceEntry blocks = 
    blocks |> Free 

let rec tryFindSpace (requiredBlocks : int) (endNode : LinkedListNode<DiskEntry>) (node : LinkedListNode<DiskEntry>) : LinkedListNode<DiskEntry> option = 
    if node = endNode then None 
    else 
        match node.Value with 
        | Free freeBlocks when freeBlocks >= requiredBlocks -> Some node 
        | _ -> 
            tryFindSpace requiredBlocks endNode node.Next
        
let compact (withFragmentation : bool) (entries : DiskEntry list) : DiskEntry list =
    let linked = new LinkedList<DiskEntry>(entries)
    let rec loop (node : LinkedListNode<DiskEntry>) = 
        printfn "Loop! "
        match node with 
        | null -> 
            printfn "null?"
            ()
        | _ -> 
            match node.Value with 
            | Free _ -> 
                printfn "Skipping free space."
                loop node.Previous 
            | File (fid, blocks) -> 
                printfn "Found file %d (%d blocks)" fid blocks 
                let requiredFreeSpace = if withFragmentation then 1 else blocks
                match tryFindSpace requiredFreeSpace node linked.First with 
                | None -> 
                    printfn "No free space available."
                    if withFragmentation then () else loop node.Previous 
                | Some spaceNode -> 
                    match spaceNode.Value with 
                    | File _ -> failwith "?"
                    | Free freeBlocks -> 
                        printfn "Found %d blocks of free space." freeBlocks
                        if freeBlocks = blocks then 
                            // Perfect match 
                            printfn "Perfect match!"
                            linked.AddBefore(spaceNode, File (fid, blocks)) |> ignore
                            linked.Remove(spaceNode)
                            let next = node.Previous
                            linked.AddBefore(node, Free blocks) |> ignore 
                            linked.Remove(node)
                            loop next
                        else if freeBlocks > blocks then 
                            // Leftover free blocks.
                            printfn "Leftover free blocks."
                            linked.AddBefore(spaceNode, File (fid, blocks)) |> ignore 
                            linked.AddBefore(spaceNode, Free (freeBlocks - blocks)) |> ignore
                            linked.Remove(spaceNode)
                            let next = node.Previous
                            linked.AddBefore(node, Free blocks) |> ignore 
                            linked.Remove(node)
                            loop next
                        else 
                            // Move as much as possible.
                            printfn "Move as much as possible."
                            linked.AddBefore(spaceNode, File (fid, freeBlocks)) |> ignore 
                            linked.Remove(spaceNode)
                            let remaining = linked.AddBefore(node, File (fid, blocks - freeBlocks))
                            linked.AddBefore(node, Free freeBlocks)
                            linked.Remove(node)
                            loop remaining
    loop linked.Last
    linked |> Seq.toList 

let compactFrag = compact true

let compactFile = compact false 

let compactFragmenting (entries : DiskEntry list) : DiskEntry list =
    let linked = new LinkedList<DiskEntry>(entries)
    let rec loop (node : LinkedListNode<DiskEntry>) = 
        printfn "Loop!"
        match node with 
        | null -> 
            printfn "null?"
            ()
        | _ -> 
            match node.Value with 
            | Free _ -> 
                printfn "Skipping free space."
                loop node.Previous 
            | File (fid, blocks) -> 
                printfn "Found file %d (%d blocks)" fid blocks 
                match tryFindSpace 1 node linked.First with 
                | None -> 
                    printfn "No free space available."
                    ()
                | Some spaceNode -> 
                    match spaceNode.Value with 
                    | File _ -> failwith "?"
                    | Free freeBlocks -> 
                        printfn "Found %d blocks of free space." freeBlocks
                        let nextNode =  
                            if freeBlocks = blocks then 
                                // Perfect match 
                                printfn "Perfect match!"
                                linked.AddBefore(spaceNode, File (fid, blocks)) |> ignore
                                node.Previous 
                            else if freeBlocks > blocks then 
                                // Leftover free blocks.
                                printfn "Leftover free blocks."
                                linked.AddBefore(spaceNode, File (fid, blocks)) |> ignore 
                                linked.AddBefore(spaceNode, Free (freeBlocks - blocks)) |> ignore
                                node.Previous
                            else 
                                // Move as much as possible.
                                printfn "Move as much as possible."
                                linked.AddBefore(spaceNode, File (fid, freeBlocks)) |> ignore 
                                let remaining = linked.AddBefore(node, File (fid, blocks - freeBlocks))
                                remaining
                        linked.Remove(spaceNode)
                        linked.Remove(node)
                        loop nextNode
    loop linked.Last
    linked |> Seq.toList 


// let compactWholeFiles (entries : DiskEntry list) : DiskEntry list = 
//     let linked = new LinkedList<DiskEntry>(entries)
//     let rec loop (node : LinkedListNode<DiskEntry>) = 
//         // printfn "Loop! "
//         match node with 
//         | null -> ()
//         | _ -> 
    

let run fileName = 
    let text = File.ReadAllText fileName |> trim 
    let digits : int list = text |> Seq.toList |> List.map (toInt) 
    let accumulateEntries (index : int, entries : DiskEntry list) (blocks : int) : (int * DiskEntry list) = 
        let entry = 
            if index % 2 = 0 then File (int64 (index / 2), blocks)
            else Free blocks
        (index + 1, entry :: entries)
    let (_, reversedEntries) = List.fold accumulateEntries (0, []) digits
    let entries = reversedEntries |> List.rev 
    // let fragmented = compactFragmenting entries
    let calculateFragmented (index : int, sum : int64) (entry : DiskEntry) : (int * int64) = 
        match entry with 
        | Free freeBlocks -> 
            (index + freeBlocks, sum)
        | File (fid, fileBlocks) -> 
            let nextIndex = index + fileBlocks
            let fileSum = [ index .. (nextIndex - 1) ] |> List.map int64 |> List.sumBy ((*) (int64 fid))
            (nextIndex, sum + fileSum)
    // fragmented |> List.iter (printfn "%A")
    // fragmented |> List.fold calculateFragmented (0, 0) |> snd |> printfn "%d"
    // entries |> compactFragmenting |> List.fold calculateFragmented (0, 0) |> snd |> printfn "%d"
    // entries |> compactFrag |> List.fold calculateFragmented (0, 0) |> snd |> printfn "%d"
    // entries |> compactFrag |> List.fold calculateFragmented (0, 0) |> snd |> printfn "%d"
    entries |> compactFile |> List.fold calculateFragmented (0, 0) |> snd |> printfn "%d"
    0

run "input"
