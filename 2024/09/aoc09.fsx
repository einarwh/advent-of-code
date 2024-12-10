// Advent of Code 2024. Day 09
// dotnet fsi aoc09.fsx

open System
open System.IO
open System.Collections.Generic

type DiskFile = 
    { fileId : int
      blocks : int }

type FreeSpace = 
    { blocks : int }

type DiskEntry = 
    | FileEntry of DiskFile 
    | SpaceEntry of FreeSpace 

let trim (input : string) = input.Trim()

let toInt (ch : char) = int (ch - '0')

let createFileEntry fileId blockIndex blocks = 
    { fileId = fileId
      blocks = blocks } |> FileEntry

let createSpaceEntry blockIndex blocks = 
    { blocks = blocks } |> SpaceEntry

let compactFragmenting (linked : LinkedList<DiskEntry>) : DiskFile list =
    let rec loop (result : DiskFile list) = 
        match linked.Count with 
        | 0 -> result 
        | 1 ->
            match linked.First.Value with 
            | FileEntry file -> 
                file :: result 
            | SpaceEntry _ -> 
                result  
        | _ -> 
            let firstEntry = linked.First.Value
            linked.RemoveFirst()
            match firstEntry with 
            | FileEntry (file : DiskFile) -> 
                loop (file :: result)
            | SpaceEntry (space : FreeSpace) -> 
                let lastEntry = linked.Last.Value 
                linked.RemoveLast()
                match lastEntry with 
                | SpaceEntry _ ->
                    linked.AddFirst(firstEntry) |> ignore
                    loop result 
                | FileEntry fileToInsert -> 
                    if fileToInsert.blocks = space.blocks then 
                        loop (fileToInsert :: result)
                    else if fileToInsert.blocks < space.blocks then 
                        let leftoverSpace = 
                            { blocks = space.blocks - fileToInsert.blocks } |> SpaceEntry
                        linked.AddFirst(leftoverSpace) |> ignore
                        loop (fileToInsert :: result)
                    else 
                        let placedFile = 
                            { fileId = fileToInsert.fileId
                              blocks = space.blocks }
                        let leftoverFile = 
                            { fileId = fileToInsert.fileId
                              blocks = fileToInsert.blocks - space.blocks } |> FileEntry
                        linked.AddLast(leftoverFile) |> ignore
                        loop (placedFile :: result)
    loop [] |> List.rev

let compactWholeFiles (linked : LinkedList<DiskEntry>) : DiskEntry list =
    let rec tryFindSpaceNode (blocks : int) (node : LinkedListNode<DiskEntry>) : LinkedListNode<DiskEntry> option = 
        match node with 
        | null -> None 
        | _ -> 
            match node.Value with 
            | SpaceEntry space when space.blocks >= blocks -> Some node 
            | _ -> tryFindSpaceNode blocks node.Next
    let rec loop (result : DiskEntry list) = 
        printfn "Loop with %d entries remaining in linked list, %d added to result." linked.Count (List.length result)
        match linked.Count with 
        | 0 -> result 
        | 1 -> linked.First.Value :: result 
        | _ -> 
            let lastEntry = linked.Last.Value
            linked.RemoveLast()
            match lastEntry with
            | SpaceEntry _ -> 
                printfn "Last entry was just space. Adding it to result."
                loop (lastEntry :: result) 
            | FileEntry file -> 
                printfn "Last entry was a file to try to move."
                // Try to find enough space!
                match tryFindSpaceNode file.blocks linked.First with 
                | Some spaceNode -> 
                    printfn "Found enough space for the file!"
                    match spaceNode.Value with 
                    | SpaceEntry space -> 
                        printfn "File takes %d blocks, found space with %d blocks." file.blocks space.blocks
                        // Add file before.
                        linked.AddBefore(spaceNode, lastEntry)
                        // Any remaining space after.
                        let leftoverSpace = 
                            { blocks = space.blocks - file.blocks } |> SpaceEntry
                        linked.AddAfter(spaceNode, leftoverSpace)
                        // Remove old space.
                        linked.Remove(spaceNode)
                        // What now?
                        loop (FileEntry file :: leftoverSpace :: result)
                    | FileEntry _ -> failwith "wrong"
                | None -> 
                    // File can't be moved.
                    loop (lastEntry :: result)
    loop [] |> List.rev

let run fileName = 
    let text = File.ReadAllText fileName |> trim 
    let digits : int list = text |> Seq.toList |> List.map (toInt) 
    let accumulateEntries (index : int, blockIndex : int, entries : DiskEntry list) (blocks : int) : (int * int * DiskEntry list) = 
        let entry = 
            if index % 2 = 0 then  
                createFileEntry (index / 2) blockIndex blocks 
            else 
                createSpaceEntry blockIndex blocks
        (index + 1, blockIndex + blocks, entry :: entries)
    let (_, _, reversedEntries) = List.fold accumulateEntries (0, 0, []) digits
    let entries = reversedEntries |> List.rev 
    let fragmented : DiskFile list = compactFragmenting (new LinkedList<DiskEntry>(entries)) 
    let calculate (index : int, sum : int64) (file : DiskFile) : (int * int64) = 
        let nextIndex = index + file.blocks
        let fileSum = [ index .. (nextIndex - 1) ] |> List.map int64 |> List.sumBy ((*) (int64 file.fileId))
        let nextSum = sum + fileSum
        (nextIndex, nextSum)
    fragmented |> List.fold calculate (0, 0) |> snd |> printfn "%d"
    let compacted : DiskEntry list = compactWholeFiles (new LinkedList<DiskEntry>(entries))
    compacted |> List.iter (printfn "%A")
    0

run "sample"
