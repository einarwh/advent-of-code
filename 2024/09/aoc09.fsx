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
    printfn "create file entry with id %d at %d: %d blocks" fileId blockIndex blocks
    { fileId = fileId
      blocks = blocks } |> FileEntry

let createSpaceEntry blockIndex blocks = 
    printfn "create space entry at %d: %d blocks" blockIndex blocks
    { blocks = blocks } |> SpaceEntry

let compact (linked : LinkedList<DiskEntry>) : DiskFile list =
    let rec loop (result : DiskFile list) = 
        printfn "======================="
        printfn "Loop. %d files placed. %d entries remaining to handle. " (List.length result) linked.Count
        match linked.Count with 
        | 0 -> result 
        | 1 ->
            match linked.First.Value with 
            | FileEntry file -> 
                printfn "Add file."
                file :: result 
            | SpaceEntry _ -> 
                printfn "Drop space."
                result  
        | _ -> 
            let firstEntry = linked.First.Value
            linked.RemoveFirst()
            match firstEntry with 
            | FileEntry (file : DiskFile) -> 
                printfn "Found file entry first %A" file
                loop (file :: result)
            | SpaceEntry (space : FreeSpace) -> 
                printfn "Found space entry first %A" space
                let lastEntry = linked.Last.Value 
                linked.RemoveLast()
                match lastEntry with 
                | SpaceEntry _ ->
                    printfn "Found space last, just skip it."
                    printfn "Put back the space in front, we didn't use it."
                    linked.AddFirst(firstEntry)
                    loop result 
                | FileEntry fileToInsert -> 
                    printfn "Found file to insert last."
                    // Insert.
                    if fileToInsert.blocks = space.blocks then 
                        // Perfect match! 
                        printfn "Perfect match, just insert the file. Nothing to put back."
                        loop (fileToInsert :: result)
                    else if fileToInsert.blocks < space.blocks then 
                        // Leftover space.
                        let leftoverSpace = 
                            { blocks = space.blocks - fileToInsert.blocks } |> SpaceEntry
                        // Put leftover space back in front.
                        linked.AddFirst(leftoverSpace)
                        // Insert the file.
                        loop (fileToInsert :: result)
                    else 
                        // File too big.
                        let placedFile = 
                            { fileId = fileToInsert.fileId
                              blocks = space.blocks }
                        let leftoverFile = 
                            { fileId = fileToInsert.fileId
                              blocks = fileToInsert.blocks - space.blocks } |> FileEntry
                        // No space to put back in front.
                        // Put remaining file back last.
                        linked.AddLast(leftoverFile)
                        // Insert the file.
                        loop (placedFile :: result)
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
    printfn "Entries: %d" (List.length entries)
    let linked = new LinkedList<DiskEntry>(entries)
    printfn "Linked: %d" (Seq.length linked)
    // linked |> Seq.iter (printfn "%A")
    let compacted : DiskFile list = compact linked 
    compacted |> List.iter (printfn "%A")
    let calculate (index : int, sum : int) (file : DiskFile) : (int * int) = 
        let nextIndex = index + file.blocks
        let fileSum = [ index .. (nextIndex - 1) ] |> List.sumBy ((*) file.fileId)
        let nextSum = sum + fileSum
        (nextIndex, nextSum)
    let (_, checksum) = compacted |> List.fold calculate (0, 0)
    printfn "%d" checksum

    0
    // digits |> List.length |> printfn "%d"

run "sample"
