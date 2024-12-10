// Advent of Code 2024. Day 09
// dotnet fsi aoc09.fsx

open System
open System.IO
open System.Collections.Generic

type Space = { index : int; blocks : int }

type DiskEntry = 
    | File of (int64 * Space) 
    | Free of Space 

let trim (input : string) = input.Trim()

let toInt (ch : char) = int (ch - '0')

let createFileEntry fileId blockIndex blocks = 
    (fileId, { index = blockIndex; blocks = blocks }) |> File

let createSpaceEntry blockIndex blocks = 
    { index = blockIndex; blocks = blocks } |> Free 

let compactFragmenting (linked : LinkedList<DiskEntry>) : (int64 * Space) list =
    let rec loop (result : (int64 * Space) list) = 
        match linked.Count with 
        | 0 -> result 
        | 1 ->
            match linked.First.Value with 
            | File file -> 
                file :: result 
            | Free _ -> 
                result  
        | _ -> 
            let firstEntry = linked.First.Value
            linked.RemoveFirst()
            match firstEntry with 
            | File file -> 
                loop (file :: result)
            | Free space -> 
                let lastEntry = linked.Last.Value 
                linked.RemoveLast()
                match lastEntry with 
                | Free _ ->
                    linked.AddFirst(firstEntry) |> ignore
                    loop result 
                | File fileToInsert -> 
                    let (fileId, fileSpace) = fileToInsert
                    if fileSpace.blocks = space.blocks then 
                        loop (fileToInsert :: result)
                    else if fileSpace.blocks < space.blocks then 
                        let leftoverSpace = 
                            { index = space.index + fileSpace.blocks
                              blocks = space.blocks - fileSpace.blocks } |> Free
                        linked.AddFirst(leftoverSpace) |> ignore
                        loop (fileToInsert :: result)
                    else 
                        let placedFile = (fileId, { index = space.index; blocks = space.blocks })
                        let leftoverFile = (fileId, { index = fileSpace.index; blocks = fileSpace.blocks - space.blocks})
                        linked.AddLast(File leftoverFile) |> ignore
                        loop (placedFile :: result)
    loop [] |> List.rev

let rec toString (node : LinkedListNode<DiskEntry>) = 
    match node with 
    | null -> ""
    | _ -> 
        match node.Value with 
        | Free freeSpace -> 
            let s = new String('.', freeSpace.blocks)
            s + toString (node.Next)
        | File (fileId, fileSpace) -> 
            let ch = fileId.ToString()[0]
            let s = new String(ch, fileSpace.blocks)
            s + toString (node.Next)

let compactWholeFiles (linked : LinkedList<DiskEntry>) =
    let rec tryFindSpaceNode (maxIndex : int) (blocks : int) (node : LinkedListNode<DiskEntry>) : LinkedListNode<DiskEntry> option = 
        match node with 
        | null -> None 
        | _ -> 
            match node.Value with 
            | Free freeSpace when freeSpace.blocks >= blocks -> Some node 
            | Free freeSpace -> 
                if freeSpace.index >= maxIndex then 
                    None 
                else 
                    tryFindSpaceNode maxIndex blocks node.Next
            | File (_, fileSpace) -> 
                if fileSpace.index >=  maxIndex then 
                    None 
                else 
                    tryFindSpaceNode maxIndex blocks node.Next
    let rec findFirstFree (node : LinkedListNode<DiskEntry>) : LinkedListNode<DiskEntry> = 
        match node with 
        | null -> null 
        | _ -> 
            match node.Value with 
            | Free _ -> node 
            | _ -> findFirstFree node.Next 
    let tryGetIndex (node : LinkedListNode<DiskEntry>) : int option = 
        match node with 
        | null -> None 
        | _ -> 
            let space = 
                match node.Value with 
                | File (_, fileSpace) -> fileSpace 
                | Free freeSpace -> freeSpace 
            Some space.index 
    let rec loop (node : LinkedListNode<DiskEntry>) (moved : Set<int64>) = 
        // printfn ""
        // printfn "Looping."
        // printfn "%s" (toString linked.First)
        match node with 
        | null -> linked 
        | _ -> 
            match node.Value with 
            | Free _ -> 
                // printfn "Skipping past space."
                loop node.Previous moved 
            | File (fileId, fileSpace) -> 
                // printfn "Entry is a file."
                if Set.contains fileId moved then 
                    printfn "File already moved, skipping past."
                    loop node.Previous moved 
                else
                    let moved' = moved |> Set.add fileId 
                    // printfn "Found a file to move: (id: %d, blocks: %d)" fileId fileSpace.blocks
                    match tryFindSpaceNode fileSpace.index fileSpace.blocks linked.First with 
                    | None -> 
                        // No space found for the given file!
                        // printfn "No space, file remains in place."
                        loop node.Previous moved'
                    | Some spaceNode -> 
                        match spaceNode.Value with 
                        | File _ -> failwith "?"
                        | Free freeSpace -> 
                            if freeSpace.index >= fileSpace.index then 
                                // printfn "Free space to the right, not to the left."
                                loop node.Previous moved'
                            else 
                                // Found space! Can safely remove the file.
                                // printfn "Found free space for the file."
                                let previous = node.Previous 
                                // Leave space where the file was!
                                linked.AddBefore(node, Free { index = fileSpace.index; blocks = fileSpace.blocks }) |> ignore
                                linked.Remove(node)
                                // Insert file before original space.
                                let movedFile = (fileId, { index = freeSpace.index; blocks = fileSpace.blocks })
                                linked.AddBefore(spaceNode, File movedFile) |> ignore
                                let leftover = freeSpace.blocks - fileSpace.blocks 
                                // printfn "Leftover space: %d" leftover
                                if leftover > 0 then 
                                    // printfn "Adding remaining space node."
                                    // Insert remaining space after original space.
                                    linked.AddAfter(spaceNode, Free { index = freeSpace.index + fileSpace.blocks; blocks = leftover }) |> ignore
                                else 
                                    // printfn "No space to add."
                                    ()
                                // Remove original space.
                                linked.Remove(spaceNode)
                                loop previous moved'
    loop (linked.Last) Set.empty |> ignore
    linked |> Seq.toList

let verifyCompacted compacted = 
    let rec fn index lst = 
        match lst with 
        | [] -> printfn "."
        | h :: t -> 
            match h with 
            | File (fileId, fileSpace) -> 
                if index <> fileSpace.index then 
                    printfn "???"
                fn (index + fileSpace.blocks) t 
            | Free freeSpace -> 
                if index <> freeSpace.index then 
                    printfn "!!!"
                fn (index + freeSpace.blocks) t 
    fn 0 compacted 

let run fileName = 
    let text = File.ReadAllText fileName |> trim 
    let digits : int list = text |> Seq.toList |> List.map (toInt) 
    let accumulateEntries (index : int, blockIndex : int, entries : DiskEntry list) (blocks : int) : (int * int * DiskEntry list) = 
        let entry = 
            if index % 2 = 0 then  
                createFileEntry (int64 (index / 2)) blockIndex blocks 
            else 
                createSpaceEntry blockIndex blocks
        (index + 1, blockIndex + blocks, entry :: entries)
    let (_, _, reversedEntries) = List.fold accumulateEntries (0, 0, []) digits
    let entries = reversedEntries |> List.rev 
    let fragmented = compactFragmenting (new LinkedList<DiskEntry>(entries)) 
    let calculateFragmented (index : int, sum : int64) (file : (int64 * Space)) : (int * int64) = 
        let (fileId, fileSpace) = file 
        let nextIndex = index + fileSpace.blocks
        let fileSum = [ index .. (nextIndex - 1) ] |> List.map int64 |> List.sumBy ((*) (int64 fileId))
        let nextSum = sum + fileSum
        (nextIndex, nextSum)
    fragmented |> List.fold calculateFragmented (0, 0) |> snd |> printfn "%d"
    let calculateWholeFiles (sum : int64) (entry : DiskEntry) : int64 = 
        match entry with 
        | File (fileId, fileSpace) -> 
            let nextIndex = fileSpace.index + fileSpace.blocks
            let fileSum = [ fileSpace.index .. (nextIndex - 1) ] |> List.map int64 |> List.sumBy ((*) (int64 fileId))
            // printfn "file sum for %d (%d blocks at index %d) = %d" fileId fileSpace.blocks fileSpace.index fileSum
            sum + fileSum
        | Free _ -> sum 
    entries |> List.iter (printfn "%A")
    let compacted = compactWholeFiles (new LinkedList<DiskEntry>(entries))
    verifyCompacted compacted 
    compacted |> List.fold calculateWholeFiles 0 |> printfn "%d"
    0

run "input"
