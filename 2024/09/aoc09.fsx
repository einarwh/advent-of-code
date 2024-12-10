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

let compact (linked : LinkedList<DiskEntry>) : DiskFile list =
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
    let linked = new LinkedList<DiskEntry>(entries)
    let compacted : DiskFile list = compact linked 
    let calculate (index : int, sum : int64) (file : DiskFile) : (int * int64) = 
        let nextIndex = index + file.blocks
        let fileSum = [ index .. (nextIndex - 1) ] |> List.map int64 |> List.sumBy ((*) (int64 file.fileId))
        let nextSum = sum + fileSum
        (nextIndex, nextSum)
    let (_, checksum) = compacted |> List.fold calculate (0, 0)
    printfn "%d" checksum

run "input"
