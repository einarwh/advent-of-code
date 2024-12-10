// Advent of Code 2024. Day 09
// dotnet fsi aoc09.fsx

open System
open System.IO

type DiskFile = 
    { fileId : int
      startIndex : int 
      endIndex : int 
      blocks : int }

type FreeSpace = 
    { startIndex: int 
      endIndex : int
      blocks : int }

type DiskEntry = 
    | FileEntry of DiskFile 
    | SpaceEntry of FreeSpace 

let trim (input : string) = input.Trim()

let toInt (ch : char) = int (ch - '0')

let createFileEntry fileId blockIndex blocks = 
    printfn "create file entry with id %d at %d: %d blocks" fileId blockIndex blocks
    { fileId = fileId
      startIndex = blockIndex 
      endIndex = blockIndex + blocks - 1 
      blocks = blocks } |> FileEntry

let createSpaceEntry blockIndex blocks = 
    printfn "create space entry at %d: %d blocks" blockIndex blocks
    { startIndex = blockIndex 
      endIndex = blockIndex + blocks - 1 
      blocks = blocks } |> SpaceEntry

let compact (entries : DiskEntry array) : DiskEntry list =
    let rec loop (leftIndex : int) (rightIndex : int) (compacted : DiskEntry list) =
        printfn "Looping - left: %d, right: %d" leftIndex rightIndex
        let leftEntry = Array.get leftIndex 
        match leftEntry with 
        | DiskEntry inPlaceFile -> 
            loop (leftIndex + 1) rightIndex (inPlaceFile :: compacted)
        | SpaceEntry space -> 
            let rightEntry = Array.get rightIndex 
            match rightEntry with 
            | SpaceEntry _ ->
                loop leftIndex (rightIndex - 1) compacted
            | DiskEntry fileToMove -> 
                // Move (parts of) file.
                if fileToMove.blocks = space.blocks then 
                    // Perfect match.
                    loop (leftIndex + 1) (rightIndex - 1) (fileToMove :: compacted)
                else if fileToMove.blocks < space.blocks  
                    // More than enough space.
                    let leftoverSpace = 
                        { startIndex = space.startIndex + fileToMove.blocks
                          endIndex = space.endIndex 
                          blocks = space.blocks - fileToMove.blocks }
                    

        let rightEntry = Array.get rightIndex
        if leftIndex < rightIndex then loop (leftIndex + 1) rightIndex compacted else compacted
    let rightIndex = (Array.length entries) - 1
    loop 0 rightIndex []

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
    let entries = reversedEntries |> List.rev |> List.toArray
    let compacted = compact entries 

    0
    // digits |> List.length |> printfn "%d"

run "sample"
