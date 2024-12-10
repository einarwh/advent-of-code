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
    let diskFiles = entries |> List.choose (function | FileEntry diskFile -> Some diskFile | _ -> None)
    let freeSpaces = entries |> List.choose (function | SpaceEntry freeSpace -> Some freeSpace | _ -> None)
    diskFiles |> printfn "%A"
    freeSpaces |> printfn "%A"
    let rec compactDiskFiles (diskFile : DiskFile)  (diskFiles : DiskFile list, freeSpaces : FreeSpace list, compacted : DiskFile list) : (DiskFile list * FreeSpace list * DiskFile list) = 
        printfn "compactDiskFiles with %d"
        match diskFiles, freeSpaces with 
        | ([], []) -> ([], [], diskFile :: compacted)
        | (file :: restFiles, []) -> compactDiskFiles diskFile (restFiles, [], (file :: compacted))
        | ([],  space :: restSpaces) ->
            if diskFile.blocks = space.blocks then 
                ([], restSpaces, diskFile :: compacted)
            else if diskFile.blocks < space.blocks then 
                let leftoverSpace = 
                    { startIndex = space.startIndex + diskFile.blocks
                      endIndex = space.endIndex 
                      blocks = space.blocks - diskFile.blocks }
                ([], (leftoverSpace :: restSpaces), diskFile :: compacted)
            else // diskFile.blocks > space.blocks
                let placedFile = 
                    { fileId = diskFile.fileId
                      startIndex = diskFile.startIndex
                      endIndex = diskFile.startIndex + space.blocks 
                      blocks = space.blocks }
                let leftoverFile = 
                    { fileId = diskFile.fileId
                      startIndex = diskFile.startIndex + space.blocks
                      endIndex = diskFile.endIndex 
                      blocks = diskFile.blocks - space.blocks }
                compactDiskFiles leftoverFile ([], restSpaces, placedFile :: compacted) 
        | (file :: restFiles, space :: restSpaces) -> 
            printfn "Compact with file %d at index %d and space at index %d" file.fileId file.startIndex space.startIndex
            if file.startIndex < space.startIndex then 
                compactDiskFiles diskFile (restFiles, freeSpaces, file :: compacted)  
            else 
                if diskFile.blocks = space.blocks then 
                    ([], restSpaces, diskFile :: compacted)
                else if diskFile.blocks < space.blocks then 
                    let leftoverSpace = 
                        { startIndex = space.startIndex + diskFile.blocks
                          endIndex = space.endIndex 
                          blocks = space.blocks - diskFile.blocks }
                    ([], (leftoverSpace :: restSpaces), diskFile :: compacted)
                else // diskFile.blocks > space.blocks
                    let placedFile = 
                        { fileId = diskFile.fileId
                          startIndex = diskFile.startIndex
                          endIndex = diskFile.startIndex + space.blocks 
                          blocks = space.blocks }
                    let leftoverFile = 
                        { fileId = diskFile.fileId
                          startIndex = diskFile.startIndex + space.blocks
                          endIndex = diskFile.endIndex 
                          blocks = diskFile.blocks - space.blocks }
                    compactDiskFiles leftoverFile ([], restSpaces, placedFile :: compacted) 
    let (_, _, compacted) = List.foldBack compactDiskFiles diskFiles (diskFiles, freeSpaces, []) 
    compacted |> List.iter (printfn "%A")

    0
    // digits |> List.length |> printfn "%d"

run "sample"
