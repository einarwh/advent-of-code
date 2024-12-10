// Advent of Code 2024. Day 09: Disk Fragmenter.
// dotnet fsi aoc09.fsx

open System
open System.IO
open System.Collections.Generic

type DiskEntry = 
    | File of (int * int) 
    | Free of int 

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
        match node with 
        | null -> ()
        | _ -> 
            match node.Value with 
            | Free _ -> 
                loop node.Previous 
            | File (fid, blocks) -> 
                let requiredFreeSpace = if withFragmentation then 1 else blocks
                match tryFindSpace requiredFreeSpace node linked.First with 
                | None -> 
                    if withFragmentation then () else loop node.Previous 
                | Some spaceNode -> 
                    match spaceNode.Value with 
                    | File _ -> failwith "?"
                    | Free freeBlocks -> 
                        if freeBlocks = blocks then 
                            linked.AddBefore(spaceNode, File (fid, blocks)) |> ignore
                            linked.Remove(spaceNode)
                            let next = node.Previous
                            linked.AddBefore(node, Free blocks) |> ignore 
                            linked.Remove(node)
                            loop next
                        else if freeBlocks > blocks then 
                            linked.AddBefore(spaceNode, File (fid, blocks)) |> ignore 
                            linked.AddBefore(spaceNode, Free (freeBlocks - blocks)) |> ignore
                            linked.Remove(spaceNode)
                            let next = node.Previous
                            linked.AddBefore(node, Free blocks) |> ignore 
                            linked.Remove(node)
                            loop next
                        else 
                            linked.AddBefore(spaceNode, File (fid, freeBlocks)) |> ignore 
                            linked.Remove(spaceNode)
                            let remaining = linked.AddBefore(node, File (fid, blocks - freeBlocks))
                            linked.AddBefore(node, Free freeBlocks) |> ignore
                            linked.Remove(node)
                            loop remaining
    loop linked.Last
    linked |> Seq.toList 

let run fileName = 
    let text = File.ReadAllText fileName |> (fun s -> s.Trim()) 
    let digits : int list = text |> Seq.toList |> List.map (fun ch -> int (ch - '0'))
    let accumulateEntries (index : int, entries : DiskEntry list) (blocks : int) : (int * DiskEntry list) = 
        let entry = 
            if index % 2 = 0 then File ((index / 2), blocks)
            else Free blocks
        (index + 1, entry :: entries)
    let (_, reversedEntries) = List.fold accumulateEntries (0, []) digits
    let entries = reversedEntries |> List.rev 
    let checksum (index : int, sum : int64) (entry : DiskEntry) : (int * int64) = 
        match entry with 
        | Free freeBlocks -> 
            (index + freeBlocks, sum)
        | File (fid, fileBlocks) -> 
            let nextIndex = index + fileBlocks
            let fileSum = [ index .. (nextIndex - 1) ] |> List.map int64 |> List.sumBy ((*) (int64 fid))
            (nextIndex, sum + fileSum)
    entries |> compact true |> List.fold checksum (0, 0) |> snd |> printfn "%d"
    entries |> compact false |> List.fold checksum (0, 0) |> snd |> printfn "%d"
    0

run "input"
