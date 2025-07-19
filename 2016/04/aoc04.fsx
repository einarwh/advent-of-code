// Advent of Code 2016. Day 04: Security Through Obscurity.
// dotnet fsi aoc04.fsx

open System
open System.IO
open System.Text.RegularExpressions

type Room = {
    name : string
    sectorId : int 
    checksum : string
}

let check (s : string) : Room option =
    let m = Regex.Match(s, "^(([a-z]+\-)+)(\d+)\[([a-z]+)\]$")
    if m.Success then
        let name = m.Groups.[1].Value
        let sectorId = int (m.Groups.[3].Value)
        let checksum = m.Groups.[4].Value
        Some { name = name; sectorId = sectorId; checksum = checksum }
    else
        None

let readLines = 
    File.ReadAllLines
    >> Array.filter (fun line -> line <> String.Empty)
    >> Array.toList

let toChecksum (name : string) = 
    name 
    |> Seq.toList 
    |> List.filter (fun ch -> ch <> '-') 
    |> List.countBy id
    |> List.sortByDescending (fun (ch, count) -> 100 * count + int 'z' - int ch)
    |> List.map fst
    |> List.take 5
    |> fun chars -> new string(chars |> List.toArray)

let isRealRoom room = 
    room.checksum = toChecksum room.name

let rotate rotations ch = 
    let aCode = int 'a'
    let zCode = int 'z'
    let rec fn (n : int) (charCode : int) = 
        if n > 0 then 
            let code = if charCode = zCode then aCode else charCode + 1
            fn (n - 1) code
        else 
            char charCode 
    fn rotations ch

let decrypt (rotations : int) (name : string) = 
    name 
    |> Seq.toArray 
    |> Array.map (fun ch -> if ch = '-' then ' ' else rotate rotations (int ch))
    |> fun chars -> new string (chars)

let checkObjectStorage (room : Room) : Room option = 
    let rotations = room.sectorId % 26
    let decrypted = decrypt rotations room.name
    if decrypted.StartsWith "northpole" then Some room else None 

let run fileName = 
    let lines = readLines fileName
    let rooms = lines |> List.choose check
    let realRooms = rooms |> List.filter isRealRoom
    realRooms |> List.map (fun r -> r.sectorId) |> List.sum |> printfn "%d"
    let objectStorage = realRooms |> List.pick checkObjectStorage
    objectStorage.sectorId |> printfn "%d" 

run "input.txt"
