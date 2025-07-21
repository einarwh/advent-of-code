// Advent of Code 2015. Day 04: The Ideal Stocking Stuffer.
// dotnet fsi aoc04.fsx

open System
open System.IO
open System.Text
open System.Security.Cryptography

let toHexHash (input : string) = 
    use md5 = MD5.Create()
    input |> Encoding.ASCII.GetBytes |> md5.ComputeHash |> Convert.ToHexString

let findNumber (prefix : string) (secretKey : string) = 
    let rec fn (index : int) = 
        let input = secretKey + index.ToString()
        let hash = toHexHash input
        if hash.StartsWith prefix then 
            index 
        else 
            fn (index + 1)
    fn 0

let readText fileName = 
    File.ReadAllText(fileName).Trim()

let run fileName = 
    let secretKey = readText fileName
    secretKey |> findNumber "00000" |> printfn "%d"
    secretKey |> findNumber "000000" |> printfn "%d"

run "input.txt"
