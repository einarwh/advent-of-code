// Advent of Code 2023. Day 20: ?
// dotnet fsi aoc20.fsx

open System
open System.IO

type Pulse = High | Low 

type Label = string 

[<AutoOpen>]
module ModuleTypes =

    type FlipFlopState = On | Off 

    type FlipFlop = {
        label : Label 
        state : FlipFlopState
    }

    type ConjunctionState = Map<Label, Pulse>

    type Conjunction = {
        label : Label 
        state : ConjunctionState 
        inputs : Label list 
        outputs : Label list 
    }

    type Broadcaster = Broadcaster 


module FlipFlop = 

    let receive (flipFlop : FlipFlop) (pulse : Pulse) = 
        match pulse with 
        | High -> (flipFlop, [])
        | Low -> 
            match flipFlop.state with 
            | On -> ({ flipFlop with state = Off }, Low)
            | Off -> ({ flipFlop with state = On }, High)

module Conjunction = 

    let receive (conjunction : Conjunction) (sender : Label) (pulse : Pulse) = 
        let state = conjunction.state |> Map.add 


let readLines =
    File.ReadAllLines >> Array.filter ((<>) String.Empty)

let run fileName =
    let lines = readLines fileName
    ()

"sample" |> run