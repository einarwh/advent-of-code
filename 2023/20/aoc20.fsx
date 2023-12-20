// Advent of Code 2023. Day 20: Pulse Propagation.
// dotnet fsi aoc20.fsx

open System
open System.IO

type Pulse = High | Low 

type Label = string 

type FlipFlopState = On | Off 

type ConjunctionMemory = Map<Label, Pulse>

type FlipFlop = {
    label : Label 
    inputs : Label list 
    outputs : Label list 
    state : FlipFlopState
}

type Conjunction = {
    label : string 
    inputs : Label list 
    outputs : Label list 
    memory : ConjunctionMemory
}

type Broadcaster = {
    input : Label
    outputs : Label list 
}

type Button = {
    output : Label 
}

type Module = 
    | FlipFlopModule of FlipFlop
    | ConjunctionModule of Conjunction
    | BroadcasterModule of Broadcaster
    | ButtonModule of Button

module FlipFlop = 

    let receive (flipFlop : FlipFlop) (inPulse : Pulse) : (FlipFlop * Pulse option) = 
        match inPulse with 
        | High -> (flipFlop, None)
        | Low -> 
            match flipFlop.state with 
            | On -> ({ flipFlop with state = Off }, Some Low)
            | Off -> ({ flipFlop with state = On }, Some High)

module Conjunction = 

    let receive (conjunction : Conjunction) (sender : Label) (inPulse : Pulse) = 
        let memory = conjunction.memory |> Map.add sender inPulse 
        let outPulse = if memory |> Map.forall (fun _ p -> p = High) then Low else High 
        let conj = { conjunction with memory = memory }
        (conj, outPulse)

module Broadcaster = 

    let receive (broadcaster : Broadcaster) (inPulse : Pulse) = 
        (broadcaster, inPulse)


let readLines =
    File.ReadAllLines >> Array.filter ((<>) String.Empty)

let run fileName =
    let lines = readLines fileName
    ()

"sample" |> run