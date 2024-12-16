module Aoc05 exposing (..)

import Browser
import Html exposing (Html)
import Html.Attributes
import Html.Events exposing (onClick)
import Dict exposing (Dict)
import Array exposing (Array)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Time

defaultDelay : Float
defaultDelay = 200

crateSize : Int 
crateSize = 10

-- MAIN

main =
  Browser.element
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions }

-- MODEL

type alias Crate = String 

type alias Crates = List Crate 

type alias Stack = List Crate 

type alias StackId = Int 

type alias Stacks = Dict StackId Stack 

type alias Command =
    { amount : Int 
    , source : StackId
    , target : StackId }

type alias Model = 
  { stacks : Stacks
  , tallestStackSeen : Int 
  , commands : List Command
  , lastCommandText : String
  , totalCommands : Int
  , delay : Float
  , paused : Bool  
  , keepOrder : Bool
  , counter : Int 
  , debug : String }

splitInput : String -> (String, String)
splitInput input = 
  case input |> String.split "\n\n" of 
    a::b::_ -> (a, b)
    _ -> ("", "")

parseStackIdLine : String -> List Int 
parseStackIdLine s = 
  s |> String.words |> List.map (String.toInt) |> List.map (Maybe.withDefault 0)

tryReadCrate : Int -> String -> Maybe String 
tryReadCrate index str = 
  case str |> String.slice index (index + 1) of 
    "" -> Nothing 
    " " -> Nothing 
    s -> Just s

toStackIndex : Int -> Int 
toStackIndex stackId = 
  1 + 4 * (stackId - 1)

initStack : List String -> Int -> (Int, Stack)
initStack crateLines stackId = 
  let 
    stackIndex = toStackIndex stackId 
    stack = crateLines |> List.filterMap (tryReadCrate stackIndex) |> List.reverse
  in 
    (stackId, stack)

parseStacks : String -> Stacks
parseStacks s = 
  let 
    lines = s |> String.split "\n"
  in 
    case List.reverse lines of 
      [] -> 
        Dict.empty 
      stackIdLine :: crateLines -> 
        let
          stackIds = parseStackIdLine stackIdLine
          stacks = stackIds |> List.map (initStack crateLines)
        in         
          Dict.fromList stacks

parseCommand : String -> Maybe Command 
parseCommand s = 
  let 
    nums = s |> String.words |> List.filterMap String.toInt 
  in 
    case nums of 
      amount::source::target::_ -> Just {amount=amount, source=source, target=target}
      _ -> Nothing
     

parseCommands : String -> List Command
parseCommands s = 
  s |> String.split "\n" |> List.filterMap parseCommand
  
init : () -> (Model, Cmd Msg)
init _ =
  let 
    sample = """    [D]    
[N] [C]    
[Z] [M] [P]
 1   2   3 

move 1 from 2 to 1
move 3 from 1 to 3
move 2 from 2 to 1
move 1 from 1 to 2"""
    input = """            [J]             [B] [W]
            [T]     [W] [F] [R] [Z]
        [Q] [M]     [J] [R] [W] [H]
    [F] [L] [P]     [R] [N] [Z] [G]
[F] [M] [S] [Q]     [M] [P] [S] [C]
[L] [V] [R] [V] [W] [P] [C] [P] [J]
[M] [Z] [V] [S] [S] [V] [Q] [H] [M]
[W] [B] [H] [F] [L] [F] [J] [V] [B]
 1   2   3   4   5   6   7   8   9 

move 3 from 5 to 7
move 2 from 8 to 9
move 4 from 3 to 5
move 2 from 1 to 7
move 1 from 3 to 6
move 2 from 1 to 7
move 1 from 8 to 7
move 4 from 2 to 8
move 10 from 9 to 1
move 6 from 6 to 2
move 1 from 6 to 7
move 9 from 8 to 6
move 4 from 2 to 4
move 2 from 4 to 1
move 6 from 1 to 6
move 1 from 3 to 2
move 2 from 1 to 4
move 2 from 4 to 3
move 2 from 1 to 3
move 4 from 3 to 1
move 15 from 7 to 9
move 4 from 5 to 9
move 13 from 9 to 4
move 10 from 4 to 8
move 1 from 7 to 4
move 6 from 9 to 5
move 11 from 6 to 7
move 4 from 5 to 7
move 3 from 8 to 7
move 4 from 2 to 4
move 1 from 5 to 1
move 5 from 8 to 4
move 1 from 5 to 4
move 10 from 7 to 1
move 8 from 7 to 9
move 12 from 1 to 9
move 8 from 9 to 1
move 2 from 6 to 9
move 2 from 8 to 4
move 1 from 6 to 9
move 13 from 4 to 2
move 13 from 4 to 2
move 1 from 6 to 1
move 1 from 6 to 4
move 1 from 4 to 5
move 14 from 1 to 8
move 1 from 5 to 4
move 13 from 9 to 5
move 9 from 8 to 2
move 8 from 2 to 1
move 5 from 8 to 2
move 5 from 1 to 6
move 3 from 1 to 3
move 1 from 4 to 8
move 9 from 5 to 9
move 18 from 2 to 8
move 3 from 3 to 5
move 2 from 6 to 4
move 14 from 2 to 7
move 1 from 4 to 2
move 1 from 6 to 9
move 1 from 2 to 5
move 1 from 6 to 2
move 1 from 4 to 6
move 6 from 8 to 1
move 2 from 6 to 9
move 5 from 5 to 3
move 1 from 7 to 8
move 10 from 9 to 7
move 13 from 8 to 5
move 5 from 5 to 2
move 6 from 5 to 7
move 1 from 8 to 5
move 5 from 5 to 9
move 5 from 9 to 7
move 4 from 3 to 8
move 6 from 1 to 6
move 4 from 2 to 4
move 3 from 7 to 5
move 2 from 2 to 9
move 1 from 3 to 7
move 29 from 7 to 9
move 4 from 5 to 2
move 5 from 6 to 4
move 3 from 7 to 9
move 3 from 8 to 6
move 1 from 2 to 6
move 3 from 2 to 5
move 1 from 8 to 4
move 1 from 5 to 9
move 8 from 4 to 9
move 15 from 9 to 2
move 1 from 5 to 1
move 10 from 9 to 4
move 5 from 4 to 5
move 5 from 5 to 4
move 1 from 1 to 9
move 1 from 4 to 3
move 8 from 2 to 4
move 7 from 2 to 7
move 1 from 3 to 8
move 1 from 5 to 6
move 4 from 7 to 3
move 1 from 8 to 2
move 7 from 4 to 7
move 11 from 9 to 7
move 5 from 4 to 2
move 3 from 9 to 6
move 3 from 3 to 8
move 4 from 2 to 4
move 5 from 9 to 5
move 1 from 2 to 1
move 3 from 8 to 5
move 2 from 9 to 1
move 1 from 2 to 5
move 2 from 9 to 6
move 3 from 7 to 5
move 7 from 4 to 1
move 4 from 4 to 9
move 3 from 7 to 2
move 3 from 1 to 9
move 1 from 2 to 3
move 2 from 7 to 9
move 6 from 5 to 4
move 6 from 4 to 3
move 5 from 5 to 1
move 6 from 7 to 8
move 1 from 5 to 1
move 2 from 9 to 4
move 1 from 4 to 3
move 10 from 6 to 4
move 2 from 2 to 1
move 6 from 4 to 1
move 5 from 8 to 3
move 1 from 8 to 2
move 7 from 3 to 9
move 1 from 6 to 9
move 2 from 7 to 3
move 20 from 1 to 6
move 7 from 3 to 8
move 2 from 9 to 6
move 1 from 2 to 3
move 2 from 3 to 6
move 1 from 1 to 4
move 6 from 4 to 7
move 5 from 8 to 3
move 22 from 6 to 4
move 2 from 9 to 7
move 3 from 3 to 4
move 6 from 4 to 2
move 11 from 9 to 3
move 9 from 3 to 7
move 5 from 4 to 2
move 5 from 7 to 2
move 5 from 7 to 6
move 10 from 2 to 4
move 3 from 2 to 1
move 1 from 6 to 3
move 1 from 1 to 7
move 17 from 4 to 1
move 1 from 8 to 4
move 2 from 7 to 5
move 3 from 2 to 5
move 3 from 3 to 8
move 4 from 5 to 1
move 3 from 3 to 7
move 1 from 4 to 5
move 21 from 1 to 5
move 3 from 8 to 3
move 4 from 7 to 5
move 1 from 1 to 7
move 1 from 6 to 3
move 4 from 4 to 1
move 1 from 8 to 1
move 3 from 4 to 9
move 5 from 1 to 8
move 3 from 9 to 3
move 5 from 6 to 1
move 5 from 1 to 4
move 6 from 3 to 2
move 1 from 3 to 2
move 3 from 8 to 1
move 7 from 2 to 1
move 10 from 5 to 2
move 12 from 5 to 7
move 2 from 8 to 3
move 5 from 5 to 8
move 8 from 1 to 6
move 5 from 4 to 5
move 3 from 8 to 6
move 1 from 8 to 3
move 6 from 6 to 7
move 2 from 3 to 8
move 3 from 2 to 1
move 6 from 2 to 9
move 2 from 8 to 4
move 1 from 3 to 9
move 1 from 8 to 6
move 1 from 6 to 9
move 7 from 9 to 5
move 1 from 9 to 7
move 1 from 4 to 6
move 2 from 6 to 5
move 1 from 4 to 1
move 1 from 2 to 7
move 5 from 1 to 2
move 10 from 7 to 4
move 12 from 5 to 7
move 6 from 4 to 8
move 2 from 5 to 6
move 1 from 8 to 9
move 1 from 9 to 5
move 30 from 7 to 9
move 4 from 8 to 4
move 1 from 8 to 7
move 2 from 1 to 4
move 6 from 6 to 3
move 1 from 4 to 1
move 1 from 1 to 2
move 8 from 4 to 8
move 1 from 4 to 5
move 2 from 5 to 6
move 2 from 9 to 8
move 3 from 2 to 1
move 4 from 3 to 2
move 1 from 6 to 4
move 1 from 7 to 1
move 2 from 8 to 2
move 1 from 9 to 2
move 2 from 3 to 2
move 1 from 4 to 2
move 4 from 9 to 6
move 3 from 6 to 4
move 21 from 9 to 8
move 13 from 2 to 7
move 9 from 8 to 5
move 3 from 1 to 4
move 14 from 7 to 2
move 5 from 8 to 9
move 1 from 1 to 2
move 7 from 8 to 6
move 2 from 8 to 2
move 8 from 6 to 9
move 1 from 4 to 5
move 5 from 8 to 2
move 4 from 5 to 9
move 9 from 9 to 6
move 2 from 7 to 6
move 1 from 8 to 7
move 9 from 6 to 4
move 1 from 6 to 5
move 1 from 7 to 3
move 1 from 4 to 7
move 1 from 7 to 2
move 9 from 2 to 3
move 8 from 4 to 1
move 8 from 9 to 2
move 2 from 6 to 5
move 4 from 5 to 2
move 2 from 9 to 5
move 1 from 4 to 9
move 10 from 3 to 7
move 1 from 9 to 2
move 1 from 5 to 3
move 7 from 2 to 8
move 7 from 1 to 5
move 1 from 1 to 2
move 2 from 8 to 2
move 1 from 3 to 5
move 2 from 8 to 6
move 2 from 8 to 9
move 2 from 4 to 6
move 3 from 2 to 8
move 3 from 6 to 7
move 7 from 5 to 8
move 7 from 2 to 7
move 1 from 6 to 8
move 5 from 2 to 7
move 6 from 8 to 3
move 2 from 7 to 1
move 7 from 2 to 5
move 1 from 3 to 5
move 1 from 1 to 5
move 2 from 9 to 7
move 4 from 3 to 7
move 2 from 4 to 6
move 1 from 1 to 6
move 1 from 2 to 4
move 16 from 5 to 6
move 1 from 4 to 9
move 19 from 6 to 1
move 1 from 3 to 5
move 1 from 9 to 1
move 1 from 8 to 5
move 5 from 8 to 3
move 5 from 7 to 2
move 3 from 2 to 9
move 5 from 1 to 7
move 2 from 5 to 1
move 3 from 9 to 4
move 4 from 1 to 9
move 2 from 2 to 8
move 2 from 8 to 6
move 1 from 6 to 9
move 4 from 3 to 8
move 4 from 8 to 3
move 2 from 3 to 8
move 1 from 8 to 2
move 1 from 9 to 7
move 10 from 1 to 7
move 26 from 7 to 6
move 3 from 9 to 3
move 1 from 4 to 6
move 2 from 1 to 4
move 1 from 1 to 6
move 1 from 9 to 3
move 1 from 2 to 3
move 4 from 4 to 9
move 10 from 7 to 8
move 3 from 7 to 4
move 4 from 9 to 4
move 4 from 4 to 7
move 4 from 3 to 9
move 5 from 7 to 5
move 3 from 5 to 1
move 3 from 9 to 8
move 3 from 1 to 5
move 2 from 3 to 5
move 7 from 8 to 1
move 7 from 8 to 9
move 4 from 6 to 3
move 3 from 3 to 6
move 1 from 3 to 4
move 2 from 4 to 1
move 1 from 9 to 6
move 4 from 1 to 3
move 3 from 5 to 1
move 1 from 5 to 2
move 6 from 1 to 2
move 6 from 2 to 7
move 2 from 7 to 4
move 1 from 2 to 6
move 1 from 1 to 4
move 3 from 5 to 7
move 6 from 7 to 4
move 1 from 9 to 3
move 1 from 3 to 6
move 4 from 4 to 3
move 9 from 6 to 1
move 10 from 1 to 6
move 7 from 4 to 5
move 28 from 6 to 4
move 3 from 6 to 7
move 3 from 3 to 8
move 4 from 5 to 7
move 1 from 8 to 4
move 18 from 4 to 7
move 8 from 7 to 6
move 6 from 4 to 1
move 2 from 5 to 4
move 8 from 6 to 1
move 2 from 8 to 9
move 1 from 5 to 3
move 1 from 9 to 1
move 5 from 9 to 2
move 2 from 9 to 3
move 1 from 2 to 5
move 2 from 1 to 5
move 6 from 7 to 5
move 1 from 6 to 4
move 6 from 5 to 9
move 2 from 4 to 1
move 8 from 1 to 8
move 4 from 9 to 7
move 1 from 5 to 6
move 1 from 1 to 6
move 2 from 1 to 2
move 1 from 9 to 7
move 3 from 2 to 4
move 2 from 8 to 3
move 5 from 8 to 2
move 4 from 2 to 5
move 1 from 8 to 9
move 12 from 3 to 2
move 2 from 6 to 2
move 12 from 2 to 4
move 6 from 2 to 3
move 4 from 1 to 9
move 8 from 4 to 7
move 3 from 3 to 4
move 1 from 5 to 4
move 5 from 9 to 6
move 3 from 5 to 8
move 1 from 9 to 1
move 2 from 8 to 5
move 3 from 5 to 6
move 1 from 8 to 4
move 4 from 7 to 8
move 1 from 1 to 3
move 2 from 8 to 3
move 7 from 6 to 7
move 1 from 3 to 7
move 2 from 8 to 6
move 22 from 7 to 8
move 6 from 4 to 8
move 5 from 8 to 6
move 5 from 6 to 2
move 4 from 2 to 3
move 6 from 8 to 5
move 4 from 4 to 7
move 1 from 3 to 7
move 4 from 4 to 5
move 1 from 5 to 4
move 2 from 6 to 5
move 9 from 5 to 6
move 10 from 6 to 7
move 1 from 2 to 1
move 3 from 4 to 8
move 16 from 7 to 9
move 1 from 7 to 8
move 1 from 1 to 8
move 1 from 8 to 3
move 2 from 7 to 4
move 15 from 8 to 1
move 1 from 8 to 1
move 4 from 8 to 4
move 7 from 9 to 7
move 3 from 5 to 9
move 10 from 9 to 6
move 2 from 9 to 2
move 7 from 7 to 4
move 9 from 3 to 2
move 8 from 2 to 7
move 1 from 8 to 4
move 3 from 2 to 1
move 9 from 7 to 1
move 9 from 4 to 1
move 2 from 7 to 5
move 1 from 5 to 4
move 1 from 5 to 2
move 6 from 1 to 3
move 16 from 1 to 2
move 9 from 2 to 1
move 5 from 6 to 9
move 2 from 1 to 9
move 1 from 2 to 5
move 4 from 4 to 8
move 2 from 8 to 2
move 2 from 2 to 3
move 17 from 1 to 2
move 2 from 1 to 9
move 13 from 2 to 8
move 1 from 2 to 4
move 11 from 8 to 3
move 3 from 3 to 4
move 3 from 9 to 2
move 1 from 5 to 2
move 1 from 9 to 3
move 3 from 4 to 3
move 1 from 4 to 9
move 3 from 3 to 4
move 1 from 8 to 7
move 7 from 2 to 9
move 3 from 1 to 7
move 3 from 2 to 8
move 3 from 7 to 9
move 10 from 3 to 5
move 3 from 6 to 9
move 8 from 9 to 4
move 1 from 2 to 1
move 1 from 7 to 9
move 2 from 2 to 3
move 4 from 4 to 8
move 1 from 6 to 2
move 7 from 5 to 3
move 1 from 5 to 2
move 9 from 8 to 9
move 12 from 3 to 8
move 1 from 1 to 9
move 9 from 8 to 6
move 1 from 5 to 7
move 1 from 5 to 4
move 2 from 2 to 9
move 1 from 2 to 6
move 2 from 4 to 3
move 9 from 4 to 8
move 6 from 3 to 6
move 12 from 6 to 2
move 2 from 6 to 7
move 8 from 8 to 3
move 5 from 8 to 7
move 3 from 6 to 5
move 6 from 3 to 7
move 6 from 7 to 6
move 1 from 4 to 9
move 4 from 6 to 5
move 20 from 9 to 6
move 4 from 9 to 8
move 2 from 8 to 7
move 4 from 6 to 4
move 10 from 6 to 1
""" 
    (stacksStr, commandsStr) = input |> String.trimRight |> splitInput

    parsedStacks = parseStacks stacksStr
    parsedCommands = parseCommands commandsStr

    model = { stacks = parsedStacks
            , tallestStackSeen = 0
            , commands = parsedCommands
            , lastCommandText = "press play to start"
            , totalCommands = List.length parsedCommands
            , delay = defaultDelay
            , paused = True
            , keepOrder = False
            , counter = 0
            , debug = "" }
  in 
    (model, Cmd.none)

-- UPDATE

type Msg = Tick | Step | TogglePlay | Faster | Slower | ToggleMachine

takeAmount : Int -> Crates -> Stack -> (Crates, Stack) 
takeAmount amount taken stack =
    if amount > 0 then 
        case stack of 
            [] -> (taken, stack) 
            top :: rest -> 
                takeAmount (amount - 1) (top :: taken) rest 
    else 
        (taken, stack)

moveAmount : Bool -> Int -> Int -> Int -> Stacks -> Stacks
moveAmount keepOrder amount source target stacks = 
    case takeAmount amount [] (Dict.get source stacks |> Maybe.withDefault []) of 
        (taken, stack) -> 
            stacks 
            |> Dict.update source (Maybe.map (\old -> stack))
            |> Dict.update target (Maybe.map (\old -> (if keepOrder then taken |> List.reverse else taken) ++ old))

runCommand : Bool -> Command -> Stacks -> Stacks 
runCommand keepOrder command stacks = 
    moveAmount keepOrder command.amount command.source command.target stacks

toCommandText : Command -> String 
toCommandText cmd = 
  "moved " ++ String.fromInt cmd.amount ++ " from " ++ String.fromInt cmd.source ++ " to " ++ String.fromInt cmd.target

getTallestStack : Stacks -> Int 
getTallestStack stacks = 
  stacks |> Dict.values |> List.map (List.length) |> List.maximum |> Maybe.withDefault 0 

toTopCrateText : Stacks -> String 
toTopCrateText stacks = 
  stacks |> Dict.values |> List.filterMap (List.head) |> String.concat 

updateModel : Model -> Model
updateModel model =
  case model.commands of 
    [] -> model 
    cmd :: restCmds -> 
      let 
        updatedStacks = runCommand model.keepOrder cmd model.stacks
        updatedCounter = model.counter + 1 
        lastCommandText = 
          if updatedCounter < model.totalCommands then 
            toCommandText cmd
          else 
            toTopCrateText updatedStacks
        tallestStack = updatedStacks |> getTallestStack
        updatedTallestStack = 
          if tallestStack > model.tallestStackSeen then 
            tallestStack 
          else 
            model.tallestStackSeen
        debugText = 
          if tallestStack > model.tallestStackSeen then 
            "Tallest: " ++ String.fromInt updatedTallestStack ++ " at cmd " ++ String.fromInt updatedCounter
          else 
            model.debug
      in 
        { model | counter = updatedCounter
        , stacks = updatedStacks
        , tallestStackSeen = updatedTallestStack
        , commands = restCmds
        , lastCommandText = lastCommandText
        , debug = debugText }

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Tick ->
      (updateModel model, Cmd.none)
    Step ->
      (updateModel model, Cmd.none)
    TogglePlay -> 
      ({model | paused = not model.paused }, Cmd.none)
    Faster -> 
      ({model | delay = model.delay / 2 }, Cmd.none)
    Slower -> 
      ({model | delay = model.delay * 2 }, Cmd.none)
    ToggleMachine -> 
      ({model | keepOrder = not model.keepOrder }, Cmd.none)

-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
  if model.paused || List.isEmpty model.commands then Sub.none 
  else Time.every model.delay (\_ -> Tick)

-- VIEW

toCrateRect : Int -> Int -> Int -> Html Msg 
toCrateRect yMax stackId crateNo = 
  let 
    w = crateSize
    h = crateSize
    xVal = stackId * 2 * w
    yVal = (1 + crateNo) * h + crateSize * 2
  in
    rect
      [ x (String.fromInt xVal)
      , y (String.fromInt (yMax - yVal))
      , width (String.fromInt w) 
      , height (String.fromInt h)
      , stroke "black"
      , fill "lightgrey" ]
      []

toCrateText : Int -> Int -> Int -> Crate -> Html Msg 
toCrateText yMax stackId crateNo crate = 
  let 
    w = crateSize
    h = crateSize
    xOffset = 2
    yOffset = 8
    xVal = xOffset + stackId * 2 * w
    yVal = (1 + crateNo) * h - yOffset + crateSize * 2
  in
    text_
      [ x (String.fromInt xVal)
      , y (String.fromInt (yMax - yVal))
      , fontSize "9px"
      , fontFamily "monospace" ]
      [ text crate ]

toStackIdText : Int -> Int -> Html Msg 
toStackIdText yMax stackId = 
  let 
    w = crateSize
    h = crateSize
    xOffset = 2
    yOffset = 8
    xVal = xOffset + stackId * 2 * w
    yVal = crateSize
  in
    text_
      [ x (String.fromInt xVal)
      , y (String.fromInt (yMax - yVal))
      , fontSize "9px"
      , fontFamily "monospace" ]
      [ text (String.fromInt stackId) ]

toCrateSvgElements : Int -> Int -> Int -> Crate -> List (Html Msg)
toCrateSvgElements yMax stackId crateNo crate = 
  [ toCrateRect yMax stackId crateNo
  , toCrateText yMax stackId crateNo crate ]

toStackSvgElements : Int -> (Int, Stack) -> List (Html Msg)
toStackSvgElements yMax (stackId, stack) =
  let 
    crateElements = 
      stack 
      |> List.reverse
      |> List.indexedMap (toCrateSvgElements yMax stackId)
      |> List.concat
    labelElement = toStackIdText yMax stackId
  in 
    labelElement :: crateElements

toSvg : Stacks -> Html Msg 
toSvg stacks = 
  let 
    yMax = 500
    lst = stacks |> Dict.toList |> List.concatMap (toStackSvgElements yMax)
  in 
    svg
      [ viewBox "0 0 210 500"
      , width "210"
      , height "500" ]
--      , Svg.Attributes.style "background-color:lightblue" ]
      lst

view : Model -> Html Msg
view model =
  let
    s = toSvg model.stacks
    commandsStr = "commands: " ++ String.fromInt model.counter ++ " of " ++ String.fromInt model.totalCommands
  in 
    Html.table 
      [ 
        Html.Attributes.style "font-family" "Courier New"
      ]
      [ Html.tr 
          [] 
          [ Html.td 
              [ Html.Attributes.align "center"
              , Html.Attributes.style "font-family" "Courier New"
              , Html.Attributes.style "font-size" "40px"
              , Html.Attributes.style "padding" "20px"]
              [ Html.div [] [Html.text "Advent of Code 2022" ]
              , Html.div [] [Html.text "Day 5: Supply Stacks" ]] ]
      , Html.tr 
          []
          [ Html.td 
              [ Html.Attributes.align "center"
              , Html.Attributes.style "padding-bottom" "10px" ]
              [ Html.text " ["
              , Html.a [ Html.Attributes.href "../../2024/"] [ Html.text "2024" ]
              , Html.text "] " 
              , Html.text " ["
              , Html.a [ Html.Attributes.href "../../2023/"] [ Html.text "2023" ]
              , Html.text "] "
              , Html.text " ["
              , Html.a [ Html.Attributes.href "../../2022/"] [ Html.text "2022" ]
              , Html.text "] "
              , Html.text " ["
              , Html.a [ Html.Attributes.href "../../2021/"] [ Html.text "2021" ]
              , Html.text "] "
              , Html.text " ["
              , Html.a [ Html.Attributes.href "../../2020/"] [ Html.text "2020" ]
              , Html.text "] "
            ] ]
      , Html.tr 
          []
          [ Html.td 
              [ Html.Attributes.align "center"
              , Html.Attributes.style "background-color" "white" 
              , Html.Attributes.style "font-family" "Courier New"
              , Html.Attributes.style "font-size" "20px"
              , Html.Attributes.style "padding" "20px"] 
              [ Html.div [ Html.Attributes.align "center" ] [ s ] 
              , Html.div [] [ Html.text commandsStr ]
              , Html.div [] [ Html.text model.lastCommandText ]
              -- , Html.div [] [ Html.text model.debug ]
              ] ] 
      , Html.tr 
          []
          [ Html.td 
              [ Html.Attributes.align "center" ]
              [ Html.button 
                [ Html.Attributes.style "width" "80px", onClick Step ] 
                [ text "Step" ]
              , Html.button 
                [ Html.Attributes.style "width" "80px", onClick Slower ] 
                [ text "Slower" ]
              , Html.button 
                [ Html.Attributes.style "width" "80px", onClick TogglePlay ] 
                [ if model.paused then text "Play" else text "Pause" ] 
              , Html.button 
                [ Html.Attributes.style "width" "80px", onClick Faster ] 
                [ text "Faster" ]
              , Html.button 
                [ Html.Attributes.style "width" "80px", onClick ToggleMachine ] 
                [ if model.keepOrder then text "CM9000" else text "CM9100" ] 
            ] ] ]
