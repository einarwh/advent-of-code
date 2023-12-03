module Aoc03 exposing (..)

import Browser
import Html exposing (Html)
import Html.Attributes
import Html.Events exposing (onClick)
import Dict exposing (Dict)
import Array exposing (Array)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Time

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
    (stacksStr, commandsStr) = sample |> String.trimRight |> splitInput

    parsedStacks = parseStacks stacksStr
    parsedCommands = parseCommands commandsStr

    model = { stacks = parsedStacks
            , tallestStackSeen = 0
            , keepOrder = False
            , counter = 0
            , debug = "" }
  in 
    (model, Cmd.none)

-- UPDATE

type Msg = Tick 

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
updateModel model = model

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Tick ->
      (updateModel model, Cmd.none)

-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model = Sub.none

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
      , height "500" 
      , Svg.Attributes.style "background-color:lightblue" ]
      lst

view : Model -> Html Msg
view model =
  let
    s = toSvg model.stacks
  in 
    Html.table 
      []
      [ Html.tr 
          [] 
          [ Html.td 
              [ Html.Attributes.align "center"
              , Html.Attributes.style "font-family" "Courier New"
              , Html.Attributes.style "font-size" "40px"
              , Html.Attributes.style "padding" "20px"]
              [ Html.div [] [Html.text "Advent of Code 2023" ]
              , Html.div [] [Html.text "Day 3: Gear Ratios" ]] ]
      , Html.tr 
          []
          [ Html.td 
              [ Html.Attributes.align "center"
              , Html.Attributes.style "background-color" "white" 
              , Html.Attributes.style "font-family" "Courier New"
              , Html.Attributes.style "font-size" "20px"
              , Html.Attributes.style "padding" "20px"] 
              [ Html.div [ Html.Attributes.align "center" ] [ s ] 
              , Html.div [] [ Html.text "." ]
              , Html.div [] [ Html.text "." ]
              , Html.div [] [ Html.text model.debug ]
              ] ] ]
