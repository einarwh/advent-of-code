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

type alias SymbolBox = 
    { xMin : Int 
    , xMax : Int 
    , yMin : Int 
    , yMax : Int }

type alias Pos = 
    { x : Int
    , y : Int }

type alias SymbolPos = 
    { x : Int
    , y : Int }

type alias NumberBox = 
    { xStart : Int
    , xEnd : Int 
    , y : Int }

type alias Crate = String 

type alias Crates = List Crate 

type alias Stack = List Crate 

type alias StackId = Int 

type alias Stacks = Dict StackId Stack 

type alias Model = 
  { stacks : Stacks
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
       
init : () -> (Model, Cmd Msg)
init _ =
  let 
    sample = """467..114..
...*......
..35..633.
......#...
617*......
.....+.58.
..592.....
......755.
...$.*....
.664.598.."""

    lines = sample |> String.split "\n"
    debugStr = lines |> List.length |> String.fromInt

    cratesample = """    [D]    
[N] [C]    
[Z] [M] [P]
 1   2   3 

move 1 from 2 to 1
move 3 from 1 to 3
move 2 from 2 to 1
move 1 from 1 to 2"""
    (stacksStr, commandsStr) = cratesample |> String.trimRight |> splitInput

    parsedStacks = parseStacks stacksStr

    model = { stacks = parsedStacks
            , counter = 0
            , debug = debugStr }
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
