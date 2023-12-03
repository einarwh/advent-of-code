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
import Regex

crateSize : Int 
crateSize = 10

unitWidth : Int 
unitWidth = 10

unitHeight : Int 
unitHeight = 10


-- MAIN

main =
  Browser.element
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions }

-- MODEL

type alias Box = 
    { xMin : Int 
    , xMax : Int 
    , yMin : Int 
    , yMax : Int }

type alias Pos = 
    { x : Int
    , y : Int }

type alias BoxedString = 
    { box : Box 
    , val : String }

type alias PosedString = 
    { pos : Pos 
    , val : String }

type alias Crate = String 

type alias Crates = List Crate 

type alias Stack = List Crate 

type alias StackId = Int 

type alias Stacks = Dict StackId Stack 

type alias Model = 
  { data : String
  , numberBoxes : List Box 
  , symbolBoxes : List Box 
  , counter : Int 
  , debug : String }

parseNumbers : String -> List (Int, String) 
parseNumbers line = 
  let 
    numberRegex = Regex.fromString "[0-9]+" |> Maybe.withDefault Regex.never
    matches = line |> Regex.find numberRegex
  in 
    matches |> List.map (\m -> (m.index, m.match))

toNumberBox : Pos -> String -> Box 
toNumberBox { x, y } s = 
  { xMin = x
  , xMax = x + String.length s - 1
  , yMin = y
  , yMax = y }

parseNumbersWithBox : Int -> String -> List BoxedString 
parseNumbersWithBox y line = 
  line |> parseNumbers |> List.map (\(x, s) -> { box = toNumberBox { x = x, y = y } s, val = s })

parseSymbols : String -> List (Int, String) 
parseSymbols line = 
  let 
    symbolRegex = Regex.fromString "[^\\d\\.]" |> Maybe.withDefault Regex.never
    matches = line |> Regex.find symbolRegex
  in 
    matches |> List.map (\m -> (m.index, m.match))

toSymbolBox : Pos -> Box 
toSymbolBox { x, y } = 
  { xMin = x - 1
  , xMax = x + 1
  , yMin = y - 1
  , yMax = y + 1 }

parseSymbolsWithBox : Int -> String -> List BoxedString 
parseSymbolsWithBox y line = 
  line |> parseSymbols |> List.map (\(x, s) -> { box = toSymbolBox { x = x, y = y }, val = s })

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
    --debugStr = lines |> List.length |> String.fromInt

    boxedNumbers = 
        lines |> List.indexedMap parseNumbersWithBox |> List.concat

    numberBoxes = boxedNumbers |> List.map (\bt -> bt.box)

    boxedSymbols = 
        lines |> List.indexedMap parseSymbolsWithBox |> List.concat

    symbolBoxes = boxedSymbols |> List.map (\bt -> bt.box)

    --debugStr = ".....+.58." |> parseNumbers |> List.length |> String.fromInt

    debugStr = 
        "617*......" 
        |> parseSymbols 
        |> List.map (\(a, b) -> b)
        |> String.concat 

    model = { data = sample
            , symbolBoxes = symbolBoxes
            , numberBoxes = numberBoxes
            , counter = 0
            , debug = debugStr }
  in 
    (model, Cmd.none)

-- UPDATE

type Msg = Tick 

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

toLineText : Int -> Int -> String -> Html Msg 
toLineText yMax i line = 
  let 
    xVal = 40
    yVal = 10 * (i + 1) 
  in
    text_
      [ x (String.fromInt 0)
      , y (String.fromInt yVal)
      , fontSize "12px"
      , fontFamily "monospace" ]
      [ text line ]

toColoredBox : Int -> String -> Box -> Html Msg 
toColoredBox yMax fillColor box = 
  let 
    xDiff = box.xMax - box.xMin 
    yDiff = box.yMax - box.yMin 
    w = unitWidth * xDiff 
    h = unitHeight * (1 + yDiff)
    xVal = unitWidth * box.xMin
    yVal = unitHeight * box.yMin
  in
    rect
      [ x (String.fromInt xVal)
      , y (String.fromInt yVal)
      , width (String.fromInt w) 
      , height (String.fromInt h)
      , stroke "black"
      , opacity "0.6"
      , fill fillColor ]
      []

toSvgX : List Box -> List Box -> String -> Html Msg 
toSvgX numberBoxes symbolBoxes data = 
  let 
    yMax = 500
    numberRects = numberBoxes |> List.map (toColoredBox yMax "pink")
    symbolRects = symbolBoxes |> List.map (toColoredBox yMax "lightgreen")
    --texts = [ "467..114..", "...*......", "..35..633." ] |> List.indexedMap (toLineText yMax)
    dataText = data |> String.split "\n" |> List.indexedMap (toLineText yMax)
    lst = numberRects ++ symbolRects ++ dataText
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
    s = toSvgX model.numberBoxes model.symbolBoxes model.data
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
