module Aoc10 exposing (..)

import Browser
import Html exposing (Html)
import Html.Attributes
import Html.Events exposing (onClick)
import Dict exposing (Dict)
import Array exposing (Array)
import Svg exposing (..)
import Svg.Attributes exposing (..)

unitSize : Int 
unitSize = 9

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

type alias Model = 
  { lines : List String
  , debug : String }

init : () -> (Model, Cmd Msg)
init _ =
  let 
    sample = """...........
.S-------7.
.|F-----7|.
.||.....||.
.||.....||.
.|L-7.F-J|.
.|..|.|..|.
.L--J.L--J.
..........."""

    lines = sample |> String.split "\n"

    model = { lines = lines
            , debug = "..." }
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

toCharText : Int -> Int -> Char -> Html Msg 
toCharText yPos xPos ch = 
  let 
    xOffset = 2
    yOffset = 8
    xVal = xOffset + xPos * unitSize
    yVal = yOffset + yPos * unitSize
  in
    text_
      [ x (String.fromInt xVal)
      , y (String.fromInt yVal)
      , fontSize "9px"
      , fontFamily "monospace" ]
      [ text (String.fromChar ch) ]

toColoredBox : String -> Box -> Html Msg 
toColoredBox fillColor box = 
  let 
    xDiff = box.xMax - box.xMin + 1
    yDiff = box.yMax - box.yMin + 1
    w = unitSize * xDiff 
    h = unitSize * yDiff
    xVal = unitSize * box.xMin
    yVal = unitSize * box.yMin
  in
    rect
      [ x (String.fromInt xVal)
      , y (String.fromInt yVal)
      , width (String.fromInt w) 
      , height (String.fromInt h)
    --   , stroke "black"
      , opacity "0.6"
      , fill fillColor ]
      []

lineToCharBox : Int -> String -> List (Html Msg)
lineToCharBox y line =  
    line |> String.toList |> List.indexedMap (toCharText y)

toSvg : Model -> Html Msg 
toSvg model = 
  let 
    lines = model.lines
    charBoxes = lines |> List.indexedMap lineToCharBox |> List.concat
    numberOfLines = lines |> List.length
    numberOfChars = lines |> List.head |> Maybe.map (String.length) |> Maybe.withDefault 0
    svgWidth = (unitSize * numberOfChars) |> String.fromInt
    svgHeight = (unitSize * numberOfLines) |> String.fromInt
  in 
    svg
      [ viewBox ("0 0 " ++ svgWidth ++ svgHeight)
      , width svgWidth
      , height svgHeight
      , Svg.Attributes.style "background-color:pink" ]
      charBoxes

view : Model -> Html Msg
view model =
  let
    s = toSvg model
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
              , Html.div [] [Html.text "Day 10: Pipe Maze" ]] ]
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
              , Html.div [] [ Html.text ".." ]
              , Html.div [] [ Html.text "..." ]
              ] ] ]
