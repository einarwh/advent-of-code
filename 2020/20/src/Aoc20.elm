module Aoc20 exposing (..)

import Browser
import Html exposing (Html)
import Html.Attributes
import Html.Events exposing (onClick)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Tile

inputname : String 
inputname = "input"

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

    input = ""

    data = if inputname == "sample" then sample else input 
    lines = data |> String.split "\n"

    debugText = "?"

    model = { lines = lines 
            , debug = debugText }
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

lineToCharTexts : Int -> String -> List (Html Msg)
lineToCharTexts y line =  
    line |> String.toList |> List.indexedMap (toCharText y)

toColoredBox : String -> (Int, Int) -> Html Msg 
toColoredBox fillColor (xStart, yStart) = 
  let 
    xVal = unitSize * xStart
    yVal = unitSize * yStart
  in
    rect
      [ x (String.fromInt xVal)
      , y (String.fromInt yVal)
      , width (String.fromInt unitSize) 
      , height (String.fromInt unitSize)
      , opacity "0.8"
      , fill fillColor ]
      []

toOutlineBox : (Int, Int) -> Html Msg 
toOutlineBox (xStart, yStart) = 
  let 
    xVal = unitSize * xStart
    yVal = unitSize * yStart
  in
    rect
      [ x (String.fromInt xVal)
      , y (String.fromInt yVal)
      , width (String.fromInt unitSize) 
      , height (String.fromInt unitSize)
      , stroke "red"
      , strokeWidth "1"
      , fill "None"
      , opacity "1" ]
      []

toSvg : Model -> Html Msg 
toSvg model = 
  let 
    lines = model.lines
    svgWidth = (unitSize * 100) |> String.fromInt
    svgHeight = (unitSize * 100) |> String.fromInt
    elements = []
  in 
    svg
      [ viewBox ("0 0 " ++ svgWidth ++ svgHeight)
      , width svgWidth
      , height svgHeight 
      , Svg.Attributes.style "background-color:lightblue" ]
      elements

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
              [ Html.div [] [Html.text "Advent of Code 2020" ]
              , Html.div [] [Html.text "Day 20: Jurassic Jigsaw" ]] ]
      , Html.tr 
          []
          [ Html.td 
              [ Html.Attributes.align "center"
              , Html.Attributes.style "background-color" "pink" 
              , Html.Attributes.style "font-family" "Courier New"
              , Html.Attributes.style "font-size" "20px"
              , Html.Attributes.style "padding" "20px"] 
              [ Html.div [ Html.Attributes.align "center" ] [ s ] 
              , Html.div [] [ Html.text <| "?" ]
              , Html.div [] [ Html.text <| "!" ]
              , Html.div [] [ Html.text model.debug ]
              ] ] ]
