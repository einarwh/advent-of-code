module Aoc16 exposing (..)

import Browser
import Html exposing (Html)
import Html.Attributes
import Html.Events exposing (onMouseEnter, onMouseLeave)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Array2D exposing (Array2D)

inputname : String
inputname = "sample"
mirrorColor : String
mirrorColor = "black"
mirrorStrokeWidth : String
mirrorStrokeWidth = "2"
mirrorOffset : Int
mirrorOffset = 2
unitSize : Int
unitSize = 16

-- MAIN

main =
  Browser.element
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions }

-- MODEL

type alias Model = 
  { grid : Array2D Char
  , lines : List String
  , startArrows : List (Int, Int)
  , debug : String }

init : () -> (Model, Cmd Msg)
init _ =
  let 
    input = ""

    sample = """.|...\\....
|.-.\\.....
.....|-...
........|.
..........
.........\\
..../.\\\\..
.-.-/..|..
.|....-|.\\
..//.|...."""

    data = if inputname == "sample" then sample else input 
    lines = data |> String.split "\n"

    grid = lines |> List.map (String.toList) |> Array2D.fromList

    debugText = ""

    model = { grid = grid 
            , lines = lines 
            , startArrows = []
            , debug = debugText }
  in 
    (model, Cmd.none)

-- UPDATE

type Msg = 
  Tick 
  | EnterBox (Int, Int) 
  | LeaveBox (Int, Int) 

updateModel : Model -> Model
updateModel model = model

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Tick ->
      (model, Cmd.none)
    EnterBox boxPos ->
      let
          arrows = boxPos :: model.startArrows
      in
        ({ model | startArrows = arrows, debug = "enter box" }, Cmd.none)
    LeaveBox boxPos ->
      let
          arrows = model.startArrows |> List.filter (\pos -> pos /= boxPos)
      in
        ({ model | startArrows = arrows, debug = "leave box" }, Cmd.none)

-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model = Sub.none

-- VIEW

toSlashMirrorLine : Int -> Int -> Html Msg
toSlashMirrorLine yPos xPos = 
  let 
    offset = mirrorOffset
    xMin = xPos * unitSize + offset
    yMin = yPos * unitSize + offset 
    xMax = (xPos + 1) * unitSize - offset
    yMax = (yPos + 1) * unitSize - offset
  in
    line 
      [ x1 (String.fromInt xMin)
      , y1 (String.fromInt yMax)
      , x2 (String.fromInt xMax)
      , y2 (String.fromInt yMin)
      , stroke mirrorColor
      , strokeWidth mirrorStrokeWidth
      , strokeLinecap "round" ]
      []

toBackslashMirrorLine : Int -> Int -> Html Msg
toBackslashMirrorLine yPos xPos = 
  let 
    offset = mirrorOffset
    xMin = xPos * unitSize + offset
    yMin = yPos * unitSize + offset 
    xMax = (xPos + 1) * unitSize - offset
    yMax = (yPos + 1) * unitSize - offset
  in
    line 
      [ x1 (String.fromInt xMin)
      , y1 (String.fromInt yMin)
      , x2 (String.fromInt xMax)
      , y2 (String.fromInt yMax)
      , stroke mirrorColor
      , strokeWidth mirrorStrokeWidth
      , strokeLinecap "round" ]
      []

toVerticalMirrorLine : Int -> Int -> Html Msg
toVerticalMirrorLine yPos xPos = 
  let 
    offset = mirrorOffset
    xMid = xPos * unitSize + unitSize // 2
    yMin = yPos * unitSize + offset 
    yMax = (yPos + 1) * unitSize - offset
  in
    line 
      [ x1 (String.fromInt xMid)
      , y1 (String.fromInt yMin)
      , x2 (String.fromInt xMid)
      , y2 (String.fromInt yMax)
      , stroke mirrorColor
      , strokeWidth mirrorStrokeWidth
      , strokeLinecap "round" ]
      []

toHorizontalMirrorLine : Int -> Int -> Html Msg
toHorizontalMirrorLine yPos xPos = 
  let 
    offset = mirrorOffset
    xMin = xPos * unitSize + offset
    xMax = (xPos + 1) * unitSize - offset
    yMid = yPos * unitSize + unitSize // 2
  in
    line 
      [ x1 (String.fromInt xMin)
      , y1 (String.fromInt yMid)
      , x2 (String.fromInt xMax)
      , y2 (String.fromInt yMid)
      , stroke mirrorColor
      , strokeWidth mirrorStrokeWidth
      , strokeLinecap "round" ]
      []

toMaybeMirrorLine : Int -> Int -> Char -> Maybe (Html Msg)
toMaybeMirrorLine yPos xPos ch = 
  case ch of 
    '/' -> Just (toSlashMirrorLine yPos xPos)
    '\\' -> Just (toBackslashMirrorLine yPos xPos)
    '|' -> Just (toVerticalMirrorLine yPos xPos)
    '-' -> Just (toHorizontalMirrorLine yPos xPos)
    _ -> Nothing

lineToMirrorLines : Int -> String -> List (Html Msg)
lineToMirrorLines y line =  
    line 
    |> String.toList 
    |> List.indexedMap (toMaybeMirrorLine y)
    |> List.filterMap identity

toBackgroundBox : Int -> Int -> Html Msg 
toBackgroundBox yPos xPos = 
  let 
    boxId = String.fromInt xPos ++ "," ++ String.fromInt yPos
    xVal = unitSize * xPos
    yVal = unitSize * yPos
  in
    rect
      [ id boxId
      , x (String.fromInt xVal)
      , y (String.fromInt yVal)
      , width (String.fromInt unitSize) 
      , height (String.fromInt unitSize)
      , fill "white"
      , onMouseEnter (EnterBox (xPos, yPos))
      , onMouseLeave (LeaveBox (xPos, yPos)) ]
      []

toArrowElement : Int -> Int -> Html Msg 
toArrowElement yPos xPos = 
  let 
    xVal = unitSize * xPos
    yVal = unitSize * yPos
  in
    rect
      [ x (String.fromInt xVal)
      , y (String.fromInt yVal)
      , width (String.fromInt unitSize) 
      , height (String.fromInt unitSize)
      , stroke "black"
      , fill "purple" ]
      []
    
toSvg : Model -> Html Msg 
toSvg model = 
  let 
    grid = model.grid
    lines = model.lines
    rowCount = Array2D.rows grid 
    colCount = Array2D.columns grid 
    mirrorLines = lines |> List.indexedMap lineToMirrorLines |> List.concat
    backgroundBoxes = 
      List.range 0 (rowCount - 1) 
      |> List.concatMap (\y -> List.range 0 (colCount - 1) |> List.map (toBackgroundBox y))
    highlightBoxes = 
      model.startArrows
      |> List.map (\(x, y) -> toArrowElement y x)

    svgWidth = (unitSize * colCount) |> String.fromInt
    svgHeight = (unitSize * rowCount) |> String.fromInt
    elements = backgroundBoxes ++ highlightBoxes ++ mirrorLines ++ []
    -- elements = insideBoxes ++ loopBoxes ++ pipeShapes ++ [startOutline]
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
              , Html.Attributes.style "font-size" "36px"
              , Html.Attributes.style "padding" "16px"]
              [ Html.div [] [Html.text "Advent of Code 2023" ]
              , Html.div [] [Html.text "Day 16: The Floor Will Be Lava" ]] ]
      , Html.tr 
          []
          [ Html.td 
              [ Html.Attributes.align "center"
              , Html.Attributes.style "background-color" "white" 
              , Html.Attributes.style "font-family" "Courier New"
              , Html.Attributes.style "font-size" "24px"
              , Html.Attributes.style "padding" "16px"] 
              [ Html.div [ Html.Attributes.align "center" ] [ s ] 
              , Html.div [] [ Html.text <| model.debug ]
              , Html.div [] [ Html.text <| model.debug ]
              , Html.div [] [ Html.text model.debug ]
              ] ] ]
