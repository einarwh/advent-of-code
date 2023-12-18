module Aoc18 exposing (..)

import Browser
import Html exposing (Html)
import Html.Attributes
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Array2D exposing (Array2D)

inputname : String
inputname = "sample"

unitSize : Int
unitSize = 8

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

type alias Position = (Int, Int)

type alias Direction = (Int, Int)

type alias Instruction = 
  { dir : Direction
  , meters : Int }

toDirection : String -> Maybe Direction 
toDirection s = 
  case s of 
    "U" -> Just (0, -1)
    "D" -> Just (0, 1)
    "R" -> Just (1, 0) 
    "L" -> Just (-1, 0)
    _ -> Nothing 

parseLine : String -> Instruction 
parseLine s = 
  case String.split " " s of 
    a :: b :: _ -> 
      let 
        d = a |> toDirection |> Maybe.withDefault (0, 0)
        m = b |> String.toInt |> Maybe.withDefault 0
      in { dir = d, meters = m }
    _ -> { dir = (0, 0), meters = 0 }

move : Position -> Direction -> Int -> List Position 
move (xStart, yStart) (xStep, yStep) meters =
  List.range 1 meters |> List.map (\m -> (xStart + xStep * m, yStart + yStep * m))

digLagoonLoop : Position -> List Position -> List Instruction -> List Position 
digLagoonLoop current positions instructions = 
  case instructions of 
    [] -> positions 
    inst :: remaining -> 
      let 
        nextPositions = move current inst.dir inst.meters 
        next = nextPositions |> List.reverse |> List.head |> Maybe.withDefault (0, 0)
      in 
        remaining |> digLagoonLoop next (positions ++ nextPositions)

digLagoon : List Instruction -> List Position 
digLagoon instructions = 
  digLagoonLoop (0, 0) [] instructions 

init : () -> (Model, Cmd Msg)
init _ =
  let 
    input = ""

    sample = """R 6 (#70c710)
D 5 (#0dc571)
L 2 (#5713f0)
D 2 (#d2c081)
R 2 (#59c680)
D 2 (#411b91)
L 5 (#8ceee2)
U 2 (#caa173)
L 1 (#1b58a2)
U 2 (#caa171)
R 2 (#7807d2)
U 3 (#a77fa3)
L 2 (#015232)
U 2 (#7a21e3)"""

    data = if inputname == "sample" then sample else input 

    lines = data |> String.split "\n"

    instructions = lines |> List.map parseLine

    grid = lines |> List.map (String.toList) |> Array2D.fromList

    debugText = instructions |> List.length |> String.fromInt 

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
    
toSvg : Model -> Html Msg 
toSvg model = 
  let 
    svgWidth = (unitSize * 100) |> String.fromInt
    svgHeight = (unitSize * 100) |> String.fromInt
    elements = []
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
              , Html.div [] [Html.text "Day 18: Lavaduct Lagoon" ]] ]
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
