module Aoc18 exposing (..)

import Browser
import Html exposing (Html)
import Html.Attributes
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Array2D exposing (Array2D)
import Set exposing (Set)
import Time 

delay : Float
delay = 500

inputname : String
inputname = "sample"

-- MAIN

main =
  Browser.element
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions }

-- MODEL

type alias Position = (Int, Int)

type alias Direction = (Int, Int)

type alias Instruction = 
  { dir : Direction
  , meters : Int }

type alias Basin = 
  { startPoint : Position 
  , trench : Set Position
  , explorationPoints : List Position
  , filledPoints : Set Position
  , cubicMeters : Int }

type alias Model = 
  { basin : Basin
  , debug : String }

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

adjustLagoon : List Position -> List Position
adjustLagoon positions = 
  let 
    xs = positions |> List.map (Tuple.first)
    ys = positions |> List.map (Tuple.second)
    xMin = xs |> List.minimum |> Maybe.withDefault 0
    yMin = ys |> List.minimum |> Maybe.withDefault 0
    xOffset = 0 - xMin 
    yOffset = 0 - yMin 
  in 
    positions |> List.map (\(x, y) -> (x + xOffset, y + yOffset))

findStartPosition : List Position -> Position
findStartPosition positions =
  let 
    xs = positions |> List.map Tuple.first 
    ys = positions |> List.map Tuple.second 
    xStart = xs |> List.minimum |> Maybe.withDefault 0
    yStart = ys |> List.minimum |> Maybe.withDefault 0
  in 
    (xStart + 1, yStart + 1)

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

    positions = instructions |> digLagoon |> adjustLagoon
    trench = positions |> Set.fromList

    startPoint = findStartPosition positions

    basin = { startPoint = startPoint 
            , trench = trench
            , explorationPoints = [ startPoint ]
            , filledPoints = Set.empty
            , cubicMeters = 0 }

    debugText = instructions |> List.length |> String.fromInt 

    model = { basin = basin 
            , debug = debugText }
  in 
    (model, Cmd.none)

-- UPDATE

type Msg = 
  Tick 

findNeighbourPositions : Position -> List Position 
findNeighbourPositions pos = 
  case pos of 
    (x, y) -> [ (x-1, y-1), (x, y-1), (x+1, y-1), (x-1, y), (x+1, y), (x-1, y+1), (x, y+1), (x+1, y+1) ]

isTrenchPoint : Set Position -> Position -> Bool
isTrenchPoint trench pos = 
  Set.member pos trench 

isFreeSpace : Set Position -> Position -> Bool 
isFreeSpace trench pos =
  isTrenchPoint trench pos |> not

updateBasin : Basin -> Basin 
updateBasin basin =
  if List.isEmpty basin.explorationPoints then 
    basin 
  else 
    let 
      trench = basin.trench
      explorationSet = basin.explorationPoints |> Set.fromList 
      filledPoints = explorationSet |> Set.union basin.filledPoints
      addedPoints = basin.filledPoints |> Set.diff filledPoints |> Set.toList
      exploreNext = addedPoints |> List.concatMap findNeighbourPositions |> Set.fromList |> Set.toList |> List.filter (isFreeSpace trench)
      cubicMeters = Set.size trench + Set.size filledPoints
    in 
      { startPoint = basin.startPoint 
      , trench = trench
      , explorationPoints = exploreNext
      , filledPoints = filledPoints
      , cubicMeters = cubicMeters }

-- type alias Basin = 
--   { startPoint : Position 
--   , trench : Set Position
--   , explorationPoints : List Position
--   , filledPoints : Set Position }

updateModel : Model -> Model
updateModel model = 
  { model | basin = updateBasin model.basin }

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Tick ->
      (updateModel model, Cmd.none)

-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
  Time.every delay (\_ -> Tick)

-- VIEW

findDimensions : Set Position -> (Int, Int) 
findDimensions trench = 
  let 
    positions = trench |> Set.toList
    xs = positions |> List.map (Tuple.first)
    ys = positions |> List.map (Tuple.second)
    xMax = xs |> List.maximum |> Maybe.withDefault 123
    yMax = ys |> List.maximum |> Maybe.withDefault 123
  in 
    (xMax + 1, yMax + 1)

toTrenchBox : Int -> (Int, Int) -> Html Msg 
toTrenchBox unitSize (xPos, yPos) = 
  let 
    xVal = unitSize * xPos
    yVal = unitSize * yPos
  in
    rect
      [ x (String.fromInt xVal)
      , y (String.fromInt yVal)
      , width (String.fromInt unitSize) 
      , height (String.fromInt unitSize)
      , fill "black" ]
      []

toFilledBox : Int -> (Int, Int) -> Html Msg 
toFilledBox unitSize (xPos, yPos) = 
  let 
    xVal = unitSize * xPos
    yVal = unitSize * yPos
  in
    rect
      [ x (String.fromInt xVal)
      , y (String.fromInt yVal)
      , width (String.fromInt unitSize) 
      , height (String.fromInt unitSize)
      , fill "lightblue" ]
      []

toSvg : Model -> Html Msg 
toSvg model = 
  let 
    (widthInUnits, heightInUnits) = findDimensions model.basin.trench
    maxUnits = Basics.max widthInUnits heightInUnits
    maxDim = 500
    unitSize = maxDim // maxUnits
    svgWidth = (unitSize * widthInUnits) |> String.fromInt
    svgHeight = (unitSize * heightInUnits) |> String.fromInt
    trenchBoxes = model.basin.trench |> Set.toList |> List.map (toTrenchBox unitSize)
    filledBoxes = model.basin.filledPoints |> Set.toList |> List.map (toFilledBox unitSize)
    elements = trenchBoxes ++ filledBoxes
    -- elements = insideBoxes ++ loopBoxes ++ pipeShapes ++ [startOutline]
  in 
    svg
      [ viewBox ("0 0 " ++ svgWidth ++ svgHeight)
      , width svgWidth
      , height svgHeight
      , Svg.Attributes.style "background-color:white" ]
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
              , Html.Attributes.style "font-size" "36px"
              , Html.Attributes.style "padding" "24px"] 
              [ Html.div [ Html.Attributes.align "center" ] [ s ] 
              , Html.div [] [ Html.text <| (String.fromInt model.basin.cubicMeters) ++ " m³"]
              ] ] ]
