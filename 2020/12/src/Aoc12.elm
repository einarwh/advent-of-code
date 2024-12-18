module Aoc12 exposing (..)

import Browser exposing (Document)
import Html exposing (Html)
import Html.Attributes
import Html.Events exposing (onClick)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Task
import Time

-- MAIN

main =
  Browser.document
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions }

-- MODEL

type DataSource = Sample | Input 

type Strategy = MoveBoat | MoveWaypoint

type alias Steps = Int

type alias Rotations = Int

type Instruction =
    N Steps
    | S Steps
    | E Steps
    | W Steps
    | L Rotations
    | R Rotations
    | F Steps
    
type alias Position = (Int, Int)

type Direction = North | West | South | East

type alias Model = 
  { ship : Position
  , direction : Direction
  , waypoint : Position
  , dataSource : DataSource
  , strategy : Strategy
  , paused : Bool 
  , finished : Bool 
  , tickInterval : Float 
  , maxxy : Int 
  , instructions : List Instruction
  , prevInstruction : Maybe Instruction
  , visited : List Position }

toRotations : Int -> Int
toRotations deg = 
  case deg of 
    90 -> 1 
    180 -> 2 
    270 -> 3 
    _ -> 0
    
parse : String -> Instruction
parse s =
  let 
    letter = s |> String.left 1
    number = s |> String.dropLeft 1 |> String.toInt |> Maybe.withDefault 0
  in 
    case letter of 
      "N" -> N number 
      "S" -> S number 
      "E" -> E number 
      "W" -> W number 
      "L" -> L (toRotations number) 
      "R" -> R (toRotations number)
      "F" -> F number 
      _ -> N 0

example = [ F 10, N 3, F 7, R 1, F 11 ]


initModel : Strategy -> DataSource -> Model
initModel strategy dataSource =
  let 
    sample = ["F10", "N3", "F7", "R90", "F11" ]
    input = [ "N5", "W1", "F61", "W2", "R90", "F50", "N2", "F40", "E4", "F48", "R180", "F17", "W4", "N5", "F3", "W3", "F1", "R90", "S2", "F23", "L90", "S3", "W3", "S4", "E4", "L90", "W3", "S3", "E4", "N2", "F28", "S2", "W2", "L180", "E3", "R90", "E3", "F83", "W5", "S4", "W3", "N2", "W5", "F90", "N2", "F82", "N2", "F2", "S4", "L90", "N3", "L90", "S2", "F12", "S3", "F40", "L90", "F56", "N1", "F29", "W2", "S2", "R270", "S4", "F14", "E4", "R90", "E2", "S2", "E2", "F82", "L90", "N3", "R180", "R90", "S1", "W1", "L90", "S2", "F78", "W2", "F52", "N4", "W5", "F38", "L90", "W2", "S2", "L90", "F66", "R90", "F62", "E3", "S5", "L90", "F99", "F2", "E4", "R90", "N3", "W4", "N1", "F71", "E2", "N3", "N2", "R90", "E2", "F66", "S4", "R90", "E5", "F29", "E5", "L90", "W2", "N2", "E3", "F18", "L180", "F17", "W1", "R90", "W3", "S5", "R90", "S3", "R180", "N5", "F69", "W1", "W3", "L180", "F72", "W5", "N1", "R180", "W3", "W4", "F85", "W4", "L90", "E4", "N5", "F73", "R90", "F70", "E4", "F79", "S5", "R180", "E2", "F35", "E4", "L270", "W2", "L90", "N5", "R90", "N4", "F64", "W2", "R270", "F33", "N5", "E4", "F94", "W1", "N1", "R90", "F79", "F46", "E1", "R180", "S3", "W3", "F72", "E1", "W4", "F95", "W2", "L90", "N3", "L90", "F85", "W3", "W1", "F54", "N3", "E1", "N4", "E5", "L90", "F61", "W2", "F7", "L180", "F87", "N4", "W1", "F87", "F3", "E3", "F63", "R90", "S4", "R180", "S4", "R180", "R90", "R90", "E5", "N4", "E2", "F86", "S3", "F98", "N4", "F70", "L90", "E4", "F26", "W4", "F19", "L90", "S4", "W4", "F84", "N1", "E4", "L180", "S2", "F74", "S1", "F86", "R90", "S2", "F78", "N4", "S2", "W1", "N5", "E2", "F38", "W4", "N1", "F75", "S1", "E1", "N3", "S1", "F54", "N3", "F88", "N5", "L180", "F15", "S2", "S2", "E2", "N3", "F97", "S3", "N3", "E3", "N5", "E3", "R90", "F87", "L90", "F15", "L90", "E5", "R90", "F70", "N3", "W2", "F47", "W2", "W3", "F17", "R90", "F95", "E4", "F28", "W4", "R90", "E2", "R180", "N4", "R180", "W4", "R270", "F73", "W1", "N2", "L90", "S1", "F65", "E1", "F42", "N2", "F74", "R90", "F21", "W5", "S1", "N5", "R90", "E4", "N5", "S5", "F99", "W4", "L180", "W1", "F83", "N2", "W2", "F87", "E2", "S3", "W1", "L180", "F89", "S1", "W2", "E2", "L90", "S2", "W1", "S5", "R180", "E5", "N1", "F82", "S3", "F7", "L90", "F31", "L90", "N3", "F84", "W3", "N4", "F100", "N1", "E2", "R90", "F90", "N3", "F43", "R90", "F2", "W4", "L90", "F87", "L90", "E3", "F71", "L180", "N1", "L90", "E4", "N3", "F31", "W1", "F80", "R270", "N1", "E4", "N1", "F22", "N4", "E1", "F57", "R90", "N3", "W2", "L180", "N3", "L180", "W4", "F59", "S4", "F10", "N5", "L90", "S3", "L90", "E1", "F96", "E4", "N3", "F54", "L180", "F47", "W1", "N4", "E1", "S4", "R180", "L90", "N1", "R90", "N3", "R90", "N4", "R90", "S3", "F59", "N5", "L90", "E4", "F72", "W4", "F76", "R90", "E3", "F70", "L180", "N3", "W2", "R90", "F65", "L90", "F71", "S3", "F43", "R90", "W2", "N2", "R90", "W1", "R90", "S4", "R180", "S1", "E3", "F72", "L90", "F61", "L90", "F75", "S1", "S5", "F15", "R90", "E3", "N2", "L270", "F48", "N1", "R180", "W2", "F69", "E4", "R90", "R90", "W1", "S5", "W5", "R90", "S4", "S3", "F51", "F43", "E2", "N5", "L180", "F89", "W1", "R90", "F59", "R90", "E2", "F51", "R90", "F91", "W4", "S5", "E4", "L90", "S5", "R90", "F44", "F47", "E4", "W1", "F77", "S5", "R90", "N2", "F87", "N4", "R90", "W5", "R90", "W5", "F89", "L90", "F61", "E2", "F29", "N4", "R90", "F31", "S1", "L90", "E5", "N2", "F7", "L180", "S4", "F63", "W4", "N5", "S2", "N1", "E5", "F87", "S5", "R180", "F14", "W4", "R180", "E1", "L90", "F67", "E2", "L90", "E5", "S2", "L90", "W2", "R90", "F94", "W4", "R90", "W3", "S3", "R90", "N5", "F55", "L90", "F43", "L90", "N5", "F16", "E4", "N2", "L270", "W3", "E1", "N2", "R180", "F51", "N5", "N1", "F36", "W4", "F38", "N5", "W1", "F29", "R180", "L90", "N1", "W3", "E1", "F78", "E1", "N1", "E2", "F57", "E4", "F83", "W5", "F32", "N3", "W4", "F36", "N2", "E3", "F74", "N4", "F54", "W5", "L90", "S1", "F42", "W4", "S5", "E3", "F64", "W2", "R180", "S2", "E1", "N2", "R90", "W3", "F36", "N3", "R90", "S2", "F53", "W2", "F85", "E5", "N2", "F9", "E1", "F83", "L90", "E5", "F44", "L90", "F92", "W5", "R270", "E4", "S1", "F6", "L90", "F96", "R90", "N1", "E4", "N1", "W3", "S2", "S4", "F39", "E1", "S1", "F82", "S3", "F78", "L90", "N4", "E1", "N2", "R90", "F63", "S3", "L180", "F52", "W2", "F49", "W2", "L270", "N1", "R180", "E3", "F79", "F73", "N1", "R90", "N3", "R180", "S2", "F35", "S1", "F43", "S1", "R90", "S4", "W4", "F12", "S1", "F2", "N3", "E4", "L90", "F51", "R90", "N4", "F90", "R90", "F99", "E3", "N1", "R90", "S3", "L270", "W5", "L90", "R270", "F50", "N5", "F33", "S3", "F18", "L90", "E4", "L180", "W4", "R90", "F21", "W4", "F24", "W2", "E5", "N3", "W1", "R90", "W3", "S3", "F82", "W1", "S1", "F12", "N3", "L90", "F37", "R180", "F36", "F27", "E3", "S3", "F36", "W4", "S1", "F6", "R90", "F59", "S1", "E1", "R180", "S2", "W3", "L90", "F45", "R90", "E1", "F29", "S5", "W3", "S5", "W4", "L270", "S2", "F13", "E4", "F28", "R90", "F80", "S4", "E1", "S2", "F62", "R90", "F26", "L180", "F19", "W2", "L180", "W5", "F15", "N1", "F68", "E4", "F75", "S2", "F58", "S4", "R180", "E3", "N1", "L90", "S2", "F12", "R90", "E5", "S5", "W4", "N5", "W1", "R180", "S1", "F70", "R90", "F97", "L90", "E3", "S3", "L270", "E1", "F51", "N4", "L180", "N1", "R90", "F42" ]
    (data, tickInterval) = 
      case dataSource of 
        Sample -> (sample, 1000) 
        Input -> (input, 10)
    instructions = data |> List.map parse
    shipPos = (0, 0)
    waypointPos = (10, 1)
    model = { ship = shipPos
            , direction = East
            , waypoint = waypointPos
            , dataSource = dataSource
            , strategy = strategy
            , finished = False 
            , paused = True 
            , maxxy = 0
            , tickInterval = tickInterval
            , instructions = instructions
            , prevInstruction = Nothing
            , visited = [ shipPos ] }
  in 
    model

init : () -> (Model, Cmd Msg)
init _ =
  (initModel MoveBoat Input, Cmd.none)

-- UPDATE

type Msg = Tick | Clear | Step | TogglePlay | Faster | Slower | UseInput | UseSample | UseMoveBoat | UseMoveWaypoint

times : Int -> (a -> a) -> (a -> a)
times n fn = 
  if n < 1 then identity 
  else fn >> times (n - 1) fn 

moveBoatNorth : Steps -> (Position, Direction) -> (Position, Direction)
moveBoatNorth steps ((x,y), dir) = ((x, y-steps), dir)

moveBoatWest : Steps -> (Position, Direction) -> (Position, Direction)
moveBoatWest steps ((x,y), dir) = ((x-steps, y), dir)

moveBoatSouth : Steps -> (Position, Direction) -> (Position, Direction)
moveBoatSouth steps ((x,y), dir) = ((x, y+steps), dir)

moveBoatEast : Steps -> (Position, Direction) -> (Position, Direction)
moveBoatEast steps ((x,y), dir) = ((x+steps, y), dir)

moveBoatForward : Steps -> (Position, Direction) -> (Position, Direction)
moveBoatForward steps (pos, dir) =
    case dir of
      North -> moveBoatNorth steps (pos, dir)
      West -> moveBoatWest steps (pos, dir)
      South -> moveBoatSouth steps (pos, dir)
      East -> moveBoatEast steps (pos, dir)

turnBoatLeftOnce : Direction -> Direction
turnBoatLeftOnce dir =
    case dir of
      North -> West
      West -> South
      South -> East
      East -> North

turnBoatRightOnce : Direction -> Direction
turnBoatRightOnce dir =
    case dir of
      North -> East
      West -> North
      South -> West
      East -> South


turnBoatLeft : Int -> (Position, Direction) -> (Position, Direction)
turnBoatLeft n (pos, dir) =
  (pos, dir |> times n turnBoatLeftOnce)

turnBoatRight : Int -> (Position, Direction) -> (Position, Direction)
turnBoatRight n (pos, dir) =
  (pos, dir |> times n turnBoatRightOnce)

moveBoat : Instruction -> (Position, Direction) -> (Position, Direction)
moveBoat inst = 
  case inst of 
    N steps -> moveBoatNorth steps 
    S steps -> moveBoatSouth steps 
    W steps -> moveBoatWest steps 
    E steps -> moveBoatEast steps 
    L rotations -> turnBoatLeft rotations
    R rotations -> turnBoatRight rotations 
    F steps -> moveBoatForward steps

moveWaypointNorth : Steps -> Position -> Position
moveWaypointNorth steps (x,y) = (x, y+steps)

moveWaypointWest : Steps -> Position -> Position
moveWaypointWest steps (x,y) = (x-steps, y)

moveWaypointSouth : Steps -> Position -> Position
moveWaypointSouth steps (x,y) = (x, y-steps)

moveWaypointEast : Steps -> Position -> Position
moveWaypointEast steps (x,y) = (x+steps, y)

towards : Position -> Position -> Position 
towards (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

forward : Steps -> (Position, Position) -> (Position, Position)
forward n (ship, waypoint) =
  (ship |> times n (towards waypoint), waypoint)

rotateLeft : Position -> Position
rotateLeft (x, y) = (-y, x)

rotateRight : Position -> Position
rotateRight (x, y) = (y, -x)
  
rotateWaypointAroundShipLeft : Int -> Position -> Position
rotateWaypointAroundShipLeft n = times n rotateLeft

rotateWaypointAroundShipRight : Int -> Position -> Position
rotateWaypointAroundShipRight n = times n rotateRight

next : Instruction -> (Position, Position) -> (Position, Position)
next inst (ship, waypoint) = 
  case inst of 
    N steps -> (ship, moveWaypointNorth steps waypoint)
    S steps -> (ship, moveWaypointSouth steps waypoint)
    W steps -> (ship, moveWaypointWest steps waypoint)
    E steps -> (ship, moveWaypointEast steps waypoint) 
    L rotations -> (ship, rotateWaypointAroundShipLeft rotations waypoint)
    R rotations -> (ship, rotateWaypointAroundShipRight rotations waypoint)
    F steps -> forward steps (ship, waypoint)

updateClear : Model -> Model
updateClear model =
  initModel model.strategy model.dataSource

updateMoveBoat : Instruction -> List Instruction -> Model -> Model 
updateMoveBoat inst rest model = 
  let 
    (ship, dir) = moveBoat inst (model.ship, model.direction)
    (x, y) = ship 
    maxxy = Basics.max model.maxxy (Basics.max (abs x) (abs y))
  in
    { model | ship = ship, direction = dir, instructions = rest, prevInstruction = Just inst, visited = ship :: model.visited, maxxy = maxxy }

updateMoveWaypoint : Instruction -> List Instruction -> Model -> Model 
updateMoveWaypoint inst rest model = 
  let 
    (ship, waypoint) = next inst (model.ship, model.waypoint)
    (x, y) = ship 
    maxxy = Basics.max model.maxxy (Basics.max x y)
  in 
    { model | ship = ship, waypoint = waypoint, instructions = rest, prevInstruction = Just inst, visited = ship :: model.visited, maxxy = maxxy }

updateStep : Model -> Model 
updateStep model =
  if model.finished then model 
  else 
    case model.instructions of 
      [] -> { model | finished = True, paused = True } 
      inst :: rest -> 
        case model.strategy of 
          MoveBoat -> 
            updateMoveBoat inst rest model 
          MoveWaypoint -> 
            updateMoveWaypoint inst rest model 

updateTogglePlay model =
  if model.finished then 
    let 
      m = initModel model.strategy model.dataSource |> updateStep
    in 
      {m | paused = False }
  else 
    if model.paused then 
        { model | paused = False } |> updateStep
    else 
        { model | paused = True }

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Tick -> 
        (updateStep model, Cmd.none)
    Step -> 
        (updateStep model, Cmd.none)
    TogglePlay ->
      (updateTogglePlay model, Cmd.none)
    Faster -> 
      ({model | tickInterval = model.tickInterval / 2 }, Cmd.none)
    Slower -> 
      ({model | tickInterval = model.tickInterval * 2 }, Cmd.none)
    Clear -> 
        (updateClear model, Cmd.none)
    UseSample ->
      (initModel model.strategy Sample, Cmd.none)
    UseInput ->
      (initModel model.strategy Input, Cmd.none)
    UseMoveBoat ->
      (initModel MoveBoat model.dataSource, Cmd.none)
    UseMoveWaypoint ->
      (initModel MoveWaypoint model.dataSource, Cmd.none)

-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
  let
    tickSub = if model.paused then Sub.none else Time.every model.tickInterval (\_ -> Tick)
  in
    tickSub

-- VIEW

instructionAsString : Instruction -> String
instructionAsString inst =
  case inst of 
    N steps -> "N " ++ String.fromInt steps
    S steps -> "S " ++ String.fromInt steps
    E steps -> "N " ++ String.fromInt steps
    W steps -> "N " ++ String.fromInt steps
    L rotations -> "L " ++ String.fromInt rotations
    R rotations -> "R " ++ String.fromInt rotations
    F steps -> "F " ++ String.fromInt steps    

distance : (Int, Int) -> Int 
distance (x, y) = 
  abs x + abs y

calc : Int -> Float 
calc n = 400.0 + (toFloat n / 300)

toPolylineElement : (Float -> Float) -> List Position -> Svg msg 
toPolylineElement translate positions = 
  let 
    s = 
      let 
        str (xval, yval) = (String.fromFloat (translate (toFloat xval))) ++ "," ++ (String.fromFloat (translate (toFloat yval)))
      in
        positions |> List.map str |> String.join " "
  in
    polyline 
      [ stroke "#606060"
      , strokeWidth "1"
      , fill "None"
      , points s ] []

toCircleElement : (Float -> Float) -> Position -> Svg msg 
toCircleElement translate (xVal, yVal) = 
  let
    xStr = xVal |> toFloat |> translate |> String.fromFloat
    yStr = yVal |> toFloat |> translate |> String.fromFloat
    rStr = String.fromInt 3
  in
    circle [ cx xStr, cy yStr, r rStr, fill "none", stroke "red" ] []

toShipElement : (Float -> Float) -> (Float, Float) -> Position -> Svg msg 
toShipElement translate (dx, dy) (xval, yval) = 
  let 
    xf = toFloat xval + dx 
    yf = toFloat yval + dy 
    xStr = xf |> translate |> String.fromFloat 
    yStr = yf |> translate |> String.fromFloat 
  in 
    svg [ version "1.0", x xStr, y yStr, width "14.75pt", height "20.00pt", viewBox "0 0 1259.000000 1280.000000", preserveAspectRatio "xMidYMid meet" ] [ metadata [] [ text "Created by potrace 1.15, written by Peter Selinger 2001-2017" ], g [ transform "translate(0.000000,1280.000000) scale(0.100000,-0.100000)", fill "#000000", stroke "none" ] [ Svg.path [ d "M4692 12785 c-117 -33 -219 -119 -274 -233 -31 -63 -33 -73 -33 -177 0 -102 2 -114 31 -175 17 -36 52 -88 77 -116 l47 -52 0 -4428 0 -4429 -2205 -3 -2206 -2 118 -138 c294 -344 529 -598 868 -938 610 -612 1134 -1054 1670 -1409 652 -432 1178 -641 1697 -676 274 -19 728 -4 1093 37 1577 173 3471 893 5410 2057 509 305 1181 752 1544 1026 l55 41 -3755 2 -3754 3 -3 4430 -2 4429 29 31 c119 126 160 289 111 436 -41 120 -113 201 -227 256 -61 29 -87 36 -155 39 -54 3 -101 -1 -136 -11z" ] [], Svg.path [ d "M5452 11888 c452 -2355 623 -3791 605 -5068 -8 -507 -26 -771 -83 -1160 -90 -620 -263 -1182 -498 -1622 l-72 -135 3126 -6 c1718 -3 3127 -4 3129 -1 12 12 -301 700 -520 1139 -654 1315 -1394 2476 -2338 3665 -804 1013 -1851 2099 -2956 3066 -201 176 -433 374 -438 374 -2 0 18 -114 45 -252z" ] [], Svg.path [ d "M4220 9345 c0 -29 -82 -368 -126 -524 -277 -971 -745 -1794 -1468 -2580 -353 -384 -693 -695 -1496 -1366 -567 -474 -836 -707 -1030 -893 l-95 -91 2113 -1 2112 0 0 2735 c0 1504 -2 2735 -5 2735 -3 0 -5 -7 -5 -15z" ] [] ] ]

view : Model -> Document Msg
view model =
  { title = "Advent of Code 2020 | Day 12: Rain Risk"
  , body = [ viewBody model ] }

viewBody : Model -> Html Msg
viewBody model =
  let
    dim = 400
    dimStr = String.fromInt dim
    dist = distance model.ship
    distanceStr = String.fromInt dist
    shipStr = "(" ++ String.fromInt (Tuple.first model.ship) ++ ", " ++ String.fromInt (Tuple.second model.ship) ++ ")"
    waypointStr = "(" ++ String.fromInt (Tuple.first model.waypoint) ++ ", " ++ String.fromInt (Tuple.second model.waypoint) ++ ")"
    instStr = 
      case model.prevInstruction of 
        Nothing -> "?"
        Just prev -> instructionAsString prev
    maxxyStr = String.fromInt model.maxxy
    (scaleFactor, shipOffset) = 
      case (model.dataSource, model.strategy) of 
        (Input, MoveBoat) -> (0.100, (-76.0, -185.0))
        (Input, MoveWaypoint) -> (0.0025, (-3030.0, -7400.0))
        (Sample, MoveBoat) -> (10.00, (-0.76, -1.84))
        (Sample, MoveWaypoint) -> (0.75, (-10.1, -24.6))
    translate n = n * scaleFactor
    shipElements = [ toShipElement translate shipOffset model.ship ]
    lineElement = toPolylineElement translate model.visited
    elements = lineElement :: shipElements
    viewBoxStr = [ "-200", "-200", "400", "400" ] |> String.join " "
  in 
    Html.table 
      [ Html.Attributes.style "width" "1080px"
      , Html.Attributes.style "font-family" "Courier New" ]
      [ Html.tr 
          [] 
          [ Html.td 
              [ Html.Attributes.align "center"
              , Html.Attributes.style "font-family" "Courier New"
              , Html.Attributes.style "font-size" "32px"
              , Html.Attributes.style "padding" "10px"]
              [ Html.div [] [Html.text "Advent of Code 2020" ]
              , Html.div [] [Html.text "Day 12: Rain Risk" ] ] ]
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
              , Html.Attributes.style "padding-bottom" "10px" ]
              [ Html.a
                [ Html.Attributes.href "https://adventofcode.com/2020/day/12" ]
                [ text "https://adventofcode.com/2020/day/12" ]
            ] ]
      , Html.tr
          []
          [ Html.td
              [ Html.Attributes.align "center"
              , Html.Attributes.style "font-family" "Courier New"
              , Html.Attributes.style "font-size" "16px" ]
              [
                Html.input
                [ Html.Attributes.type_ "radio", onClick UseInput, Html.Attributes.checked (model.dataSource == Input) ]
                []
              , Html.label [] [ Html.text "Input" ]
              , Html.input
                [ Html.Attributes.type_ "radio", onClick UseSample, Html.Attributes.checked (model.dataSource == Sample) ]
                []
              , Html.label [] [ Html.text "Sample" ]
            ] ]
      , Html.tr 
          []
          [ Html.td 
              [ Html.Attributes.align "center"
              , Html.Attributes.style "padding" "10px" ]
              [ Html.button 
                [ Html.Attributes.style "width" "80px", onClick Clear ] 
                [ Html.text "Reset"]
              , Html.button 
                [ Html.Attributes.style "width" "80px", onClick Slower ] 
                [ text "Slower" ]
              , Html.button 
                [ Html.Attributes.style "width" "80px", onClick TogglePlay ] 
                [ if model.paused then text "Solve" else text "Pause" ] 
              , Html.button 
                [ Html.Attributes.style "width" "80px", onClick Faster ] 
                [ text "Faster" ]
              , Html.button 
                [ Html.Attributes.style "width" "80px", onClick Step ] 
                [ Html.text "Step" ]
            ] ]
      , Html.tr
          []
          [ Html.td
              [ Html.Attributes.align "center"
              , Html.Attributes.style "font-family" "Courier New"
              , Html.Attributes.style "font-size" "16px" ]
              [
                Html.input
                [ Html.Attributes.type_ "radio", onClick UseMoveBoat, Html.Attributes.checked (model.strategy == MoveBoat) ]
                []
              , Html.label [] [ Html.text "Boat" ]
              , Html.input
                [ Html.Attributes.type_ "radio", onClick UseMoveWaypoint, Html.Attributes.checked (model.strategy == MoveWaypoint) ]
                []
              , Html.label [] [ Html.text "Waypoint" ]
            ] ]
      , Html.tr 
          [] 
          [ Html.td 
              [ Html.Attributes.align "center"
              , Html.Attributes.style "font-family" "Courier New"
              , Html.Attributes.style "font-size" "24px"
              , Html.Attributes.style "padding" "10px"]
              [ 
                Html.div [] [ Html.text (distanceStr) ]
            --   , Html.div [] [ Html.text ("Ship: " ++ shipStr) ]
            --   , Html.div [] [ Html.text ("Waypoint: " ++ waypointStr) ]
              , Html.div [] [ Html.text (instStr) ]
            --   , Html.div [] [ Html.text ("Maxxy: " ++ maxxyStr) ]
              ] ]
      , Html.tr 
          []
          [ Html.td 
              [ Html.Attributes.align "center"
              , Html.Attributes.style "background-color" "white" 
              , Html.Attributes.style "padding" "10px"] 
              [ svg [ viewBox viewBoxStr, width dimStr, height dimStr, Svg.Attributes.style "background-color:lightblue" ] elements 
              ] ] ]