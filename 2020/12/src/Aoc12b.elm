module Aoc12b exposing (..)

import Browser
import Html exposing (Html)
import Html.Attributes
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Task
import Time

delay : Float
delay = 10

-- MAIN

main =
  Browser.element
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions }

-- MODEL

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

type alias Model = 
  { ship : Position
  , waypoint : Position
  , instructions : List Instruction
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

init : () -> (Model, Cmd Msg)
init _ =
  let 
    input = [ "N5", "W1", "F61", "W2", "R90", "F50", "N2", "F40", "E4", "F48", "R180", "F17", "W4", "N5", "F3", "W3", "F1", "R90", "S2", "F23", "L90", "S3", "W3", "S4", "E4", "L90", "W3", "S3", "E4", "N2", "F28", "S2", "W2", "L180", "E3", "R90", "E3", "F83", "W5", "S4", "W3", "N2", "W5", "F90", "N2", "F82", "N2", "F2", "S4", "L90", "N3", "L90", "S2", "F12", "S3", "F40", "L90", "F56", "N1", "F29", "W2", "S2", "R270", "S4", "F14", "E4", "R90", "E2", "S2", "E2", "F82", "L90", "N3", "R180", "R90", "S1", "W1", "L90", "S2", "F78", "W2", "F52", "N4", "W5", "F38", "L90", "W2", "S2", "L90", "F66", "R90", "F62", "E3", "S5", "L90", "F99", "F2", "E4", "R90", "N3", "W4", "N1", "F71", "E2", "N3", "N2", "R90", "E2", "F66", "S4", "R90", "E5", "F29", "E5", "L90", "W2", "N2", "E3", "F18", "L180", "F17", "W1", "R90", "W3", "S5", "R90", "S3", "R180", "N5", "F69", "W1", "W3", "L180", "F72", "W5", "N1", "R180", "W3", "W4", "F85", "W4", "L90", "E4", "N5", "F73", "R90", "F70", "E4", "F79", "S5", "R180", "E2", "F35", "E4", "L270", "W2", "L90", "N5", "R90", "N4", "F64", "W2", "R270", "F33", "N5", "E4", "F94", "W1", "N1", "R90", "F79", "F46", "E1", "R180", "S3", "W3", "F72", "E1", "W4", "F95", "W2", "L90", "N3", "L90", "F85", "W3", "W1", "F54", "N3", "E1", "N4", "E5", "L90", "F61", "W2", "F7", "L180", "F87", "N4", "W1", "F87", "F3", "E3", "F63", "R90", "S4", "R180", "S4", "R180", "R90", "R90", "E5", "N4", "E2", "F86", "S3", "F98", "N4", "F70", "L90", "E4", "F26", "W4", "F19", "L90", "S4", "W4", "F84", "N1", "E4", "L180", "S2", "F74", "S1", "F86", "R90", "S2", "F78", "N4", "S2", "W1", "N5", "E2", "F38", "W4", "N1", "F75", "S1", "E1", "N3", "S1", "F54", "N3", "F88", "N5", "L180", "F15", "S2", "S2", "E2", "N3", "F97", "S3", "N3", "E3", "N5", "E3", "R90", "F87", "L90", "F15", "L90", "E5", "R90", "F70", "N3", "W2", "F47", "W2", "W3", "F17", "R90", "F95", "E4", "F28", "W4", "R90", "E2", "R180", "N4", "R180", "W4", "R270", "F73", "W1", "N2", "L90", "S1", "F65", "E1", "F42", "N2", "F74", "R90", "F21", "W5", "S1", "N5", "R90", "E4", "N5", "S5", "F99", "W4", "L180", "W1", "F83", "N2", "W2", "F87", "E2", "S3", "W1", "L180", "F89", "S1", "W2", "E2", "L90", "S2", "W1", "S5", "R180", "E5", "N1", "F82", "S3", "F7", "L90", "F31", "L90", "N3", "F84", "W3", "N4", "F100", "N1", "E2", "R90", "F90", "N3", "F43", "R90", "F2", "W4", "L90", "F87", "L90", "E3", "F71", "L180", "N1", "L90", "E4", "N3", "F31", "W1", "F80", "R270", "N1", "E4", "N1", "F22", "N4", "E1", "F57", "R90", "N3", "W2", "L180", "N3", "L180", "W4", "F59", "S4", "F10", "N5", "L90", "S3", "L90", "E1", "F96", "E4", "N3", "F54", "L180", "F47", "W1", "N4", "E1", "S4", "R180", "L90", "N1", "R90", "N3", "R90", "N4", "R90", "S3", "F59", "N5", "L90", "E4", "F72", "W4", "F76", "R90", "E3", "F70", "L180", "N3", "W2", "R90", "F65", "L90", "F71", "S3", "F43", "R90", "W2", "N2", "R90", "W1", "R90", "S4", "R180", "S1", "E3", "F72", "L90", "F61", "L90", "F75", "S1", "S5", "F15", "R90", "E3", "N2", "L270", "F48", "N1", "R180", "W2", "F69", "E4", "R90", "R90", "W1", "S5", "W5", "R90", "S4", "S3", "F51", "F43", "E2", "N5", "L180", "F89", "W1", "R90", "F59", "R90", "E2", "F51", "R90", "F91", "W4", "S5", "E4", "L90", "S5", "R90", "F44", "F47", "E4", "W1", "F77", "S5", "R90", "N2", "F87", "N4", "R90", "W5", "R90", "W5", "F89", "L90", "F61", "E2", "F29", "N4", "R90", "F31", "S1", "L90", "E5", "N2", "F7", "L180", "S4", "F63", "W4", "N5", "S2", "N1", "E5", "F87", "S5", "R180", "F14", "W4", "R180", "E1", "L90", "F67", "E2", "L90", "E5", "S2", "L90", "W2", "R90", "F94", "W4", "R90", "W3", "S3", "R90", "N5", "F55", "L90", "F43", "L90", "N5", "F16", "E4", "N2", "L270", "W3", "E1", "N2", "R180", "F51", "N5", "N1", "F36", "W4", "F38", "N5", "W1", "F29", "R180", "L90", "N1", "W3", "E1", "F78", "E1", "N1", "E2", "F57", "E4", "F83", "W5", "F32", "N3", "W4", "F36", "N2", "E3", "F74", "N4", "F54", "W5", "L90", "S1", "F42", "W4", "S5", "E3", "F64", "W2", "R180", "S2", "E1", "N2", "R90", "W3", "F36", "N3", "R90", "S2", "F53", "W2", "F85", "E5", "N2", "F9", "E1", "F83", "L90", "E5", "F44", "L90", "F92", "W5", "R270", "E4", "S1", "F6", "L90", "F96", "R90", "N1", "E4", "N1", "W3", "S2", "S4", "F39", "E1", "S1", "F82", "S3", "F78", "L90", "N4", "E1", "N2", "R90", "F63", "S3", "L180", "F52", "W2", "F49", "W2", "L270", "N1", "R180", "E3", "F79", "F73", "N1", "R90", "N3", "R180", "S2", "F35", "S1", "F43", "S1", "R90", "S4", "W4", "F12", "S1", "F2", "N3", "E4", "L90", "F51", "R90", "N4", "F90", "R90", "F99", "E3", "N1", "R90", "S3", "L270", "W5", "L90", "R270", "F50", "N5", "F33", "S3", "F18", "L90", "E4", "L180", "W4", "R90", "F21", "W4", "F24", "W2", "E5", "N3", "W1", "R90", "W3", "S3", "F82", "W1", "S1", "F12", "N3", "L90", "F37", "R180", "F36", "F27", "E3", "S3", "F36", "W4", "S1", "F6", "R90", "F59", "S1", "E1", "R180", "S2", "W3", "L90", "F45", "R90", "E1", "F29", "S5", "W3", "S5", "W4", "L270", "S2", "F13", "E4", "F28", "R90", "F80", "S4", "E1", "S2", "F62", "R90", "F26", "L180", "F19", "W2", "L180", "W5", "F15", "N1", "F68", "E4", "F75", "S2", "F58", "S4", "R180", "E3", "N1", "L90", "S2", "F12", "R90", "E5", "S5", "W4", "N5", "W1", "R180", "S1", "F70", "R90", "F97", "L90", "E3", "S3", "L270", "E1", "F51", "N4", "L180", "N1", "R90", "F42" ]
    instructions = input |> List.map parse
    shipPos = (0, 0)
    waypointPos = (10, 1)
    model = { ship = shipPos
            , waypoint = waypointPos
            , instructions = instructions
            , visited = [ shipPos ] }
  in 
    (model, Cmd.none)

-- UPDATE

type Msg = Tick

north : Steps -> Position -> Position
north steps (x,y) = (x, y+steps)

west : Steps -> Position -> Position
west steps (x,y) = (x-steps, y)

south : Steps -> Position -> Position
south steps (x,y) = (x, y-steps)

east : Steps -> Position -> Position
east steps (x,y) = (x+steps, y)

towards : Position -> Position -> Position 
towards (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

forward : Steps -> (Position, Position) -> (Position, Position)
forward n (ship, waypoint) =
  (ship |> times n (towards waypoint), waypoint)

rotateLeft : Position -> Position
rotateLeft (x, y) = (-y, x)

rotateRight : Position -> Position
rotateRight (x, y) = (y, -x)

times : Int -> (a -> a) -> (a -> a)
times n fn = 
  if n < 1 then identity 
  else fn >> times (n - 1) fn 
  
left : Int -> Position -> Position
left n = times n rotateLeft

right : Int -> Position -> Position
right n = times n rotateRight

next : Instruction -> (Position, Position) -> (Position, Position)
next inst (ship, waypoint) = 
  case inst of 
    N steps -> (ship, north steps waypoint)
    S steps -> (ship, south steps waypoint)
    W steps -> (ship, west steps waypoint)
    E steps -> (ship, east steps waypoint) 
    L rotations -> (ship, left rotations waypoint)
    R rotations -> (ship, right rotations waypoint)
    F steps -> forward steps (ship, waypoint)

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Tick -> 
      let 
        updated = 
          case model.instructions of 
            [] -> model 
            inst :: rest -> 
              let 
                (ship, waypoint) = next inst (model.ship, model.waypoint)
              in 
                { ship = ship
                , waypoint = waypoint 
                , instructions = rest
                , visited = ship :: model.visited }
      in 
        (updated, Cmd.none)

-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
  Time.every delay (\_ -> Tick)

-- VIEW

distance : (Int, Int) -> Int 
distance (x, y) = 
  abs x + abs y

calc : Int -> Float 
calc n = 400.0 + (toFloat n / 300)

toPolylineElement : List Position -> Svg msg 
toPolylineElement positions = 
  let 
    s = 
      let str (xval, yval) = (String.fromFloat (calc xval)) ++ "," ++ (String.fromFloat (800 - calc yval))
      in
        positions |> List.map str |> String.join " "
  in
    polyline 
      [ stroke "gray"
      , strokeWidth "1"
      , fill "None"
      , points s ] []

toShipElement : Position -> Svg msg 
toShipElement (xval, yval) = 
  let 
    xStr = String.fromFloat (calc xval - 14.75)
    yStr = String.fromFloat (800 - (calc yval + 35.00))
  in 
    svg [ version "1.0", x xStr, y yStr, width "29.5pt", height "40.00pt", viewBox "0 0 1259.000000 1280.000000", preserveAspectRatio "xMidYMid meet" ] [ metadata [] [ text "Created by potrace 1.15, written by Peter Selinger 2001-2017" ], g [ transform "translate(0.000000,1280.000000) scale(0.100000,-0.100000)", fill "#000000", stroke "none" ] [ Svg.path [ d "M4692 12785 c-117 -33 -219 -119 -274 -233 -31 -63 -33 -73 -33 -177 0 -102 2 -114 31 -175 17 -36 52 -88 77 -116 l47 -52 0 -4428 0 -4429 -2205 -3 -2206 -2 118 -138 c294 -344 529 -598 868 -938 610 -612 1134 -1054 1670 -1409 652 -432 1178 -641 1697 -676 274 -19 728 -4 1093 37 1577 173 3471 893 5410 2057 509 305 1181 752 1544 1026 l55 41 -3755 2 -3754 3 -3 4430 -2 4429 29 31 c119 126 160 289 111 436 -41 120 -113 201 -227 256 -61 29 -87 36 -155 39 -54 3 -101 -1 -136 -11z" ] [], Svg.path [ d "M5452 11888 c452 -2355 623 -3791 605 -5068 -8 -507 -26 -771 -83 -1160 -90 -620 -263 -1182 -498 -1622 l-72 -135 3126 -6 c1718 -3 3127 -4 3129 -1 12 12 -301 700 -520 1139 -654 1315 -1394 2476 -2338 3665 -804 1013 -1851 2099 -2956 3066 -201 176 -433 374 -438 374 -2 0 18 -114 45 -252z" ] [], Svg.path [ d "M4220 9345 c0 -29 -82 -368 -126 -524 -277 -971 -745 -1794 -1468 -2580 -353 -384 -693 -695 -1496 -1366 -567 -474 -836 -707 -1030 -893 l-95 -91 2113 -1 2112 0 0 2735 c0 1504 -2 2735 -5 2735 -3 0 -5 -7 -5 -15z" ] [] ] ]

view : Model -> Html Msg
view model =
  let
    dim = 800
    dimStr = String.fromInt dim
    dist = distance model.ship
    distanceStr = String.fromInt dist
    shipElements = [ toShipElement model.ship ]
    lineElement = toPolylineElement model.visited
    elements = lineElement :: shipElements
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
              [ text distanceStr ] ]
      , Html.tr 
          []
          [ Html.td 
              [ Html.Attributes.align "center"
              , Html.Attributes.style "background-color" "white" 
              , Html.Attributes.style "padding" "20px"] 
              [ svg [ width dimStr, height dimStr ] elements 
              ] ] ]