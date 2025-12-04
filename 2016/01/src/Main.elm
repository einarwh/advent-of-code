module Main exposing (..)

import Browser
import Html exposing (Html)
import Html.Attributes
import Html.Events exposing (onClick)
import Set exposing (Set)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Time

defaultTickInterval : Float
defaultTickInterval = 10

-- MAIN

main =
  Browser.element
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions }

-- MODEL

type DataSource = Input | Sample

type alias Pos = (Int, Int)

type Dir = N | W | S | E

type Instruction = R Int | L Int 

type Step = TurnLeft | TurnRight | MoveForward

type Visit = Vertical | Horizontal | Both 

type alias Model = 
  { pos : Pos 
  , dir : Dir 
  , twice : Maybe Pos
  , path : List (Dir, Pos)
  , steps : List (Dir, Pos)
  , walked : List Pos
  , seen : Set Pos 
  , paused : Bool 
  , finished : Bool 
  , tickInterval : Float 
  , debug : String }

input : String 
input = """L5, R1, L5, L1, R5, R1, R1, L4, L1, L3, R2, R4, L4, L1, L1, R2, R4, R3, L1, R4, L4, L5, L4, R4, L5, R1, R5, L2, R1, R3, L2, L4, L4, R1, L192, R5, R1, R4, L5, L4, R5, L1, L1, R48, R5, R5, L2, R4, R4, R1, R3, L1, L4, L5, R1, L4, L2, L5, R5, L2, R74, R4, L1, R188, R5, L4, L2, R5, R2, L4, R4, R3, R3, R2, R1, L3, L2, L5, L5, L2, L1, R1, R5, R4, L3, R5, L1, L3, R4, L1, L3, L2, R1, R3, R2, R5, L3, L1, L1, R5, L4, L5, R5, R2, L5, R2, L1, L5, L3, L5, L5, L1, R1, L4, L3, L1, R2, R5, L1, L3, R4, R5, L4, L1, R5, L1, R5, R5, R5, R2, R1, R2, L5, L5, L5, R4, L5, L4, L4, R5, L2, R1, R5, L1, L5, R4, L3, R4, L2, R3, R3, R3, L2, L2, L2, L1, L4, R3, L4, L2, R2, R5, L1, R2"""

tryParseInstruction : String -> Maybe Instruction
tryParseInstruction s = 
  case String.left 1 s of 
    "L" -> 
      s |> String.dropLeft 1 |> String.toInt |> Maybe.map L
    "R" -> 
      s |> String.dropLeft 1 |> String.toInt |> Maybe.map R
    _ ->
      Nothing

parseInstructionList : String -> List Instruction
parseInstructionList s = 
  s |> String.split ", " |> List.filterMap tryParseInstruction

move inst (dir, pos, path) = 
  case inst of 
    R steps -> 
      let 
        nextDir = turnRight dir 
        nextPos = moveForward nextDir steps pos 
      in 
        (nextDir, nextPos, (nextDir, nextPos) :: path)
    L steps -> 
      let 
        nextDir = turnLeft dir 
        nextPos = moveForward nextDir steps pos 
      in 
        (nextDir, nextPos,(nextDir, nextPos) :: path)

moveInSteps inst (dir, pos, path) = 
  case inst of 
    R steps -> 
      let 
        nextDir = turnRight dir 
        nextPositions = moveForwardInSteps nextDir steps pos 
        nextPos = nextPositions |> List.head |> Maybe.withDefault pos
        nextPathSegment = nextPositions |> List.map (\p -> (nextDir, p))
      in 
        (nextDir, nextPos, List.append nextPathSegment path)
    L steps -> 
      let 
        nextDir = turnLeft dir 
        nextPositions = moveForwardInSteps nextDir steps pos 
        nextPos = nextPositions |> List.head |> Maybe.withDefault pos
        nextPathSegment = nextPositions |> List.map (\p -> (nextDir, p))
      in 
        (nextDir, nextPos, List.append nextPathSegment path)

walk : Dir -> Pos -> List Instruction -> List (Dir, Pos)
walk dir pos instructions = 
  let 
    (_, _, result) = instructions |> List.foldl move (dir, pos, [])
  in 
    result |> List.reverse

walkInSteps : Dir -> Pos -> List Instruction -> List (Dir, Pos)
walkInSteps dir pos instructions = 
  let 
    (_, _, result) = instructions |> List.foldl moveInSteps (dir, pos, [])
  in 
    result |> List.reverse

moveForward : Dir -> Int -> Pos -> Pos 
moveForward dir steps (x, y) = 
  case dir of 
    N -> (x, y - steps)
    W -> (x - steps, y)
    S -> (x, y + steps)
    E -> (x + steps, y)

moveForwardOneStep : Dir -> Pos -> Pos 
moveForwardOneStep dir (x, y) = 
  case dir of 
    N -> (x, y - 1)
    W -> (x - 1, y)
    S -> (x, y + 1)
    E -> (x + 1, y)

moveForwardLoop : List Pos -> Dir -> Int -> Pos -> List Pos 
moveForwardLoop acc dir stepsLeft (x, y) = 
  if stepsLeft > 0 then 
    let 
      nextPos = moveForwardOneStep dir (x, y)
    in 
      moveForwardLoop (nextPos :: acc) dir (stepsLeft - 1) nextPos
  else 
    acc

moveForwardInSteps : Dir -> Int -> Pos -> List Pos 
moveForwardInSteps dir steps (x, y) = 
  moveForwardLoop [] dir steps (x, y)

turnRight : Dir -> Dir 
turnRight dir = 
  case dir of 
    N -> E
    E -> S 
    S -> W
    W -> N

turnLeft : Dir -> Dir 
turnLeft dir = 
  case dir of 
    N -> W
    E -> N
    S -> E
    W -> S

instToStr inst = 
  case inst of 
    L steps -> "L" ++ String.fromInt steps 
    R steps -> "R" ++ String.fromInt steps 

dirToStr dir = 
  case dir of 
    N -> "N"
    W -> "W"
    S -> "S"
    E -> "E"

initModel : Model 
initModel = 
  let 
    instructions = parseInstructionList input
    path = walk N (0, 0) instructions
    steps = walkInSteps N (0, 0) instructions
    -- debug = steps |> List.map (\(d, p) -> dirToStr d ++ ": (" ++ posToStr p ++ ")") |> String.join " "
    debug = " "
  in 
    { pos = (0, 0)
    , dir = N
    , twice = Nothing
    , path = path
    , steps = steps
    , walked = []
    , seen = Set.empty
    , paused = True
    , finished = False 
    , tickInterval = defaultTickInterval
    , debug = debug }

init : () -> (Model, Cmd Msg)
init _ =
  (initModel, Cmd.none)

-- UPDATE

type Msg = 
  Tick 
  | Step 
  | TogglePlay 
  | Faster 
  | Slower 
  | Clear 

getNestedPositions : (Int, Int) -> (Int, Int) -> List (List Pos)
getNestedPositions (xMin, yMin) (xMax, yMax) = 
  let
    ys = List.range yMin (yMax - 1)
    xs = List.range xMin (xMax - 1)
  in 
    ys |> List.map (\y -> xs |> List.map (\x -> (x, y)))

updateClear : Model -> Model
updateClear _ = 
  initModel

updateStep : Model -> Model
updateStep model = 
  case model.steps of 
    [] -> { model | finished = True}
    (d, pos) :: rest ->
      let 
        (twice, seen) = 
          case model.twice of 
            Just _ -> (model.twice, model.seen) 
            Nothing -> 
              if Set.member pos model.seen then 
                (Just pos, model.seen)
              else 
                (Nothing, Set.insert pos model.seen)
        walked = pos :: model.walked 
        dbg = walked |> List.length |> String.fromInt
      in 
        { model | dir = d, pos = pos, walked = walked, steps = rest, twice = twice, seen = seen }

updateTogglePlay : Model -> Model
updateTogglePlay model = 
  if model.finished then 
    let 
      m = initModel 
    in 
      {m | paused = False }
  else 
    { model | paused = not model.paused }

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Clear -> 
      (updateClear model, Cmd.none)
    Tick ->
      (updateStep model, Cmd.none)
    Step ->
      (updateStep model, Cmd.none)
    Faster -> 
      ({model | tickInterval = model.tickInterval / 2 }, Cmd.none)
    Slower -> 
      ({model | tickInterval = model.tickInterval * 2 }, Cmd.none)
    TogglePlay -> 
      (updateTogglePlay model, Cmd.none)

-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
  let 
    tickSub = if model.paused then Sub.none else Time.every model.tickInterval (\_ -> Tick)
  in 
    tickSub

-- VIEW

posToStr : Pos -> String
posToStr (x, y) = 
  String.fromInt x ++ ", " ++ String.fromInt y

toPolyline : List Pos -> Html Msg
toPolyline path = 
  -- <polyline points="60, 110 65, 120 70, 115 75, 130 80, 125 85, 140 90, 135 95, 150 100, 145"/>
  let 
    ptsStr = path |> List.map posToStr |> String.join " " 
    attrs = [ stroke "currentcolor", strokeWidth "1px", fill "none", points ptsStr ]
  in 
    Svg.polyline attrs []

toSvg : Model -> Html Msg 
toSvg model = 
  let 
    topLeft = (-30, -60)
    botRight = (170, 160)
    positions = model.path |> List.map Tuple.second
    polyline = toPolyline model.walked
    maybeTwiceElement = 
      case model.twice of 
        Just (xt, yt) -> [ Svg.circle [ cx (String.fromInt xt), cy (String.fromInt yt), r "3", stroke "currentcolor", strokeWidth "1px", fill "none" ] [] ]
        Nothing -> []
    elements = List.append [ polyline ] maybeTwiceElement
  in 
    svg
      [ viewBox "-170 -170 340 340"
      , width "340"
      , height "340"
      , Svg.Attributes.style "max-width: 100%"
      ]
      elements

manhattan : Pos -> Int 
manhattan (x, y) = 
  abs x + abs y 

view : Model -> Html Msg
view model =
  let
    -- Grid: (-29, -58) to (168, 158)
    svgElement = toSvg model 
    distanceStr = manhattan model.pos |> String.fromInt
    twiceStr = 
      case model.twice of 
        Just p -> manhattan p |> String.fromInt
        Nothing -> "?"
  in 
    Html.table 
      [ Html.Attributes.align "center"
      , Html.Attributes.style "width" "100%"
      , Html.Attributes.style "font-family" "Courier New" ]
      [ Html.tr 
          [] 
          [ Html.td 
              [ Html.Attributes.align "center"
              , Html.Attributes.style "font-family" "Courier New"
              , Html.Attributes.style "font-size" "32px"
              , Html.Attributes.style "padding" "20px"]
              [ Html.div [] [Html.text "Advent of Code 2016" ]
              , Html.div [] [Html.text "Day 1: No Time for a Taxicab" ] ] ]
      , Html.tr 
          []
          [ Html.td 
              [ Html.Attributes.align "center"
              , Html.Attributes.style "padding-bottom" "10px" ]
              [ Html.a 
                [ Html.Attributes.href "https://adventofcode.com/2024/day/6" ] 
                [ Html.text "https://adventofcode.com/2024/day/6" ]
            ] ]
      , Html.tr 
          []
          [ Html.td 
              [ Html.Attributes.align "center"
              , Html.Attributes.style "padding" "10px" ]
              [ Html.button 
                [ Html.Attributes.style "width" "80px", onClick Clear ] 
                [ Html.text "Clear"]
              , Html.button 
                [ Html.Attributes.style "width" "80px", onClick Slower ] 
                [ Html.text "Slower" ]
              , Html.button 
                [ Html.Attributes.style "width" "80px", onClick TogglePlay ] 
                [ if model.paused then Html.text "Play" else Html.text "Pause" ] 
              , Html.button 
                [ Html.Attributes.style "width" "80px", onClick Faster ] 
                [ Html.text "Faster" ]
              , Html.button 
                [ Html.Attributes.style "width" "80px", onClick Step ] 
                [ Html.text "Step" ]
            ] ]
      , Html.tr 
          []
          [ Html.td 
              [ Html.Attributes.align "center"
              , Html.Attributes.style "font-family" "Courier New"
              , Html.Attributes.style "font-size" "24px" ] 
              [ 
                Html.div [] [ Html.text distanceStr ]
              , Html.div [] [ Html.text twiceStr ]
              ] ]
      , Html.tr 
          []
          [ Html.td 
              [ Html.Attributes.align "center"
              , Html.Attributes.style "font-family" "Source Code Pro, monospace"
              , Html.Attributes.style "font-size" "4px"
              , Html.Attributes.style "padding" "10px" ] 
              [ 
                Html.div [] [ svgElement ]
              ] ] ]
