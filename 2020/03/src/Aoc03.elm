module Aoc03 exposing (..)

{- Advent of Code 2020. Day 3: Toboggan Trajectory. -}

import Browser exposing (Document)
import Html exposing (Html)
import Html.Attributes
import Html.Events exposing (onClick)
import Dict exposing (Dict)
import Array exposing (Array)
import Set exposing (Set)
import Array2D exposing (Array2D)
import Html exposing (text)
import Time

defaultTickInterval : Float
defaultTickInterval = 10

-- MAIN

main =
  Browser.document
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions }

-- MODEL

type DataSource = Input | Sample

type alias Pos = (Int, Int)

type Dir = N | W | S | E

type Move = Turn | Forward

type Visit = Vertical | Horizontal | Both 

type Cell = Highlight Char | Plain Char 

type alias Model = 
  { warehouse : Array2D Char
  , large : Bool 
  , wide : Bool 
  , robot : Pos 
  , dataSource : DataSource
  , moves : List Char 
  , paused : Bool 
  , finished : Bool 
  , tickInterval : Float 
  , message : String }

sample : String
sample = """..##.......
#...#...#..
.#....#..#.
..#.#...#.#
.#...##..#.
..#.##.....
.#.#.#....#
.#........#
#.##...#...
#...##....#
.#..#...#.#"""

input : String 
input = """....#...............##...#...#.
#...#..#.....##.##.#.##....#...
...#.....#...#.................
#..#..#.......#...#.#..........
...##..#.#..........##...#.....
........###.#.##..#............
...###......##.#..#.#...#.#....
......##..#.#....#...#.........
.................#......#......
..............##....#..........
#.....................#...#.#.#
.##..#............##...##.##..#
.....#.####...#..##......#.#..#
#.......#.#..#......##.#.#....#
.....##...###.#..........##....
#...........#.##....##.....#..#
..###..##.##.....#....#........
...#.#.#............#.#..#....#
#......#....#...##.#.#.#.#..#..
.......#.#...#..#..#..##......#
.....#..#.............#..#...#.
##..#.##.....#........#........
....##....#..#...........#...#.
.......#........##.......##....
..##...#.......#........##.#...
..........#..#.....##........#.
..#..##..#............#........
.#.#...#...#.......#......#....
....#....#.....#.#.........###.
.............#...#....#..#.#...
##.#...#..#......#.#.##.....#..
#...##.#..........#..#.#...#...
#####.......#.#.....#..#.......
#...#...#........#....#...#....
......##.#..#..#............#..
....#....#.......#...###.......
.#......##...#.##....#...#.....
..#....#...##.....#.#...##.#...
#.......#........#.####........
#.##..#..#.........#.#........#
.#...#.#.#.#......#....#.#..#..
#...####...##.##.#....#......##
..#...#......##........#.....#.
...#.#....##...................
...##................#.........
...##.....##........#....#..#..
.........#..#.....#............
.#..#.......................#..
.#.........#..##........#.#.#.#
......#.....##..#.##...#..#.##.
..#..............##.......#....
...............##....#...##..#.
###...#..###.........#...#.....
...#..#...#....#.....##........
....#..##...#........#.........
..#......#.......#.....#..#....
.#...........##.....###....#...
.#..#.....##.........##.....#..
....##.#.....#................#
..#..#......#.#..#....#..##....
#.....#...##............#......
.#.............#....#.......#..
#.........#..#...##.#...#.#.##.
...#......#..####....#.#.....#.
......#........#..........##.##
......##.#..##.##.....#........
##.....#..##.##..#.......##....
.##.........#...........#...#..
.....#...###..#...#...........#
..........#.#......#.###.....#.
...#.............#.##......##..
#.##.........#..###...........#
....#..##....#..#..#........###
...#........##.......##..#..#..
...#......#..#.#...............
#......###....#.#..#.#..#....#.
#.#.####.#.........#..#.#.#....
.....#....#...............#...#
.#........#......#.#...#.......
................#...#.....##...
.............#...####..........
.................##....##.###..
#................#......#......
.###.#........#..##.....#..###.
..#.#..#...#..#.#...#.#.....#.#
.....#............#..##..#..#.#
#........##.#...#.....#........
#.#.#..###......###............
...#..#...........##...#.....#.
......#........#...#.#....#....
....#..........#.#..#.#....##..
...#.....##..#......#.#.##...#.
.........#..#................#.
..#....#.##.....#.......#......
...#.....#.......##.##.....#...
#...#..............#..###..#...
#.#......#.#....#........##..#.
...#...##...##..#.........#....
..#...#......##.#.#.#....##....
#.......#.......#..#..#........
.........#..#.....#....#.....##
.#......#.......#.#..#..#...#.#
..#....#.#..#..................
#.....####..........#.#.....#.#
.#..#.#.#....#.#.....#.#.......
....##......#..#.....#.#.#...#.
...##...#......##.#....##.#....
..#..##....#...#...........#...
.......#........#...##.#.......
#.#..#....##.#....##...........
.......#............#..##..##..
#.#.#.....#....##.#.#.#.....#..
##...#...#.......#..#...#.....#
##..##.##..........#........##.
..............#.....#..#..##...
.......#...#.........#....#.#..
...#..#..#....#.#....##........
..#.......#....#....##.........
#...#.....#..#.#...##....#.....
.....##..#..##..#..............
.....##............#....#.#....
..#.....#....##.#.....#..#.....
#...#..#..#......#.#.#..##.....
.............................##
#...#.#................#....#.#
.#.#.#....##......###..##......
#.....#..#.##.#.#.##...###.....
.........#............##..#....
.#..#...#....#.....#.#........#
...............#......#..#.....
...................###........#
.###..##..##.......#.#.........
#.........#......#....#.#...#..
.#.#....#.......#.#..##...##...
.#.....#....##.......#.#.....#.
.........#...#....#.#..........
....###..#..##.#...##....#..#..
...#.#..##.#.........###.#..#..
#...#...........#....#.........
....##...........#.#.#......###
#....#...........##..#.........
###....#.....#.......#....###..
.#.......#....#.#.#.#......#.#.
........#...............#.#.#..
....#.........#.....#...##.##.#
...#............#.............#
..........#..#.................
........#.....##............#.#
..#...##........#...#.....#.#..
....#........#.#.#..........#..
#.#...#...........#............
....#.#...##...........#.....#.
...........#.#..#.....#........
.....#..#..#..#.....#.#.....#.#
#.....#.......#.......#...#....
#.........#....#.#........#..#.
...#..#.........#.....#..#.....
...#..#.............#..........
.#.......#..........#.....#...#
.....#.#......#.......#....#...
...#.....#..#..##....##....#...
.#.#.#..#...#.....#....#.......
..##.#..........#.....#.#......
..#..#.............#...##..##..
.#.............#..#....##...#..
..#...#.....#.................#
..##.......#.....#...#....#....
.#..#.##.........#...#.#...#...
...##.......##..#.....##.##...#
........####.#.........#.......
..#.#...##.#..#..#.......##.#..
.#..#............###..#..#.....
#.....#.#...#.#.......#........
..........#......#.#...#...#...
..#......#..#..#.#...#.........
..###........#.#....#.#...##...
.#.....#..#.#......#........#..
.#...#..#...#....#.......#..#..
..#....#..#.....#.#........#...
#..#.#.........#..........#..#.
.#.....##....#.........#.#.#.#.
#.#...#.....#.#.#....#.#..#....
.........#...................#.
..#.....#..##...#..........#.#.
..............#....#.........#.
.#....#.....#..............##..
#...#...#.#........##.........#
....###....#.#....#.#.........#
.....#........#.....##.........
.#...##..##..#.........##......
............#.....#........#...
..#....#.......#......#..#.#.#.
#.......#.#...........#..##.#..
......#.##......#....#.......#.
.....#........#...###.....#....
###..........#........#.#.#....
.....#...#.#...#...#...##.....#
.##...#.#........#.#....#......
......#.........#.....#.#......
.....#.##.....###.#...#...##..#
.#.#.......##....#..#..#.##....
.####...###.#.#.#.#............
......#..##...#..........#.##.#
......#............#...........
.....#.#..#.......##...##......
......#........#..#....#.#.#.#.
#..#..#.....#..#.....#.......#.
.#...#.....#..............#....
.#....#..#.##.#............####
..........#....#.##...#.#......
...#.#.#.#.#.......#.........#.
##........#..##..#.........#...
..#......#...#..#.#.....#......
..#.#......#...#...#.#.........
........................##.....
...#.##.#........#...#.......#.
..#.#......#....##........#.#..
#......#.##........#..#......#.
.....#..#..#.............#.....
......#......#........#....#...
...#....###.....#..#.#....#....
#.......................#....#.
..#...#...................#....
....#..#.....##.#..#...#.....#.
...#.........#...#.......#.....
..#....#.....#...#...#.#####...
.....####......#...........#...
......#.#..........#...#.#.#..#
###..#.#....#..#...............
...#...###..#..#.#.#...........
.....#...#.##.#.#.###..##......
.........#...........#....##.#.
....#..#......#................
...........#..#..#...#.#.......
..#.....#......##.###..........
.........#...................#.
..........#...#.#....##........
..#...##....#....#.......#...##
#......#.....#...#...#...#.....
....##...#.#.......#.#...##....
...#.....#....#.....#....#.....
#....##.....##..##..........##.
.....#.....#.#.#...............
.#.##....#.....#.#..#....#..##.
.....#.#.....##....#...........
.........#..#.......##..##.....
..#....##.....###...#....#.#...
............#......#.#...#..#..
#..##......#.#.##....#.#.......
.#.#.....#...#.#.#....#.....#..
#....#..#.#....#...#...........
......#.#.....#...#.#.#......#.
###..#....#.###.............#..
..............#####........###.
..#..#.#.#.#......#......#.....
###.........#.#..........#..#.#
.#.........#...#......####.....
..#.......####..#....#...#..#..
#.#..#.#...............#.#.#.#.
###....#.....##.#....#......##.
..#..#........#....###.#.#.....
...#.#..........#.....#...#....
....#......##.#............#..#
...##...#.....#..##....#..#.#.#
.......#.....#..#....#....##.#.
.#..#....#..#......##....##...#
..#......#...#.#..###..#.##....
#...#.....#......##...#.......#
.....#.#.....#...##............
.#..##.##..#..##.#........#....
....#.#......##...#.#.#.#..##..
.#..............##........#....
.##....#..#..#....#...#......#.
............###....##.......##.
..............####.....#.......
........##..##.#...#.......#...
....#..#.....##.......#####...#
.##..##..#.....#...#..#..#....#
##..#.#.#...........#..........
#..#......#...#....#...........
...#..##.#..........#..#.......
........#.#.....#......##......
.....#....#............#.......
.#.#..#....##......#.......###.
.#..#.#........#......#...##..#
...#....#......#..#........#.##
.........#..#...#..#.#.##......
....###.#...........#...#......
.##............#.......#..##...
##...#.#...............#.#...##
..#..#.....#.#..#..#...........
..#..#.##..#......#.##..#.#....
..#...#......#.#...#....##.#...
...###....####......#....#...#.
.......##........#.....##....#.
.........##..........#...#.....
.....#............#.##.#....#.#
..........#...#....##..........
....................#......#...
#......#..#...#.............##.
...........#...................
..#...#.........#.##.#..##.#...
#.#....#.#.....#............#..
.#..#.....#.....####......#.#..
#....#.......##..#...........#.
............#...#.....#..#.#...
#...........#...#####....#...#.
..........#...###..##.........#
#.....###............#..#..#.#.
...##.....#....#......#.....#..
#....#.......#..#......###...#.
...##.##......##..##..........#
.......#.#..#.#..#.#.#.#..#..#.
..#..###...#....#.....#......#.
...#.........#..#.##.#.....###.
..#.........#.##.#..#..#..###..
..####..#.........#.........#.#
..#.#...#.......#....##........
.#......#.#....................
..........#.......#.#..#..#....
..#........#....#.#..#.........
..#.....#.............#....#...
##...#.........#.....#...#....."""

read : DataSource -> String
read dataSource = 
  case dataSource of 
    Input -> input
    Sample -> sample

widen text = 
    text |> String.replace "#" "##" |> String.replace "O" "[]" |> String.replace "." ".." |> String.replace "@" "@."

initModel : Bool -> DataSource -> Model 
initModel wide dataSource = 
  let 
    data = read dataSource
    (basicRows, moves) = 
      case String.split "\n\n" data of 
        a :: b :: _ -> (a |> String.split "\n", b |> String.toList)
        _ -> ([], [])
    rows = if wide then basicRows |> List.map widen else basicRows
    numberOfRows = rows |> List.length 
    numberOfCols = rows |> List.head |> Maybe.withDefault "?" |> String.length 
    warehouse = rows |> List.map (String.toList) |> Array2D.fromList
    robot = findRobot warehouse 
  in 
    { warehouse = warehouse
    , large = dataSource == Input 
    , wide = wide
    , robot = robot 
    , dataSource = dataSource
    , moves = moves
    , paused = True
    , finished = False  
    , tickInterval = defaultTickInterval 
    , message = "?" }

init : () -> (Model, Cmd Msg)
init _ =
  (initModel False Input, Cmd.none)

-- UPDATE

type Msg = 
  Tick 
  | Step 
  | TogglePlay 
  | ToggleWide 
  | Faster 
  | Slower 
  | Clear 
  | UseDataSource DataSource

getAllPositions : Array2D Char -> List Pos
getAllPositions board = 
  let
    ys = List.range 0 (Array2D.rows board - 1)
    xs = List.range 0 (Array2D.columns board - 1)
  in 
    ys |> List.concatMap (\y -> xs |> List.map (\x -> (x, y)))

findRobotLoop : Array2D Char -> List Pos -> Pos 
findRobotLoop warehouse positions = 
  case positions of 
    [] -> (0, 0)
    (x, y) :: rest -> 
      case Array2D.get y x warehouse of 
        Just '@' -> (x, y)
        _ -> findRobotLoop warehouse rest 

findRobot : Array2D Char -> Pos 
findRobot warehouse = 
  findRobotLoop warehouse (getAllPositions warehouse)

moveToOffset : Char -> Pos 
moveToOffset move = 
  case move of 
    '^' -> (0, -1)
    '<' -> (-1, 0)
    'v' -> (0, 1)
    '>' -> (1, 0)
    _ -> (0, 0)

moveStep move (x, y) = 
  let 
    (dx, dy) = moveToOffset move 
  in 
    (x + dx, y + dy)

zip : List a -> List b -> List (a, b) 
zip lst1 lst2 = 
  case (lst1, lst2) of 
    ((h1 :: r1), (h2 :: r2)) -> (h1, h2) :: zip r1 r2 
    _ -> []

tryFindSpaceDoubleLoop : Array2D Char -> Char -> List Pos -> List (Pos, Pos) -> List (Pos, Pos)
tryFindSpaceDoubleLoop warehouse move positionsToMove swaps = 
  let 
    nextPositions = positionsToMove |> List.map (moveStep move)
    things = nextPositions |> List.map (\(x, y) -> Array2D.get y x warehouse |> Maybe.withDefault '?')
    proposedSwaps = zip positionsToMove nextPositions
    nextSwaps = proposedSwaps ++ swaps 
  in 
    if things |> List.any (\ch -> ch == '#') then 
      -- Met a wall! 
      []
    else if things |> List.all (\ch -> ch == '.') then 
      -- Free space for all!
      nextSwaps 
    else 
      -- Boxes...
      let 
        positionsAndThings = nextPositions |> List.map (\(x, y) -> ((x, y), Array2D.get y x warehouse |> Maybe.withDefault '?'))
        openPositions = positionsAndThings |> List.filterMap (\(p, ch) -> if ch == '[' then Just p else Nothing)
        closePositions = positionsAndThings |> List.filterMap (\(p, ch) -> if ch == ']' then Just p else Nothing)
        toTheRight (x, y) = (x + 1, y)
        toTheLeft (x, y) = (x - 1, y)
        rightPositions = openPositions |> List.map toTheRight 
        leftPositions = closePositions |> List.map toTheLeft 
        nextPositionsToMove = (openPositions ++ rightPositions ++ closePositions ++ leftPositions) |> Set.fromList |> Set.toList 
      in 
        tryFindSpaceDoubleLoop warehouse move nextPositionsToMove nextSwaps

tryFindSpaceDouble : Array2D Char -> Pos -> Char -> List (Pos, Pos)
tryFindSpaceDouble warehouse robot move = 
  tryFindSpaceDoubleLoop warehouse move [ robot ] []

tryFindSpaceSimpleLoop : Array2D Char -> Char -> Pos -> List (Pos, Pos) -> List (Pos, Pos)
tryFindSpaceSimpleLoop warehouse move pos swaps = 
  let 
    (dx, dy) = moveToOffset move 
    (x, y) = moveStep move pos
    nextSwaps = (pos, (x, y)) :: swaps 
  in 
    case Array2D.get y x warehouse of 
      Just '#' -> []
      Just '.' -> nextSwaps 
      Just '[' -> 
        tryFindSpaceSimpleLoop warehouse move (x, y) nextSwaps 
      Just ']' -> 
        tryFindSpaceSimpleLoop warehouse move (x, y) nextSwaps 
      Just 'O' -> 
        tryFindSpaceSimpleLoop warehouse move (x, y) nextSwaps 
      _ -> [] 

tryFindSpaceSimple : Array2D Char -> Pos -> Char -> List (Pos, Pos) 
tryFindSpaceSimple warehouse robot move = 
  tryFindSpaceSimpleLoop warehouse move robot []

tryFindSpace : Bool -> Array2D Char -> Pos -> Char -> List (Pos, Pos) 
tryFindSpace wide warehouse robot move = 
  if wide then 
    case move of 
      '^' -> tryFindSpaceDouble warehouse robot move 
      'v' -> tryFindSpaceDouble warehouse robot move 
      '<' -> tryFindSpaceSimple warehouse robot move 
      '>' -> tryFindSpaceSimple warehouse robot move 
      _ -> tryFindSpaceSimple warehouse robot move 
  else 
    tryFindSpaceSimple warehouse robot move

moveStuff : Array2D Char -> List (Pos, Pos) -> Array2D Char
moveStuff warehouse swaps = 
  case swaps of
    [] -> warehouse 
    ((x1, y1), (x2, y2)) :: rest -> 
      let 
        maybeCell1 = Array2D.get y1 x1 warehouse
        maybeCell2 = Array2D.get y2 x2 warehouse
      in 
        case (maybeCell1, maybeCell2) of 
          (Just cell1, Just cell2) -> 
            let 
              wh = warehouse |> Array2D.set y1 x1 cell2 |> Array2D.set y2 x2 cell1 
            in 
              moveStuff wh rest  
          _ -> 
            let 
              wh = Array2D.set 0 0 '?' warehouse  
              wh2 = Array2D.set 3 3 '?' wh  
            in 
              moveStuff wh2 rest

tryMoveRobot : Bool -> Array2D Char -> Pos -> Char -> (Array2D Char, Pos) 
tryMoveRobot wide warehouse robot move = 
  let 
    swaps = tryFindSpace wide warehouse robot move
  in 
    if List.length swaps == 0 then 
      (warehouse, robot) 
    else 
      let 
        wh = moveStuff warehouse swaps
        rb = moveStep move robot
      in 
        (wh, rb)

swapsToText swaps = 
  case swaps of 
    [] -> ""
    ((x1, y1), (x2, y2)) :: rest -> 
      let 
        x1s = String.fromInt x1
        y1s = String.fromInt y1
        x2s = String.fromInt x2
        y2s = String.fromInt y2
        s = "(" ++ x1s ++ "," ++ y1s ++ ") -> (" ++ x2s ++ "," ++ y2s ++ ")"
      in 
        s ++ (swapsToText rest)

gpsCoordinate : Pos -> Int 
gpsCoordinate (x, y) = 
    y * 100 + x

updateClear : Model -> Model
updateClear model = 
  initModel model.wide model.dataSource

updateStep : Model -> Model
updateStep model = 
  let 
    wide = model.wide 
    warehouse = model.warehouse 
    robot = model.robot
  in 
    case model.moves of 
      [] -> model 
      move :: movesLeft -> 
        let 
          (wh, rb) = tryMoveRobot wide warehouse robot move
          (rx, ry) = rb 
        in 
          { model | warehouse = wh, robot = rb, moves = movesLeft, message = ""  }

updateKey : Char -> Model -> Model
updateKey move model = 
  let 
    wide = model.wide 
    warehouse = model.warehouse 
    robot = model.robot
    (wh, rb) = tryMoveRobot wide warehouse robot move
    (rx, ry) = rb 
    in 
      { model | warehouse = wh, robot = rb, message = ""  }

updateTogglePlay : Model -> Model
updateTogglePlay model = 
  { model | paused = not model.paused }

updateToggleWide : Model -> Model
updateToggleWide model = 
  initModel (not model.wide) model.dataSource 

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
    ToggleWide -> 
      (updateToggleWide model, Cmd.none)
    UseDataSource dataSource -> 
      (initModel model.wide dataSource, Cmd.none)

-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
  let 
    tickSub = if model.paused then Sub.none else Time.every model.tickInterval (\_ -> Tick)
  in 
    Sub.batch [ tickSub ] 

-- VIEW

toWarehouseRows : Array2D Char -> List String 
toWarehouseRows warehouse = 
  let
    ys = List.range 0 (Array2D.rows warehouse - 1)
    xs = List.range 0 (Array2D.columns warehouse - 1)
    rows = ys |> List.map (\y -> xs |> List.filterMap (\x -> Array2D.get y x warehouse) |> String.fromList)
  in 
    rows 

toRowElements : String -> List (Html Msg)
toRowElements rowText = 
  [ Html.text rowText, Html.br [] [] ]

isBox wh (x, y) = 
  if (Array2D.get y x wh == Just 'O' || Array2D.get y x wh == Just '[') then Just (x, y) else Nothing

view : Model -> Document Msg
view model = 
  { title = "Advent of Code 2020 | Day 3: Toboggan Trajectory"
  , body = [ viewBody model ] }

viewBody : Model -> Html Msg
viewBody model =
  let
    textFontSize = if model.large then "12px" else "28px"
    warehouse = model.warehouse
    rows = toWarehouseRows warehouse
    -- Insert robot symbol.
    elements = rows |> List.concatMap (toRowElements)

    gpsSum = 
      if List.isEmpty model.moves then
        let 
          positions = warehouse |> getAllPositions
          boxPositions = positions |> List.filterMap (isBox warehouse)  
        in 
          boxPositions |> List.map gpsCoordinate |> List.sum |> String.fromInt 
      else 
        "?"
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
              , Html.div [] [Html.text "Day 3: Toboggan Trajectory" ] ] ]
      , Html.tr 
          []
          [ Html.td 
              [ Html.Attributes.align "center"
              , Html.Attributes.style "padding-bottom" "10px" ]
              [ 
                Html.text " ["
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
              , Html.text "] " ] 
          ]
      , Html.tr 
          []
          [ Html.td 
              [ Html.Attributes.align "center"
              , Html.Attributes.style "padding-bottom" "10px" ]
              [ Html.a 
                [ Html.Attributes.href "https://adventofcode.com/2020/day/3" ] 
                [ text "https://adventofcode.com/2020/day/3" ]
            ] ]
      , Html.tr 
          []
          [ Html.td 
              [ Html.Attributes.align "center" ]
              [ 
                Html.input 
                [ Html.Attributes.type_ "radio", onClick (UseDataSource Input), Html.Attributes.checked (model.dataSource == Input) ] 
                []
              , Html.label [] [ Html.text "Input" ]
              , 
                Html.input 
                [ Html.Attributes.type_ "radio", onClick (UseDataSource Sample), Html.Attributes.checked (model.dataSource == Sample) ] 
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
                [ text "Reset" ]
              , Html.button 
                [ Html.Attributes.style "width" "80px", onClick Slower ] 
                [ text "Slower" ]
              , Html.button 
                [ Html.Attributes.style "width" "80px", onClick TogglePlay ] 
                [ if model.paused then text "Play" else text "Pause" ] 
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
              [ Html.Attributes.align "center" ]
              [ Html.input 
                [ Html.Attributes.type_ "checkbox", onClick ToggleWide, Html.Attributes.checked model.wide ] 
                []
              , Html.label [] [ Html.text " Wide" ]
            ] ]
      , Html.tr 
          []
          [ Html.td 
              [ Html.Attributes.align "center"
              , Html.Attributes.style "background-color" "white" 
              , Html.Attributes.style "font-family" "Courier New"
              , Html.Attributes.style "font-size" "24px"
              , Html.Attributes.style "padding-top" "10px"
              , Html.Attributes.style "width" "200px" ] 
              [ 
                Html.div [] [ Html.text gpsSum ]
              ] ]
      , Html.tr 
          []
          [ Html.td 
              [ Html.Attributes.align "center"
              , Html.Attributes.style "background-color" "white" 
              , Html.Attributes.style "font-family" "Source Code Pro, monospace"
              , Html.Attributes.style "font-size" textFontSize
              , Html.Attributes.style "padding" "10px"
              , Html.Attributes.style "width" "200px" ] 
              [ 
                Html.div [] elements
              ] ] ]
