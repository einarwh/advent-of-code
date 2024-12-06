module Aoc06 exposing (..)

import Browser
import Html exposing (Html)
import Html.Attributes
import Html.Events exposing (onClick)
import Dict exposing (Dict)
import Array exposing (Array)
import Set exposing (Set)
import Array2D exposing (Array2D)
import Html exposing (text)
import Time

defaultDelay : Float
defaultDelay = 200

-- MAIN

main =
  Browser.element
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions }

-- MODEL

type alias Pos = (Int, Int)

type Dir = N | W | S | E

type Move = Turn | Forward

type Visit = Vertical | Horizontal | Both 

type alias Model = 
  { found : Int 
  , board : Array2D Char
  , guardPos : Pos 
  , guardDir : Dir
  , routeWalked : List (Pos, Dir, Move)
  , routeRemaining : List (Pos, Dir, Move)
  , useSample : Bool
  , paused : Bool 
  , delay : Float 
  , message : String
  , counter : Int 
  , debug : String }

sample = """....#.....
.........#
..........
..#.......
.......#..
..........
.#..^.....
........#.
#.........
......#..."""

input = """........#.............................................#.........#..............#.......#....................#....................#
......................#........#........................#.............##............................#.#.............#..........#..
....#..................................#..................#.........#....#..............#..#......................#........#...#..
.....#...#...............#................#..........................#......#.....................#.......................#.......
......................................................##.#....#..................................................................#
#....#....................#..................................#.....................................#......................#.......
.......#...........................#.......................#......#...#.................................................#...#.....
....................##..........................#................#..........................#.#..................#....#.........#.
#..................................#...............#...............#.........................#............##......................
.............................#....................................#...............#.........#..........#.......................#..
.................................#............###.......#............##......#...............................#...............#.#..
.#..........................................#.......#.......................##..#........#........#...............#...............
......#...#............#.#..............#...................#..........................................................##.........
#................##....................................................................#........#.................#...#...........
...#..........#..........................##......#....#..............................#......................................#.....
..............................##.#..........................................................#..............................#......
......#...........#....#..................#............#....................................................................#.....
.....................#........................#.......................................................#...........................
...................#.#..........................................................................................#.#...............
.........................................##.........................................................#.......#...###......#........
...............................................................##.......................#..........................#..............
...#...........#.................#..............#......#.....#..........................#....................#....#...............
.................................#................#................#..............................................#.......##.....#
........#..........................#................#.......................#...................#.................................
.............................#.............................#..........#.........................#........#........................
....#................#.#................#...........................................................#...#....#......#......#......
........#..............................#...............................#.................#........................................
...........................................#..#........#...#...........#..#.............#.........................................
.#.................#.#................................#................#.#..................................#........#............
.......#..........................................#.................................................................#.............
...................#....................................................#..#...............................................#......
...#.#........#...#.....#.....................................................................................................#...
......#.........................#......................#.............#.....................#......................................
..........................#............#.................................#..#..........#........#......................#..........
...#..#......#.......................#........................#...................................................................
............#...........#................#..............#.........................................................................
.#........#....#.........................................#........................................#...............................
...#......................##.........#.......#.#...........#................................................#...#.................
...........#......#.........#....#.....#.......................................................#.............#....................
.............#...#..............................................#.........#.....##....................#...#.........#.............
...............#................................#..........................................#.....#.#.........................#...#
................#.....#..#.............#..............#..............#.......#....................................................
......#.........................................................................................................#.........##......
..#...........................#.#...........................................#..#.....#............................................
...#...#..................#..................................#.........#...................#..................................#...
............#..#.....................#.........................................#.......#..................................#.......
...#.......................................................................#.............................#........................
......#..................#....................................#......##...........................................................
.#...............#.....#..........................................................................................#...............
...#.............#.........................#.....................#...#..........#....#............................#...............
...#............................#.......#...........#.........#........#.....#...........#...................................#....
.....................#...................#................................#...............................#......#.........#..##..
.............#........................................................................................#........................##.
.....#.......#.......#............................................................................................................
............................................#................................................................#..............#.....
....#..#......................................................#...................................................#...............
.................................#...##..................#.................................................#....#.................
........#...................................#...#..........................................................#......................
........................#........##...........#.......#........#...................................#..............................
...........#...#...................................................#..............#......#...............#.#..................#...
........#.............................................#...........................................................................
........#................#......#..........#..#........#.#.......#...#........................#..#......................#.......#.
.......#..........................................................................................................#.....#.....#...
......................##....................#.....#.................#...........#.......................#.##..........#......#....
....................................##....................................................................#.................#.....
..#.................................#.#...........#............................................#....#.............................
#...................................#...........................................#............#.........#...............#..........
.......................#.............................................................#..........#...#.............#.....#.........
#..................#.#.............................................................................#.................#............
.....................#......#.......#...........#..........................................^...............................#......
................................#......................#.#.....#..............#..................#....#.#......#....#.............
............................................................................#........#.....................................#......
.............#......................................................#...................................#......##..#....#.........
...............#.................#...................#...............#............................................................
..............#...........#..#...........................................#...................................#..........#.........
..........#...........................#...................#..............#...#.......#..........#...................#.............
.............#.#..................................#....#..#............#.....................#................#...#.....#.........
...................#..........................................................#............##.....................................
...........................................#............#........................................................................#
..#..............................#....#...........................................................................................
..#..#............#...........................#...#...........#............#.....#.................#..............................
....#...................#.........#...............................................................................................
.......#..........................#....................................##........#.................##...#.#........#..#...........
.#........................#....#.......#.....#...................#.............................................#..................
..#...............................................................#................................................#..............
.....#........................................#...................................................................................
..........................................#..........................................................#............................
....................###.........................#............................#...#...........#............#.......................
.....#.....#...............#........#.#....#...................#...........#........#...........#.................................
................................................................................................#...##............................
......................#............#.........................#.......................#.....#...........#...#.#....................
............................#....................#..................#..............#..................................#...........
...........................#...................................#..............................................................#...
..................#................#.....................#...................#........#..............#............................
............#.............#.....................#..................#.........#......................#............#............#...
....#..........................#...............................................................................................#..
................#.....................................#.............................#...........#........................#........
#.##...........#.........................................#...............#..........................................#.#...........
........................................#..............#.....................#............#.....#.................................
.......#....##..#..#.....#........................................................................................................
....##.............#..............................#...............................................................................
.....#........................#.........#..#......#........#......................#.......#..............#..........#.......#.....
..#....#.........................#.#....................#..#.......#.............#..................#........#..................#.
..........##....#........................................................................#........................................
........#......#................................................................................................#................#
.....#.......#...#.............#....................................#..#........................#...#..............#....#.........
.............#...................................................##...#...........................................................
..............#.............................................#.............#................#.............#..............#.........
..#....#..........#.....#......#......................#................................#...............................#..........
.................................................................................##......#...........................#.....#......
..#....#..........#.......#.........................................#...............#..#............................#........#....
...........#........#.#....................#........#.....................................#..................#....................
.......#............#................#...........#.............#......#.........#...........................#.....................
#.#....#.#...............#....#.....#.......................#...............................#...................##...#............
.................##...#.................................................................#...........................#.............
.....#........................................#.........#...................................................#.....................
....................#.....................#................................................#...##.....................#...........
...............#...............................................................#....#.............................................
.................#................#...#.....................................#......#....................#...............#.#.......
...................#............................................#..#....#..#........#...............................#.........#...
..#........#...............#..................#.....#..................#.....................................#........##...#......
.....#.....#....#.....#................#.#.....................##.............................................................#...
................#............#......#......#.......................................................#....................#........#
........#..............#..#......................................................................##.....##........................
.........#.............................#.........#.......................................#..........................#.##..........
.#...................#........................................................#....#......................................#.......
....#..........#.....#.........................................................................................#..................
.......#.............................#............#.........................#....#....#........#......#.....#.......#.............
..#.......#........................#........................#.....................................................................
........................#...............................#.#.............#................................#..................#....."""

initBoard : Bool -> Array2D Char
initBoard useSample = 
  let 
    data = if useSample then sample else input 
  in 
    data |> String.split "\n" |> List.map (String.toList) |> Array2D.fromList

findStartPos : Array2D Char -> (Int, Int)
findStartPos board = 
  let 
    columns = Array2D.columns board 
    loop (x, y) = 
      case Array2D.get y x board of 
        Nothing -> (0, 0)
        Just '^' -> (x, y)
        _ -> 
          let 
            next = if (x + 1 == columns) then (0, y + 1) else (x + 1, y)
          in 
            loop next
  in 
    loop (0, 0)

moveForward : Dir -> Pos -> Pos 
moveForward dir (x, y) = 
  case dir of 
    N -> (x, y - 1)
    W -> (x - 1, y)
    S -> (x, y + 1)
    E -> (x + 1, y)

turnRight : Dir -> Dir 
turnRight dir = 
  case dir of 
    N -> E
    E -> S 
    S -> W
    W -> N

walk : Array2D Char -> List (Pos, Dir, Move) -> Dir -> Move -> Pos -> List (Pos, Dir, Move) 
walk board visited dir move pos = 
  let 
    nextPos = moveForward dir pos 
    (nextX, nextY) = nextPos 
  in 
    case Array2D.get nextY nextX board of 
      Nothing -> -- Leaving the board.
        ((pos, dir, move) :: visited) |> List.reverse
      Just '#' -> -- Found an obstacle.
        walk board visited (turnRight dir) Turn pos  
      _ -> -- Go ahead.
        walk board ((pos, dir, move) :: visited) dir Forward nextPos

initModel : Bool -> Model 
initModel useSample = 
  let 
    board = initBoard useSample
    startPos = findStartPos board 
    (xStart, yStart) = startPos
    guardless = board |> Array2D.set yStart xStart '.'
    dir = N
    route = walk board [] N Forward startPos
    msg = (String.fromInt xStart) ++ "," ++ (String.fromInt yStart)
  in 
    { found = 0
    , board = guardless
    , guardPos = startPos
    , guardDir = dir
    , routeRemaining = route
    , routeWalked = []
    , message = msg
    , useSample = useSample 
    , paused = True
    , delay = defaultDelay
    , counter = 0
    , debug = "" }


init : () -> (Model, Cmd Msg)
init _ =
  (initModel False, Cmd.none)

-- UPDATE

type Msg = Tick | Step | TogglePlay | Faster | Slower | Clear | ToggleSample

getAllPositions : Array2D Char -> List Pos
getAllPositions board = 
  let
    ys = List.range 0 (Array2D.rows board - 1)
    xs = List.range 0 (Array2D.columns board - 1)
  in 
    ys |> List.concatMap (\y -> xs |> List.map (\x -> (x, y)))

updateClear : Model -> Model
updateClear model = 
  initModel model.useSample

updateStep : Model -> Model
updateStep model = 
  let 
    z = 0
    pos = model.guardPos
    dir = model.guardDir 
  in 
    case model.routeRemaining of 
      (p, d, m) :: rest -> 
        { model | guardPos = p, guardDir = d, routeRemaining = rest, routeWalked = (p, d, m) :: model.routeWalked }
      _ -> 
        { model | paused = True }

updateTogglePlay : Model -> Model
updateTogglePlay model = 
  { model | paused = not model.paused }

updateToggleSample : Model -> Model
updateToggleSample model = 
  let
    useSample = not model.useSample
  in
    initModel useSample 

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
      ({model | delay = model.delay / 2 }, Cmd.none)
    Slower -> 
      ({model | delay = model.delay * 2 }, Cmd.none)
    TogglePlay -> 
      (updateTogglePlay model, Cmd.none)
    ToggleSample -> 
      (updateToggleSample model, Cmd.none)

-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
  if model.paused then Sub.none 
  else Time.every model.delay (\_ -> Tick)

-- VIEW

chooseChar : Model -> Pos -> Char -> Char 
chooseChar model pos ch = 
  if pos == model.guardPos then 
    case model.guardDir of 
      N -> '^'
      W -> '<'
      S -> 'v'
      E -> '>'
  else 
    let 
      dirs = model.routeWalked |> List.filterMap (\(p, d, m) -> if p == pos then Just (d, m) else Nothing)
    in 
      case dirs of 
        [] -> ch 
        [(dir, move)] -> 
          case move of 
            Turn -> '+'
            Forward -> 
              case dir of  
                N -> '|'
                W -> '-'
                S -> '|'
                E -> '-'
        _ -> '+'

toCharElement : Model -> Pos -> Html Msg 
toCharElement model (x, y) = 
    case Array2D.get y x model.board of 
      Nothing -> Html.text "?"
      Just ch -> 
        let
          useChar = chooseChar model (x, y) ch 
          str = String.fromChar useChar 
        in
          if model.routeWalked |> List.any (\(p, d, m) -> p == (x, y)) then 
            (Html.span [Html.Attributes.style "background-color" "#CCCCCC" ] [ Html.text str ]) 
          else 
            Html.text str 

view : Model -> Html Msg
view model =
  let
    board = model.board 
    ys = List.range 0 (Array2D.rows board - 1)
    xs = List.range 0 (Array2D.columns board - 1)
    nestedPositions = ys |> List.map (\y -> xs |> List.map (\x -> (x, y)))
    nestedElements = nestedPositions |> List.map (\positions -> positions |> List.map (toCharElement model))
    elements = nestedElements |> List.foldr (\a b -> List.append a (Html.br [] [] :: b)) []
    positionsVisited = model.routeWalked |> List.map (\(p, d, m) -> p) |> Set.fromList |> Set.size 
    textFontSize = if model.useSample then "36px" else "12px"
  in 
    Html.table 
      [ Html.Attributes.style "width" "1080px"]
      [ Html.tr 
          [] 
          [ Html.td 
              [ Html.Attributes.align "center"
              , Html.Attributes.style "font-family" "Courier New"
              , Html.Attributes.style "font-size" "40px"
              , Html.Attributes.style "padding" "20px"]
              [ Html.div [] [Html.text "Advent of Code 2024" ]
              , Html.div [] [Html.text "Day 6: Guard Gallivant" ] ] ]
      , Html.tr 
          []
          [ Html.td 
              [ Html.Attributes.align "center"
              , Html.Attributes.style "padding" "10px" ]
              [ Html.button 
                [ Html.Attributes.style "width" "80px", onClick ToggleSample ] 
                [ Html.text (if model.useSample then "Input" else "Sample") ]
              , Html.button 
                [ Html.Attributes.style "width" "80px", onClick Clear ] 
                [ Html.text "Clear" ] 
              , Html.button 
                [ Html.Attributes.style "width" "80px", onClick Step ] 
                [ Html.text "Step" ]
              , Html.button 
                [ Html.Attributes.style "width" "80px", onClick Slower ] 
                [ text "Slower" ]
              , Html.button 
                [ Html.Attributes.style "width" "80px", onClick TogglePlay ] 
                [ if model.paused then text "Play" else text "Pause" ] 
              , Html.button 
                [ Html.Attributes.style "width" "80px", onClick Faster ] 
                [ text "Faster" ]
            ] ]
      , Html.tr 
          []
          [ Html.td 
              [ Html.Attributes.align "center"
              , Html.Attributes.style "background-color" "white" 
              , Html.Attributes.style "font-family" "Courier New"
              , Html.Attributes.style "font-size" "24px"
              , Html.Attributes.style "width" "200px" ] 
              [ 
                Html.div [] [ Html.text (String.fromInt positionsVisited) ]
              ] ]
      -- , Html.tr 
      --     []
      --     [ Html.td 
      --         [ Html.Attributes.align "center"
      --         , Html.Attributes.style "background-color" "white" 
      --         , Html.Attributes.style "font-family" "Courier New"
      --         , Html.Attributes.style "font-size" "24px"
      --         , Html.Attributes.style "width" "200px" ] 
      --         [ 
      --           Html.div [] [ Html.text model.message ]
      --         ] ]
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
