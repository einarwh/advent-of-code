module Aoc06 exposing (..)

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

type Mode = Part1 | Part2 

type alias Model = 
  { board : Array2D Char
  , vizBoard : Array2D Cell 
  , guardPos : Pos 
  , guardDir : Dir
  , routeWalked : List (Pos, Dir, Move)
  , routeRemaining : List (Pos, Dir, Move)
  , mode : Mode 
  , calculating : Bool
  , candidateObstructions : List Pos
  , verifiedLoops : Array Pos
  , currentLoopIndex : Int
  , dataSource : DataSource
  , paused : Bool 
  , finished : Bool 
  , tickInterval : Float 
  , calcInterval : Float 
  , message : String
  , counter : Int 
  , debug : String }

sample : String
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

input : String
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

initBoard : DataSource -> Array2D Char
initBoard dataSource = 
  let 
    data = 
      case dataSource of 
        Sample -> sample 
        Input -> input 
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

initModel : Mode -> DataSource -> Model 
initModel mode dataSource = 
  let 
    board = initBoard dataSource
    startPos = findStartPos board 
    (xStart, yStart) = startPos
    guardless = board |> Array2D.set yStart xStart '.'
    vizBoard  = board |> Array2D.map Plain
    dir = N
    route = walk board [] N Forward startPos
    candidateObstructions = route |> List.map (\(p, _, _) -> p)
    msg = (String.fromInt xStart) ++ "," ++ (String.fromInt yStart)
  in 
    { board = guardless
    , vizBoard = vizBoard
    , guardPos = startPos
    , guardDir = dir
    , routeRemaining = route
    , routeWalked = []
    , message = msg
    , dataSource = dataSource
    , calculating = False 
    , candidateObstructions = candidateObstructions
    , verifiedLoops = Array.empty
    , currentLoopIndex = 0
    , mode = mode 
    , paused = True
    , finished = False 
    , tickInterval = defaultTickInterval
    , calcInterval = 100
    , counter = 0
    , debug = "" }

init : () -> (Model, Cmd Msg)
init _ =
  (initModel Part1 Input, Cmd.none)

-- UPDATE

type Msg = 
  Tick 
  | Step 
  | TogglePlay 
  | Faster 
  | Slower 
  | Clear 
  | UseSample 
  | UseInput 
  | EnablePart1 
  | EnablePart2
  | Calculate

getAllPositions : Array2D Char -> List Pos
getAllPositions board = 
  let
    ys = List.range 0 (Array2D.rows board - 1)
    xs = List.range 0 (Array2D.columns board - 1)
  in 
    ys |> List.concatMap (\y -> xs |> List.map (\x -> (x, y)))

updateClear : Model -> Model
updateClear model = 
  initModel model.mode model.dataSource

updateStep : Model -> Model
updateStep model = 
  let 
    dir = model.guardDir 
  in 
    case model.routeRemaining of 
      (p, d, m) :: rest -> 
        -- Update vizBoard!
        let 
          guardPos = p 
          (x, y) = p
          guardDir = d 
          newCell = 
            case m of 
              Turn -> Highlight '+'
              Forward -> 
                case Array2D.get y x model.vizBoard of 
                  Nothing -> Plain '?' 
                  Just cell ->
                    case cell of 
                      Highlight ch -> 
                        case ch of 
                          '|' -> Highlight (if dir == E || dir == W then '+' else '|')
                          '-' -> Highlight (if dir == N || dir == S then '+' else '-')
                          _ -> Highlight '?'
                      Plain _ -> 
                        if dir == E || dir == W then Highlight '-' else Highlight '|'

          vizBoard = Array2D.set y x newCell model.vizBoard 
        in 
          { model | vizBoard = vizBoard
          , guardPos = guardPos
          , guardDir = guardDir
          , routeRemaining = rest
          , routeWalked = (p, d, m) :: model.routeWalked }
      _ -> 
        { model | paused = True, finished = True }

updateTogglePlay : Model -> Model
updateTogglePlay model = 
  if model.finished then 
    let 
      m = initModel model.mode model.dataSource
    in 
      {m | paused = False }
  else 
    { model | paused = not model.paused }

updateUseSample : Model -> Model
updateUseSample model = 
  initModel model.mode Sample 

updateUseInput : Model -> Model
updateUseInput model = 
  initModel model.mode Input 

findLoop : List (Pos, Dir) -> Dir -> Pos -> Array2D Char -> Bool 
findLoop visited dir pos board = 
  if List.member (pos, dir) visited then True 
  else 
    let 
      (x, y) = moveForward dir pos
    in 
      case Array2D.get y x board of 
        Nothing -> False 
        Just '#' -> findLoop visited (turnRight dir) pos board 
        _ -> findLoop ((pos, dir) :: visited) dir (x, y) board

hasLoop : Pos -> Array2D Char -> Bool 
hasLoop startPos board =
  findLoop [] N startPos board

addObstruction : Array2D Char -> Pos -> Array2D Char 
addObstruction board (x, y) =
  Array2D.set y x '#' board 

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
    EnablePart1 -> 
      ({model | mode = Part1 }, Cmd.none)
    EnablePart2 -> 
      let 
        freshModel = initModel Part2 model.dataSource
        m = { freshModel | calculating = True }
      in 
        (m, Cmd.none)
    TogglePlay -> 
      (updateTogglePlay model, Cmd.none)
    UseSample -> 
      (updateUseSample model, Cmd.none)
    UseInput -> 
      (updateUseInput model, Cmd.none)
    Calculate -> 
      let 
        batchSize = 1
        candidates = model.candidateObstructions |> List.take batchSize 
      in 
        if List.length candidates > 0 then 
          let 
            remaining = model.candidateObstructions |> List.drop batchSize
            board = model.board
            startPos = model.guardPos 
            newVerified = 
              candidates 
              |> List.filterMap (\obs -> if hasLoop startPos (addObstruction board obs) then Just (obs) else Nothing)
              |> Array.fromList 
            verifiedLoops = Array.append model.verifiedLoops newVerified

          in 
            ({ model | verifiedLoops = verifiedLoops, candidateObstructions = remaining }, Cmd.none)
        else 
          ({model | calculating = False}, Cmd.none)

-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
  let 
    tickSub = if model.paused then Sub.none else Time.every model.tickInterval (\_ -> Tick)
    calcSub = if model.calculating then Time.every model.calcInterval (\_ -> Calculate) else Sub.none 
  in 
    Sub.batch [ tickSub, calcSub ]

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

toCharElement : Array2D Cell -> Pos -> Html Msg 
toCharElement vizBoard (x, y) = 
    case Array2D.get y x vizBoard of 
      Nothing -> Html.text "?"
      Just cell -> 
        case cell of 
          Highlight ch -> 
            (Html.span [Html.Attributes.style "background-color" "#CCCCCC" ] [ Html.text (String.fromChar ch) ]) 
          Plain ch -> 
            Html.text (String.fromChar ch)

view : Model -> Document Msg
view model = 
  { title = "Advent of Code 2024 | Day 6: Guard Gallivant (Part 1)"
  , body = [ viewBody model ] }

viewBody : Model -> Html Msg
viewBody model =
  let
    guardChar = 
      case model.guardDir of 
        N -> '^'
        W -> '<'
        S -> 'v'
        E -> '>'
    (xGuard, yGuard) = model.guardPos
    board = Array2D.set yGuard xGuard (Highlight guardChar) model.vizBoard 
    ys = List.range 0 (Array2D.rows board - 1)
    xs = List.range 0 (Array2D.columns board - 1)
    nestedPositions = ys |> List.map (\y -> xs |> List.map (\x -> (x, y)))
    nestedElements = nestedPositions |> List.map (\positions -> positions |> List.map (toCharElement board))
    elements = nestedElements |> List.foldr (\a b -> List.append a (Html.br [] [] :: b)) []
    positionsVisited = model.routeWalked |> List.map (\(p, _, _) -> p) |> Set.fromList |> Set.size 
    textFontSize = 
      case model.dataSource of 
        Sample -> "24px"
        Input -> "9px"

    (text1, text2) = 
      if model.calculating then 
        let 
          obsCountStr = String.fromInt (List.length model.candidateObstructions)
          loopCountStr = String.fromInt (Array.length model.verifiedLoops)
          str = obsCountStr ++ " candidates left, " ++ loopCountStr ++ " verified"
        in 
          ("Calculating", str)
      else 
        if model.mode == Part1 then 
          let 
            t1 = "(" ++ (String.fromInt xGuard) ++ ", " ++ (String.fromInt yGuard) ++ ")"
          in 
            (t1, String.fromInt positionsVisited)
        else ("Loop X of Y", String.fromInt positionsVisited)
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
              , Html.Attributes.style "padding" "20px"]
              [ Html.div [] [Html.text "Advent of Code 2024" ]
              , Html.div [] [Html.text "Day 6: Guard Gallivant (Part 1)" ] ] ]
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
                [ Html.Attributes.href "https://adventofcode.com/2024/day/6" ] 
                [ Html.text "https://adventofcode.com/2024/day/6" ]
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
                [ Html.text "Clear"]
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
              [ Html.Attributes.align "center"
              , Html.Attributes.style "background-color" "white" 
              , Html.Attributes.style "font-family" "Courier New"
              , Html.Attributes.style "font-size" "24px"
              , Html.Attributes.style "width" "200px" ] 
              [ 
                Html.div [] [ Html.text text1 ]
              , Html.div [] [ Html.text text2 ]
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
