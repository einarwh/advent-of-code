module Aoc08 exposing (..)

import Browser
import Html exposing (Html)
import Html.Attributes
import Html.Events exposing (onClick)
import Dict exposing (Dict)
import Array exposing (Array)
import Set exposing (Set)
import Array2D exposing (Array2D)
import Html exposing (text)

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

type Cell = Highlight Char | Plain Char 

type alias Model = 
  { board : Array2D Char
  , vizBoard : Array2D Cell 
  , useSample : Bool
  , message : String
  , counter : Int 
  , debug : String }

sample = """............
........0...
.....0......
.......0....
....0.......
......A.....
............
............
........A...
.........A..
............
............"""

input = """..................................................
................2.................................
......6.........x.0..G............................
..............x5......0..................S........
.....0............................................
..................................y..............e
..........................G...............O.......
.....................0........GO...............d..
.........................8..........e.............
.........6....................................e...
......z6..5...N..x...................eY...........
................6.........5..........Y.E..........
.........X.....N....................E.a...S.....4.
...........................N.2......d.............
...s..................92.....a...................4
............s....................GO........4......
...........................................d.....S
.....................X....N.......................
.........A........................................
.s.....................A....E.......a.........Y...
.....g....s..................E.....Y..............
.............o....................................
...............................3...............O..
.g.................F.3.y..........................
.......F................y.....................d...
..................................X...............
..8....5............X..Z..........................
..g.....8.....na..................................
......................................3...........
.............J.......x............S.Z.............
..2J....h.A...............Z.......................
......A.............................3.............
............J.......n.............................
.8......o....n...........Z........................
..................o..............y................
..F.........................D...............9H....
.................................1.............9..
..................................................
.........h.....n......................f...........
.h....................z..........j.........9......
.......oF............................j............
..........h......z...........7.....1.f............
........................7.......1...H...j........f
........................................f.........
...........................7.......H..............
................................H.................
.............z...........D........................
..............J....................Dj.............
....................................D.............
....................7.......1....................."""

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
    vizBoard  = board |> Array2D.map Plain
  in 
    { board = board
    , vizBoard = vizBoard
    , message = "blabl"
    , useSample = useSample
    , counter = 0
    , debug = "" }

init : () -> (Model, Cmd Msg)
init _ =
  (initModel False, Cmd.none)

-- UPDATE

type Msg = 
  Clear 
  | Solve 
  | ToggleSample 

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

-- let rec pairs lst = 
--     match lst with 
--     | [] -> []
--     | h :: t -> 
--         List.map (fun it -> (h, it)) t @ pairs t 

pairs : List a -> List (a, a) 
pairs lst = 
  case lst of 
    [] -> []
    h :: t -> 
      let 
        these = t |> List.map (\it -> (h, it))
        rest = pairs t 
      in 
        List.append these rest 

updateSolve : Model -> Model 
updateSolve model = model 

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
    Solve -> 
      (updateSolve model, Cmd.none)
    ToggleSample -> 
      (updateToggleSample model, Cmd.none)

-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none

-- VIEW

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

view : Model -> Html Msg
view model =
  let
    board = model.vizBoard 
    ys = List.range 0 (Array2D.rows board - 1)
    xs = List.range 0 (Array2D.columns board - 1)
    nestedPositions = ys |> List.map (\y -> xs |> List.map (\x -> (x, y)))
    nestedElements = nestedPositions |> List.map (\positions -> positions |> List.map (toCharElement board))
    elements = nestedElements |> List.foldr (\a b -> List.append a (Html.br [] [] :: b)) []
    textFontSize = if model.useSample then "32px" else "12px"

    (text1, text2) = ("Loop X of Y", "blibla")
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
              , Html.div [] [Html.text "Day 8: Resonant Collinearity" ] ] ]
      -- , Html.tr 
      --     []
      --     [ Html.td 
      --         [ Html.Attributes.align "center" ]
      --         [ Html.input 
      --           [ Html.Attributes.type_ "radio", onClick EnablePart1, Html.Attributes.checked (model.mode == Part1) ] 
      --           []
      --         , Html.label [] [ Html.text "Part 1" ]
      --         , Html.input 
      --           [ Html.Attributes.type_ "radio", onClick EnablePart2, Html.Attributes.checked (model.mode == Part2) ] 
      --           []
      --         , Html.label [] [ Html.text "Part 2" ]
      --       ] ]
      , Html.tr 
          []
          [ Html.td 
              [ Html.Attributes.align "center"
              , Html.Attributes.style "padding" "10px" ]
              [ Html.button 
                [ Html.Attributes.style "width" "80px", onClick Clear ] 
                [ Html.text "Clear" ] 
              , Html.button 
                [ Html.Attributes.style "width" "80px", onClick Solve ] 
                [ text "Solve" ] 
              , Html.button 
                [ Html.Attributes.style "width" "80px", onClick ToggleSample ] 
                [ Html.text (if model.useSample then "Input" else "Sample") ]
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
