module Aoc08Strength exposing (..)

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

type alias Model = 
  { board : Array2D Char
  , antinodes : Dict Pos Float
  , useSample : Bool
  , withHarmonics : Bool 
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

initModel : Bool -> Bool -> Model 
initModel withHarmonics useSample = 
  let 
    board = initBoard useSample
  in 
    { board = board
    , antinodes = Dict.empty
    , message = "blabl"
    , useSample = useSample
    , withHarmonics = withHarmonics
    , counter = 0
    , debug = "" }

init : () -> (Model, Cmd Msg)
init _ =
  (initModel False False, Cmd.none)

-- UPDATE

type Msg = 
  Clear 
  | Solve 
  | ToggleSample 
  | ToggleHarmonics 

getAllPositions : Array2D Char -> List Pos
getAllPositions board = 
  let
    ys = List.range 0 (Array2D.rows board - 1)
    xs = List.range 0 (Array2D.columns board - 1)
  in 
    ys |> List.concatMap (\y -> xs |> List.map (\x -> (x, y)))

updateClear : Model -> Model
updateClear model = 
  initModel model.withHarmonics model.useSample

inBounds : Array2D Char -> (Int, Int) -> Bool
inBounds board (x, y) = 
  case Array2D.get y x board of 
    Just _ -> True 
    Nothing -> False 

findAntinodes : Array2D Char -> (Pos, Pos) -> List (Pos, Float)
findAntinodes board ((x1, y1), (x2, y2)) = 
  let 
    xd = x2 - x1 
    yd = y2 - y1 
    candidates = [ (x1 - xd, y1 - yd), (x2 + xd, y2 + yd) ]
  in 
    candidates |> List.filter (inBounds board) |> List.map (\pos -> (pos, 1.0))

unfold : (s -> Maybe (a, s)) -> s -> List a 
unfold generator state = 
  case generator state of 
    Just (a, s) -> a :: (unfold generator s)
    Nothing -> []

findAntinodesWithHarmonics : Array2D Char -> (Pos, Pos) -> List (Pos, Float)
findAntinodesWithHarmonics board ((x1, y1), (x2, y2)) = 
  let 
    xd = x2 - x1 
    yd = y2 - y1 
    rd = 0.02
    unfolder fn (pos, r) = if inBounds board pos then Just ((pos, r), fn (pos, r)) else Nothing
    sub ((x, y), r) = ((x - xd, y - yd), r - rd)
    add ((x, y), r) = ((x + xd, y + yd), r - rd)
    subList = ((x1, y1), 1.0) |> unfold (unfolder sub)
    addList = ((x1, y1), 1.0) |> unfold (unfolder add)
  in 
    List.append subList addList 

checkAntenna : Array2D Char -> Pos -> Maybe Char 
checkAntenna board (x, y) = 
  case Array2D.get y x board of 
    Nothing -> Nothing
    Just '.' -> Nothing 
    Just ch -> Just ch

findAllAntennae : Array2D Char -> List (Char, Pos)
findAllAntennae board = 
  board
  |> getAllPositions
  |> List.filterMap (\pos -> checkAntenna board pos |> Maybe.map (\ch -> (ch, pos)))

gatherPositionsForAntenna : List (Char, Pos) -> Char -> List Pos
gatherPositionsForAntenna antennae antenna = 
  antennae |> List.filterMap (\(a, lst) -> if a == antenna then Just lst else Nothing)

groupAntennae : List (Char, Pos) -> List (Char, List Pos)
groupAntennae antennae = 
  let 
    distinct = antennae |> List.map (Tuple.first) |> Set.fromList |> Set.toList 
  in 
    distinct |> List.map (\ch -> (ch, gatherPositionsForAntenna antennae ch)) 

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

findAntinodesForAntenna board antinodeFinder positions = 
  positions
  |> pairs 
  |> List.concatMap (antinodeFinder board)

chooseStrongestAntinodes : List (Pos, Float) -> List (Pos, Float)
chooseStrongestAntinodes antinodes = 
  case antinodes of 
    [] -> []
    (pos, strength) :: t -> 
      let 
        strengths = antinodes |> List.filterMap (\(p, s) -> if p == pos then Just s else Nothing)
        strongest = List.maximum strengths |> Maybe.withDefault strength
        removed = t |> List.filter (\(p, s) -> p /= pos)
      in 
        (pos, strongest) :: (chooseStrongestAntinodes removed)

findAllAntinodes : (Array2D Char -> (Pos, Pos) -> List (Pos, Float)) -> Array2D Char -> Dict Pos Float
findAllAntinodes antinodeFinder board = 
  let 
    antennae = findAllAntennae board
    grouped = groupAntennae antennae
    all = grouped |> List.concatMap (\(a, posList) -> findAntinodesForAntenna board antinodeFinder posList)
  in 
    all |> chooseStrongestAntinodes |> Dict.fromList 

updateSolve : Model -> Model 
updateSolve model = 
  let 
    antinodeFinder = if model.withHarmonics then findAntinodesWithHarmonics else findAntinodes
    board = model.board
    antinodes = findAllAntinodes antinodeFinder board
  in 
    { model | antinodes = antinodes }

updateToggleSample : Model -> Model
updateToggleSample model = 
  let
    useSample = not model.useSample
  in
    initModel model.withHarmonics useSample 

updateToggleHarmonics : Model -> Model
updateToggleHarmonics model = 
  let
    withHarmonics = not model.withHarmonics
  in
    initModel withHarmonics model.useSample 

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Clear -> 
      (updateClear model, Cmd.none)
    Solve -> 
      (updateSolve model, Cmd.none)
    ToggleSample -> 
      (updateToggleSample model, Cmd.none)
    ToggleHarmonics -> 
      (updateToggleHarmonics model, Cmd.none)

-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none

-- VIEW

toCharElement : Array2D Char -> Dict Pos Float -> Pos -> Html Msg 
toCharElement board antinodes (x, y) = 
  case Array2D.get y x board of 
    Nothing -> Html.text "?"
    Just ch ->
      case antinodes |> Dict.get (x, y) of 
        Nothing -> Html.text (String.fromChar ch) 
        Just strength -> 
          if ch == '.' then 
            let 
              textElement = Html.text "#" 
              num = ((1.0 - strength) * 255.0) |> round
              alpha = String.fromFloat strength
              rgba = "rgba(0, 0, 0, " ++ alpha ++")"
              spanElement = Html.span [ Html.Attributes.style "color" rgba ] [ textElement ]
            in 
              spanElement
          else
            Html.text (String.fromChar ch) 

view : Model -> Html Msg
view model =
  let
    board = model.board 
    antinodes = model.antinodes
    ys = List.range 0 (Array2D.rows board - 1)
    xs = List.range 0 (Array2D.columns board - 1)
    nestedPositions = ys |> List.map (\y -> xs |> List.map (\x -> (x, y)))
    nestedElements = nestedPositions |> List.map (\positions -> positions |> List.map (toCharElement board antinodes))
    elements = nestedElements |> List.foldr (\a b -> List.append a (Html.br [] [] :: b)) []
    textFontSize = if model.useSample then "32px" else "12px"
    countStr = antinodes |> Dict.size |> String.fromInt 
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
              [ Html.Attributes.align "center" ]
              [ Html.input 
                [ Html.Attributes.type_ "checkbox", onClick ToggleHarmonics, Html.Attributes.checked model.withHarmonics ] 
                []
              , Html.label [] [ Html.text "With harmonics" ]
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
                Html.div [] [ Html.text countStr ]
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
