module Main exposing (..)

{- Advent of Code 2024. Day 08: Resonant Collinearity. -}

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

type DataSource = Input | Sample

type alias Pos = (Int, Int)

type alias Model = 
  { board : Array2D Char
  , antinodes : Set (Int, Int)
  , dataSource : DataSource
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

initBoard : DataSource -> Array2D Char
initBoard dataSource = 
  let 
    data =
      case dataSource of 
        Sample -> sample
        Input -> input
  in 
    data |> String.split "\n" |> List.map (String.toList) |> Array2D.fromList

initModel : Bool -> DataSource -> Model 
initModel withHarmonics dataSource = 
  let 
    board = initBoard dataSource
  in 
    { board = board
    , antinodes = Set.empty
    , message = "?"
    , dataSource = dataSource
    , withHarmonics = withHarmonics
    , counter = 0
    , debug = "" }

init : () -> (Model, Cmd Msg)
init _ =
  (initModel False Input, Cmd.none)

-- UPDATE

type Msg = 
  Clear 
  | Solve 
  | UseSample 
  | UseInput
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
  initModel model.withHarmonics model.dataSource

inBounds : Array2D Char -> (Int, Int) -> Bool
inBounds board (x, y) = 
  case Array2D.get y x board of 
    Just _ -> True 
    Nothing -> False 

findAntinodes : Array2D Char -> (Pos, Pos) -> List (Int, Int)
findAntinodes board ((x1, y1), (x2, y2)) = 
  let 
    xd = x2 - x1 
    yd = y2 - y1 
    candidates = [ (x1 - xd, y1 - yd), (x2 + xd, y2 + yd) ]
  in 
    candidates |> List.filter (inBounds board)

unfold : (s -> Maybe (a, s)) -> s -> List a 
unfold generator state = 
  case generator state of 
    Just (a, s) -> a :: (unfold generator s)
    Nothing -> []

findAntinodesWithHarmonics : Array2D Char -> (Pos, Pos) -> List (Int, Int)
findAntinodesWithHarmonics board ((x1, y1), (x2, y2)) = 
  let 
    xd = x2 - x1 
    yd = y2 - y1 
    unfolder fn pos = if inBounds board pos then Just (pos, fn pos) else Nothing
    sub (x, y) = (x - xd, y - yd)
    add (x, y) = (x + xd, y + yd)
    subList = (x1, y1) |> unfold (unfolder sub)
    addList = (x1, y1) |> unfold (unfolder add)
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

findAllAntinodes : (Array2D Char -> ((Int, Int), (Int, Int)) -> List (Int, Int)) -> Array2D Char -> Set (Int, Int)
findAllAntinodes antinodeFinder board = 
  let 
    antennae = findAllAntennae board
    grouped = groupAntennae antennae
    all = grouped |> List.concatMap (\(a, posList) -> findAntinodesForAntenna board antinodeFinder posList)
  in 
    Set.fromList all 

updateSolve : Model -> Model 
updateSolve model = 
  let 
    antinodeFinder = if model.withHarmonics then findAntinodesWithHarmonics else findAntinodes
    board = model.board
    antinodes = findAllAntinodes antinodeFinder board
  in 
    { model | antinodes = antinodes }

updateUseSample : Model -> Model
updateUseSample model = 
  initModel model.withHarmonics Sample 

updateUseInput : Model -> Model
updateUseInput model = 
  initModel model.withHarmonics Input 

updateToggleHarmonics : Model -> Model
updateToggleHarmonics model = 
  let
    withHarmonics = not model.withHarmonics
  in
    initModel withHarmonics model.dataSource 

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Clear -> 
      (updateClear model, Cmd.none)
    Solve -> 
      (updateSolve model, Cmd.none)
    UseSample -> 
      (updateUseSample model, Cmd.none)
    UseInput -> 
      (updateUseInput model, Cmd.none)
    ToggleHarmonics -> 
      (updateToggleHarmonics model, Cmd.none)

-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none

-- VIEW

toCharElement : Array2D Char -> Set (Int, Int) -> Pos -> Html Msg 
toCharElement board antinodes (x, y) = 
  case Array2D.get y x board of 
    Nothing -> Html.text "?"
    Just ch ->
        if ch == '.' && Set.member (x, y) antinodes then 
          Html.text "#"
        else 
          Html.text (String.fromChar ch) 

toStyledCharElement : Array2D Char -> Set (Int, Int) -> Pos -> Html Msg 
toStyledCharElement board antinodes (x, y) = 
  case Array2D.get y x board of 
    Nothing -> Html.text "?"
    Just ch ->
      case ch of 
        '.' -> 
          if Set.member (x, y) antinodes then 
            Html.span [Html.Attributes.class "draw-dark-green adaptive" ] [ Html.text "#" ]
          else 
            Html.span [Html.Attributes.class "draw-empty adaptive" ] [ Html.text "." ]
        _ -> 
          Html.text (String.fromChar ch) 

view : Model -> Html Msg
view model =
  let
    board = model.board 
    antinodes = model.antinodes
    ys = List.range 0 (Array2D.rows board - 1)
    xs = List.range 0 (Array2D.columns board - 1)
    nestedPositions = ys |> List.map (\y -> xs |> List.map (\x -> (x, y)))
    nestedElements = nestedPositions |> List.map (\positions -> positions |> List.map (toStyledCharElement board antinodes))
    elements = nestedElements |> List.foldr (\a b -> List.append a (Html.br [] [] :: b)) []
    textFontSize = 
      case model.dataSource of 
        Sample -> "24px"
        Input -> "16px"
    countStr = antinodes |> Set.size |> String.fromInt    
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
              , Html.Attributes.style "padding" "10px"]
              [ Html.div [] [Html.text "Advent of Code 2024" ]
              , Html.div [] [Html.text "Day 8: Resonant Collinearity" ] ] ]
      , Html.tr 
          []
          [ Html.td 
              [ Html.Attributes.align "center"
              , Html.Attributes.style "padding-bottom" "10px" ]
              [ Html.a 
                [ Html.Attributes.href "https://adventofcode.com/2024/day/8" ] 
                [ text "https://adventofcode.com/2024/day/8" ]
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
                [ Html.text "Clear" ] 
              , Html.button 
                [ Html.Attributes.style "width" "80px", onClick Solve ] 
                [ text "Solve" ] 
            ] ]
      , Html.tr 
          []
          [ Html.td 
              [ Html.Attributes.align "center" ]
              [ Html.input 
                [ Html.Attributes.type_ "checkbox", onClick ToggleHarmonics, Html.Attributes.checked model.withHarmonics ] 
                []
              , Html.label [] [ Html.text " With harmonics" ]
            ] ]
      , Html.tr 
          []
          [ Html.td 
              [ Html.Attributes.align "center"
              , Html.Attributes.style "font-family" "Courier New"
              , Html.Attributes.style "font-size" "24px"
              , Html.Attributes.style "padding-top" "10px" ] 
              [ 
                Html.div [] [ Html.text countStr ]
              ] ]
      , Html.tr 
          []
          [ Html.td 
              [ Html.Attributes.align "center"
              , Html.Attributes.style "font-family" "Source Code Pro, monospace"
              , Html.Attributes.style "font-size" textFontSize
              , Html.Attributes.style "padding" "10px" ] 
              [ 
                Html.div [
                  Html.Attributes.style "max-width" "100%"
                ] elements
              ] ] ]
