module Aoc10 exposing (..)

import Browser exposing (Document)
import Html exposing (Html)
import Html.Attributes
import Html.Events exposing (onClick)
import Dict exposing (Dict)
import Array exposing (Array)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Array2D exposing (Array2D)

unitSize : Int 
unitSize = 9

-- MAIN

main =
  Browser.document
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions }

-- MODEL

type alias Model = 
  { lines : List String
  , field : Array2D Char
  , startPos : (Int, Int)
  , loop : List (Int, Int)
  , steps : Int 
  , insideTiles : List (Int, Int)
  , insideCount : Int
  , debug : String }

findStartPosHelper : Array2D Char -> List (Int, Int) -> Maybe (Int, Int)
findStartPosHelper field indexes =
  case indexes of 
    [] -> Just (7, 7) 
    (x, y) :: rest -> 
      case Array2D.get y x field of 
        Just ch -> 
          if ch == 'S' then Just (x, y) 
          else findStartPosHelper field rest
        Nothing -> Just (x, y)

findStartPos : Array2D Char -> Maybe (Int, Int)
findStartPos field =
  findStartPosHelper field (getIndexes field)

northPipes = ['S','|', 'L', 'J']
westPipes = ['S','-', 'J', '7']
southPipes = ['S','|', '7', 'F']
eastPipes = ['S','-', 'L', 'F']

checkConnect : Char -> Array2D Char -> (Int, Int) -> List Char -> List Char -> Maybe (Int, Int)
checkConnect pipe field (x, y) sourcePipes targetPipes = 
  if List.member pipe sourcePipes then 
    case Array2D.get y x field of
      Just p -> 
        if List.member p targetPipes then 
          Just (x, y)
        else 
          Nothing 
      Nothing -> Nothing
  else Nothing

findConnections : Array2D Char -> (Int, Int) -> List (Int, Int)
findConnections field (x, y) = 
  case Array2D.get y x field of 
    Just pipe -> 
      [ checkConnect pipe field (x, y - 1) northPipes southPipes 
      , checkConnect pipe field (x - 1, y) westPipes eastPipes 
      , checkConnect pipe field (x, y + 1) southPipes northPipes 
      , checkConnect pipe field (x + 1, y) eastPipes westPipes ]
      |> List.filterMap identity
    Nothing -> []

getIndexes : Array2D Char -> List (Int, Int)
getIndexes field = 
  let 
    width = Array2D.columns field
    height = Array2D.rows field
  in 
    List.range 0 (height - 1) 
    |> List.concatMap (\y -> List.range 0 (width - 1) |> List.map (\x -> (x, y)))

findLoopHelper : Array2D Char -> (Int, Int) -> (Int, Int) -> List (Int, Int) -> List (Int, Int)
findLoopHelper field (x, y) prev positions = 
  case Array2D.get y x field of 
    Just pipe -> 
      if pipe == 'S' then positions 
      else 
        let 
          conns = findConnections field (x, y) 
          filtered = conns |> List.filter (\pos -> pos /= prev)
        in 
          case filtered of 
            [ next ] -> findLoopHelper field next (x, y) ((x, y) :: positions) 
            _ -> [(List.length conns, List.length filtered)]
    Nothing -> [(3, 3)]

findLoop startPos field = 
  case findConnections field startPos of 
    [a, b] -> 
      findLoopHelper field a startPos [startPos]
    _ -> [(1, 1)]

justLoop : List (Int, Int) -> Array2D Char -> Array2D Char -> Array2D Char
justLoop tiles field loopField = 
  case tiles of 
    [] -> loopField
    (x, y) :: rest -> 
      case Array2D.get y x field of 
        Just pipe -> 
          let 
            updated = loopField |> Array2D.set y x pipe 
          in 
            justLoop rest field updated
        Nothing -> loopField

findInsideHelper : Int -> Bool -> Maybe Char -> List Int -> List Char -> List Int 
findInsideHelper ix inside maybeWallChar found remaining = 
  case remaining of 
    [] -> found 
    h :: t -> 
      case h of 
        '|' -> 
          findInsideHelper (ix + 1) (not inside) Nothing found t 
        '-' -> 
          findInsideHelper (ix + 1) inside maybeWallChar found t 
        'F' -> 
          findInsideHelper (ix + 1) inside (Just h) found t 
        'L' -> 
          findInsideHelper (ix + 1) inside (Just h) found t 
        '7' -> 
          case maybeWallChar of
            Just 'L' -> findInsideHelper (ix + 1) (not inside) Nothing found t
            _ -> findInsideHelper (ix + 1) inside Nothing found t
        'J' -> 
          case maybeWallChar of
            Just 'F' -> findInsideHelper (ix + 1) (not inside) Nothing found t
            _ -> findInsideHelper (ix + 1) inside Nothing found t
        _ -> 
          if inside then 
            findInsideHelper (ix + 1) inside maybeWallChar (ix :: found) t
          else 
            findInsideHelper (ix + 1) inside maybeWallChar found t

findInside : String -> List Int
findInside s = 
  findInsideHelper 0 False Nothing [] (String.toList s)

toOffset : (Int, Int) -> (Int, Int) -> (Int, Int)
toOffset (xStart, yStart) (x, y) = 
  (x - xStart, y - yStart)

chooseReplacement : (Int, Int) -> (Int, Int) -> Char
chooseReplacement aOff bOff = 
  if aOff == ( 0, -1) && bOff == (-1,  0) then 'J' -- N, W
  else if aOff == ( 0, -1) && bOff == ( 0,  1) then '|' -- N, S
  else if aOff == ( 0, -1) && bOff == ( 1,  0) then 'L' -- N, E
  else if aOff == (-1,  0) && bOff == ( 0,  1) then '7' -- W, S
  else if aOff == (-1,  0) && bOff == ( 1,  0) then '-' -- W, E
  else if aOff == ( 0,  1) && bOff == ( 1,  0) then 'F' -- S, E
  else 'S'

findReplacement : (Int, Int) -> Array2D Char -> Char
findReplacement startPos field = 
  case findConnections field startPos of 
    [a, b] -> 
      let
        aOff = toOffset startPos a
        bOff = toOffset startPos b 
      in 
        chooseReplacement aOff bOff
    _ -> 'S'

replaceStart : (Int, Int) -> Array2D Char -> Array2D Char
replaceStart startPos field = 
  let
    pipe = findReplacement startPos field 
    (xStart, yStart) = startPos
  in 
    Array2D.set yStart xStart pipe field

init : () -> (Model, Cmd Msg)
init _ =
  let 
    sample = """...........
.S-------7.
.|F-----7|.
.||.....||.
.||.....||.
.|L-7.F-J|.
.|..|.|..|.
.L--J.L--J.
..........."""

    input = """-7-|-J-F7F--7.|77.77FFJ7F77..7FJ-77-FF7FF-777F-77--L7FFJJF77FF|7F7F--FJ7-7-77-7-7.F-7--F7.F7F---7F7FF7FL|7-7-F--F7-FF7-|.|77LL-FFL7J7F-F-F7.
|..FJ7F.LL|7L-J-LJ77F-J77.--F|FL7|LFLF|LL7||FJFJJ..L|FJ7.L777|L-JF7.LLJ7|L-JF7|-F|J7J7.||FJ|L--7L7---77..J7.7J7F-J-7J7J.L|-7||F7-L.LFLFJ7LJ7
FF7|FFJF|F77J-||LL7-7-L|-7JJJ|7FF-7..|L-FJL7L7|JFF77|L--7FJL-7J.F|L-7L-F77|F|L777|L7-F-JLJFJ.F7|FJJLJJLL|7|.L|JLJ|.JJ|77.|LL-|-JLLJ7J.||7FLJ
FJL7F-L-J7L-7.-JFJJ|JF|.|.|.-F7FL|.-.|..L-7|FJ|.JL77F|J-77||-F7FFJF-JF|||77|J.L|7--F-|F--7|F7||||F7|--7.-JF-7-.7J7F|.JJ--7FJF|.L.LLJ7FL---7|
L7FJ||.|L77-|7LF|J.7|-J7|.FF.LJ.F|FF7|...|||L7L77J||7J.FJF7F7|L7|FJ7F--JL7L7LFF|J.F-7LJF7|||LJLJLJ|7-FJFJ|LJJLJ7J|-J7|.F7JF|L7-.F.|-LJ|F7||7
|LL.|7FL-JL-LJF7JF7L7.LJJ--F.LF--F-J|F77FFJL-JFJF7|L---F7|||||FJ||F-JF-7FJF7-F-7FFJFJ-FJLJ|L-7F---JF-7JJ.F....LJ-L7J|77|.7|7|.L7LFJFJ.L-7JJ.
-.|FL|.J--7LL.LF7-L-L-||.LFL-FJJ.|.F||F-7L---7L7|L7-|FL|LJ|||||FJLJF-J7LJJ||JL7L7|FJJ.L7F7L7FJL--7L|FJ7F||FJ7|-7J-LFJFJ|FF|LF-|JL-|7J|J..LLF
L|LF7|FJF--7F7.LJ|7-7LLL|.|L|JF-77L-7F|FJF---JFJ|FJF77JL-7||LJ||F7FJFF-7F-JL7-L7||L--7FLJL7LJF--7L-JL7F-7JJ.LJ7LFJ|JF---7J|.F-7----JJJ-F7-J.
|.-FJL7LFJ.JFF-|JLJ-F-|-|FJFJFJ.FL7F-7|L7L---7L-JL-J|F7F7||L7FJ||LJF7L7|L7F-J7FJLJF7FJF7F7L-7L7LL----J|FJLLF|..-J-7FJ|.F-J|7|F|.||LF|L---.|7
F|F|F||F|F7.FJ||-7JJ|FFJ|F7|-L--LJFL7LJFJF7F-JF-7F--J|||LJL-JL7|L7L||FJ|L||LF-JF--JLJFJLJL-7|FJF77F7F-JL7-7FFL-|7|L-|-7FL-J|F7LF-L---7|7L.FF
FJ|7-L|-.||F7|L|.J|||LLFJ|FF..FJLF7FJF7L-JLJF7L7LJF7.||L-----7LJFJFJ||FJFJL7L-7|F7JJFL7F7F7LJL-JL-JLJF7FJJLJ|J-JJ77|F-FJJ7FL-L.||.F-J-FJ|F7.
LFLF.L|FJL-.L7L|7.|FL77LLJ.F-7F7-|||FJL---7FJL7L--J|FJL7FF-7JL-7L7|FJ||LL7FJF-JLJL7F7FJ|LJL----7F----J||77-FJJ.J-77J|J|7FJ-LFJF|-7L-7-||FJ|.
-J||7--7--7FJ|-L-FF7LLF.|FFL7|||FJ|LJF77F7LJF-JF--7|L-7L7|FJF-7L7||L-JL-7||LL-7F--J||L-JJF--7F7||F7F7JLJF77|LLF7F||7L7||7|-LL.-JFF-7J-|J--7.
LF-LJJ-JF|7FL777F-||7|LF7-F7|||LJFJF-JL7||-FJF7L-7LJF7|FJ||7|FJFJ||F----J|L7F7|L-7FJL7F-7L-7LJLJLJLJL---J|F7LFF7LL|J7.||77J|LFJL|J-J7.-7L|||
F7J|..|-J-FJF77-F-||-F7||FJLJLJF-J-L--7LJ|FJFJL--JF-JLJL7||FJL7L7LJL--7F7L7LJLJF-J|F-JL7|F-JF---7F-------J||--|F7.||L--J.|F7-F7.||FJL-|7F7J7
JL7-7F7.LJJFFF||JF||L|LJ|L--7F-JLF7|F7L-7|L7|F7F7LL-7F-7LJ||F7L7|F----J|L7|F--7L-7|L7F7||L--JF7J||F7LF-7F7||7LF7L|LJJ|..FJ--7L|-JLL-7.|LL|L-
.FJJ|-|F-7L7.JJ-7J|L7L-7|F7FJL--7||FJ|F-JL-JLJ||L-7JLJ-L-7|LJL7LJ|F-7F-JFJLJF7L7FJL7||||L7F--JL7|||L7|FJ||||7-LF-F7J7|-|77-L77F.F--FJLL7LL-L
FF7-||||.J7F-J7.LJL7L--JLJLJF7F-J||L7|L---7F-7LJF-JF7F7F7||JF7L-7LJFJL-7|F7FJ|-|L7FS||LJFJL--7FJ|||FJ||FJLJ|FF7F7.7J||.LF77.|||7F7.|7.FJ||L|
J||..F777LJLJ.|7||FL-----7F-J||F7||FJ|F7F7LJFJF-JF-JLJLJ||L7||F7|F-JF7FJ|||L7|FJFJL7LJF-J|F-7||FJ||L7|||F--JFJLJL7-F-77-|L7-|J|FF77J7.|7LJ.J
L77F.L-L-7F7|-|LF-FF7F-77LJF7|LJLJ|L7|||||F7L7L-7L-7F-7FJ|FJ|LJLJL--J|L7LJL7|||FJ7FJF7L7F7L7|||L7|L7LJLJL7F7|F---JJL7L7FJFJ-|F7FJ|J7F|L7.7-|
|.JL-J|L-.LJ--|7LF7||L7|F7FJLJF7F-JFJ||||||L7|F-JF7LJFJL-JL7L---7F---JFJF--J||||F-JFJL-J|L-J|||FJ|FJF----J|||L7F7|F|L7LJFJLF-J||FJ--LL-F77-J
|F|7J|LFJ7FJJJL-L|LJ|7||||L-7FJ||F7L7||||||FJ||F7||F7L--7F-JF7-FJL---7L7|F-7|||LJF-JJFF7L--7||||FJL7|7F7F7|||FJ|L77F7L7FJJ-L-7||||7||7.LF|7.
L-LF-LJL|LLJ|-|.LL-7L7||||F7LJFJ|||FJLJ||||L-JLJLJLJL7F7|L7FJL7L7F---JFJLJFJ|||F-JF-7FJ|F7FJLJLJ|F-JL-JLJLJLJL-JFJFJL-J|F7F--J||L7F7-F-.L|J.
F.||.L.L|7JFJF|7LF7L7||||||L-7L7|||L--7||LJF--7F-----J||L7|L-7L7||F7F7L--7L7|||L-7L7|L7|||L--7F-J|F7F7F7F7F7F7F-JJL7F-7LJ|L--7||FJ|L7J|.FJ-|
L77|F.LJL7FJ-F|7F|L-JLJ|||L-7L-J|||F--JLJF-JF7LJFF----JL-J|F7L7LJLJLJ|F--JFJ|LJF-JFJ|FJ||L7F-JL--J|||||LJLJLJLJF--7||FJF-JF7FJLJL-JFJFJ77J.F
.L-LJFJ-||JL-F--LL7F7F7LJL7FL--7|||L----7|F7|L7F7L--7F---7LJL7L-7F---JL--7L-JF7|F7L7|L7LJFJL--7F-7|LJ|L-7F-7F7L|F-JLJL7L7J|||F-----JJF..F7F|
|J-L-J-FFLFF.|J.|LLJLJL-7FJF7F-J|||F7F7FJLJ||FJ||F7LLJF7FJF7FJF-JL-7F7JF7L7F7||||L-JL7L-7L7F--J|JLJF7|F-J|FJ|L7||F7LF7L7L7|LJL-------7---7-F
F77L--FJFJ|L-J.F|J.F----JL7||L-7LJ||LJ|L7F-J|L7||||F7FJ|L-J||LL-7F-J||FJL7||LJLJL-7F-JF-JFJL--7L7F-J|LJF7|L7L7LJLJL7|L7|FJ|F---------J7L-LJ.
LLJ7F7LF-7L--F--F--JF7F7F7LJL7F|F-JL-7L7|L77L7||||||||FJF--JL7F7||JFJ|L7FJ|L-7-F7J|L-7L-7L-7LFJFJL7FJF7|||FJFJF7F-7|L7LJL7|L7F7F7F----7-|JL-
F|F-J77LFJ7-7JFLL---JLJLJL-7FJFJL7|F7L7|L7L7FJLJLJ||LJL7L--7FJ|LJL-JFJFJL-JF-JFJ|FJF-JF7L-7L7L7|F7|L7||||||J|FJLJFJ|J|F7FJ|FJ|||LJF---J7..-J
L--7F||L|F7L|.|F|LF--7F----JL7L-7|FJ|FJL7L7LJF----JL7F-JF--J|LL7F--7L7L7F--JF-JFJL7L--J|F7L7|F||||L7||LJLJL7||F7JL-JFJ|LJFJ|7|LJF-JF77L|7J|.
LLL|7|F.|JJ.FF.J--L-7|L---7F7L7J||L7||F7|FJF-JF--7F7|L-7|F-7|F7|L7JL-J-|L7F7L-7L7FJF---J|L7|L-J||L7||L--7F7LJ||L7FF7L7L--JFJFJF-JF7|L-7||.LL
-.-.|7LFJJ.F||F|.LFFJL---7LJ|FJFJL7||||LJL7|F7|F-J|LJF-JLJ-|||||FJF7F7FJFJ||F7L7||FJF-7|L7|L--7|L7|||F--J||F-JL7L7||L|F---JFL7|F7|||F-J7-F7.
|-|-L|777--|-LFJ-F7L----7L--JL7|F7LJLJL7F7|||||L7FJF-JF-7F-JLJLJL7||||L7L-J||L7|LJ|FJFJF7||F--JL7|LJ|L--7|LJF-7|FJ||FJL7F7F7L|LJLJLJL--7-J7J
|.|.LF7L7JFLF.|..|L----7L--7F7||||F-7F-J||||||L7||FJF7L7|L---7F--J||||F|F--JL7|L7FJ|FJ|||||L--7FJ|F-JFF7|L--JFJ||||LJF-J||||FJF----7F--J|.-7
|-F7-J7JL777F|LF7L7F7F7L---J|LJLJLJFJ|F7|||||L7||||FJL7|L7F-7|L7F7||||FJL7F7FJ|FJ|FJ|F7||||F7FJ|FJ|F-7|LJF---JFJ|FJF-JF-JLJLJFJF--7|L-7JF7FF
|LLJJ-LF-F77FF7|L7LJLJL----7|F-7F7FJJ||LJ||||F||||||F-JL7||7LJFJ||||||L-7||||.||FJ|FJ|||||LJ|L7|L7|L7||F7L-7F7L7||FJFFJF----7L-JF7||F7|-7|FJ
|.FJ7-FL7|L7FJLJFJ.F7F7F---J||FJ||L-7LJF-J||L7LJ||||L7F-J|L--7|FJ||LJ|F-J||||FJ|L7|L-J|||L77|FJL-J|FJ|||L--J||FJLJL-7L7|-F7|L-7FJLJLJLJJ||77
7-7F7.7LFL7|L--7|F7|||LJF7F7LJL-JL--JF7L-7|L-JF-J|||FJL7J|F-7|||FJ|F-JL7FJ|||L7L7||-F-J|L-JFJL--7FJ|FJ|L---7|||F----JFJL7|L--7|L-7LLLF-7-FF7
JJFL--FFF-JL---JLJLJ|L--JLJL-7F------J|F7|L7JFJF7|||L7FJFJL7|||||FJ|F7FJL7|||FJFJ|L7L-7L-7|L7F--JL7|L7|F--7||LJ|F7LF7L7FJ|F--JL-7L7J-L7L--J|
LF7|7-7JL7F--7F-7F-7L7F7F---7LJF-----7LJ|L7L7|FJ||||FJL7L7FJ|||||L7LJ|L7FJ|||L7|FJFJF-JF7|F-JL7F--JL7LJ|F7LJ|F-J||FJL-JL-JL7F-7FJFJ-F-JF7F-J
7-L|7L|L|LJFFJ|||L7L7LJLJF-7L--JF---7|F7|FJFJ|L7||||L7FJ-|L7LJ|||FJF-JFJL7|||FJ|L7L7L-7|||L-7FJL--7FJF-J||F-JL--JLJF7F7F7F7LJFJL-JF7|F-JLJ7|
||7|J.J-F7F7L7|FJFJLL--7FJLL7F-7|F--JLJ||L7L7L7|LJ||FJL-7|FJF-J||L7L-7L-7||LJL7L7L7L-7LJ|L7FJL-7F-JL7L7FJLJF-7F7F--JLJLJLJL7FJF7F7|LJL---7--
JJFJL7JFJLJ|FLJL-J|F7F7LJF--J|-LJL-7F77LJ7|FJFJL-7||L7F7||L7|F7||FJF-JF7|||F--JFJFJF-J7FJFJL7F-JL-7FJFJ|F-7L7LJ|L---------7LJ-|LJLJF7F7F-J|7
.||J||LL--7|F-7F--7|LJL-7|F--JF----J|L7F-7|L7L7F7|LJFJ|||L7|LJ|||L7L-7|||||L--7|7L7L--7L-J|FJL---7LJ7L-JL7L-JF7|F-7F7F7F--JF--JF-7FJ|||L7.L7
-LL-J.-LF7||L7LJF7|L---7|LJF7.L--7F7|FJ|FJL-J7LJ|L7JL7||L7||F7|||FJF-J|||||FF7|L7FJF--JF---JF7F7FJJF77F-7|7F7|||L7|||||L---JF-7L7LJ|LJL-J-.|
L|FJ7.LFJLJL7L--J||F7F-J|F7||F7F7LJLJ|7||7.F7F-7L7|F7||L7||LJ|LJLJFJF7||||L7|LJFJL7L-7LL7F7FJ||||F-JL-JFJL-JLJ||FJ|||||F7F-7L7|FJF7JJFJ.|FL7
.F7LF-FL-7F7L----JLJLJF7LJLJLJLJL----JFJL7FJLJFJJLJ|LJ|-|||F7L-7F7L7||||||FJL-7L7L|F-JF-J|||FJ|||L----7L------JLJ|LJLJLJLJ||FJ|L-J|JJ7.|F|FF
FLJ-|-F--J||F7F7F7F-7FJL--7F----------JF7LJF7FJF--7|F7|FJ||||F7LJ|FJ|LJ||LJ7F-JFJFJ|F-JF7|LJL7|||F7-F7L--------7F---------7|L7|F7FJ.LL.|||F|
L..--7L-7FJLJLJLJLJ.LJF--7LJF---------7|L7FJ|L-JF-JLJ||L7||||||F7||FJF-JL--7L7FJ7L7||F7||||F-J|LJ||FJL--------7LJF-7F-----JL-J|||L77F-7-LFFJ
JFJ|L||||L7LF---------JF7L--JF-------7|L7||||F-7L---7||FJ|LJ|||||||L7|F--7FJFJL-7-LJ||LJ|L7L-7L7FJLJF-7F-----7L--JFJL----7F-7JLJ|FJFL7|||LJJ
.|-|-F-FL-JJ|F---------J|F7F-JF7F----JL7||L7|||L7F--J||L7L7FJ|||||L7|||F-JL7L7F7L7F-J|FFJFJF-JFJL--7|FJ|7F7F7L---7L------J|FJF-7LJF77|L-|.F-
.LF|-||F---7LJF77F7FF---J|||F-J|L----7L|||FJ|L7FJL--7|L7L7||FJLJ||FJ||||F7FJJ||L7||F7|FJFJFL7FJF---J|L7|FJLJL----JF-------JL-JFJ7|||J7JLLL|J
F-F-.FFJF-7|.FJL-JL-JF---JLJL-7L----7L7LJLJJL7|L7F--J|FJFJ||L--7|||FJLJLJ||F7|L7||LJLJL7|F--JL7L--7FJ7LJL--7F-----JF-------7F7L7F-JL-7L7LLF.
L77-J|L-JFJL-JF7F7F--JF---7F-7L----7L7L-7F--7||.LJF77LJ7L7|L-7FJ|||L7F---J|||L-JLJF----J||F7F-JF7FJ|F-7F7F7LJF7F---JF7-F7F7LJ|FJL-7F-J||F7L7
LJ||LJ-|JL----JLJ||F7JL--7LJFJF----J|L--J|F-JLJF--JL----7LJ7FJL7||L-JL---7LJL--7F-JF7F7FJLJLJF7||L-JL7LJLJL7FJ|L-7F-JL-JLJL7|||F7-||JFFL|7||
L-F7FL7|LF-7F---7LJ||F---JF7L-JF---------J|F7F7|F-------JFF-JF-J|L7JLLF--JF7F--J|F-JLJ|L7F-77|LJL----JF7F7-LJ7|F7LJF---7F-7L7LJ|L-J|F77JLL.|
.L|L777-FJFJL--7|F7||L----JL---JF-7F-----7LJLJ||L--------7|F7||7|FJ.F-L-7FJ|L7F7LJF---JFJL7L-JF-7F7F-7||||F7F7LJ|F7L--7||7L-JF7|F--J|L7777.7
FF|FJF7FL7|F7F7||||||F7F-7F---7FL7LJF7F7|L7F-7LJF-7F-----J||LJ7-||J7FL|J||JL7LJL77L---7|F-JF-7|FJ|||FLJLJLJLJ|F7LJL7F-JLJF---JLJL7F7|FJF7--|
F-J|-||-FJLJ||LJLJ||||||FJL--7|F7L-7|LJL-7LJF|F-JFJL7F---7||JLL-LJ7FF.|FLJJJ|F--JFF7JFJ|L--JF||L-J||F-----7F7LJL--7|L7F7FJF-7F7F7LJLJ|||L7-J
L-7L-JL-JF-7LJF7F7LJLJLJL--7FJLJ|F7LJF--7L--7||F7L--J|F--JLJ777|LJ77|7LLL-FFJ|F-7FJL7L-JF----JL-7JLJL----7||L-----JL-J||L7|FLJLJ|F7F7L-JFJ7J
LFJF7F7F7|FJF7|LJL7F--7F--7|L--7|||F7L-7|F--J|LJ|F---JL-7F|.|JF|-|.LF-.|.LFL7LJFJ|F7L7F-JF7F-7F7|F7F-----J|L---7F-----JL-JL---7.LJ||L7F7L-7|
.L7|LJLJLJL7|LJ|F7LJF7LJF-JL-7FJLJ||L--JLJF-7|F7LJF7F7F-JJ-||-7|F----JF|-FF-JF7L7LJL7LJF-J||FJ|LJ|LJF7F--7L-7F7LJF7F-7F7F7F7F7L--7LJLLJL7FJ-
-7LJF----7-LJF7FJL--JL-7L7F-7LJF-7|L------JFJ||L7FJLJLJF7-F777|F|.L7FLF|--L-7||FJJ|-L--JLFJ|L7|F-JF7|||F-JF7LJ|F7||L7LJLJ||LJL---JJ7|FJ|LJF|
LL7|L-7F7L---JLJF7F-7F7L-J|7|F-J7|L---7F-7FJJLJFJL-----JL-JL-7LL|F||F.FJ7FJ-LJLJJLFF7F---JFJLLJL--JLJLJL--JL-7||LJL7L---7|L--7F--7JFJ7--L7--
.|L-JF|||F--7F7FJ||7LJL7F-JFJ|F--JF7F7LJFJL-7F7L7F7F-----7F--J.LLL-|J-F7LFJ-L..L7.FJ|L--7FJ.F----7F-7F--7F---JLJF--JF---JL7F7LJF-JFF-J7LLL-J
-L-7LFJ|LJF7LJ||FJ|F---J|F-JFJL---JLJL7-L--7LJ|-|||L--7F-JL--77LLL.7--J7-||7|7-||FL7|F7JLJF7L---7|L7|L-7|L---7F7L--7L7F7JFLJ|F-J7-LJ-JF7J|F.
||7LLL-J|FJL--J|L7|L7F--JL--JF----7F-7L---7|F7L7LJ|F7FJL7F--7|7F7|7L-77L-777J|F--LJ|LJL--7||F7F-JL-JL-7||F-7FJ|L--7L7LJL7F-7|L7J7.|L|.7J.LFJ
FJ77|LJJLL-7F7FJ7||FJ|F---7F-JF7F7LJ||F-7FJLJL7L-7||||-|||.LLJJFL--.FLJ-JJJ|JFL7L|.L--7F7LJLJLJF-7F--7LJLJ|LJFJF7FJ.L---J|FJL-JFJ-J--7L7-|J.
F.F7-7F7LF7LJLJF7LJL-JL--7LJF7|LJL--7|L7LJF--7L--JLJ|L-7LJ-7F|-L.-77.J-F|.LLF|LFF7F7F7LJL-7F---JLLJF7L-----7FJFJLJF-7F7F7||F---7|.|L|7LF-F-7
F7||7F||FJL---7||JF7-F7F7|F7|||F----JL-JF-JF7L-7F7F7L--J7|-LFJLJ-LFFJJ.F|..FJ|FF|LJLJL7F--J|F--7F--JL------J|FJF7.L7||||LJLJF-7L77|.F|7.||L-
LFJL7FJ|L-7F-7LJL-JL7||||LJ||||L---7F7F7L7FJL--J|LJ|F7F7F77|.7-|7F7|7|F7LF-7L77LL--7F7LJF7FJL-7|L------7F--7|L-JL7FJLJLJF--7L7L7|7J--.|77|L|
|L-7|L7|F7LJFJF----7||LJL-7LJLJF7F7LJLJL7||F7F--JF-J|LJLJL77.-7JL-L7-7JJ.-.F-|F-|LL||L7FJ||F--JL----7F7LJF-J|F---J|F----JF7L7L7LJ|7||F|LJ7L-
LF-JL-JLJL7LL-JF7F-J||F--7L-7F7|LJ|F7F-7LJLJ||F7FJLFJF-7F-JL||.7F7.|L|JLLJ7F-||L-J7LJFJ||LJL---7F--7LJL--JF7|L----JL--7F7|L7L-JFF7F7--77-|77
.L7F-7F7F7L----JLJF7LJL7FL-7LJ|L-7LJ|L7L7F7FJ||LJF7|FJJLJF|---7|7.J7-|7.L|FJ|LLJL-JJ.L7L7F-----J|F-JF7F7F7|||F-7F7F7F-J|||FJLF77|||||.|LJ|J-
F.||FJ|LJ|F7F--7F7||F-7|F-7L-7|F7L-7L7|JLJ|L7|L--JLJL-7FF-7J.LFL77F---J---J.7J7.|F77FLL-J|F-----JL-7|LJLJLJLJL7LJLJLJF-JLJL--JL-JLJL77J77LJJ
--LJL7|-FJ||L7FJ|LJ||.LJL7L-7||||F7L7|L-7-L-J|F----7F-J7L7L-7--7|-|.F7.L7F|-7||-FF-7-F7F7LJF-------J|F----7F-7L--7F7.|F----7F7F7F7F7L7-L7JFF
LFLLFJL7L7|L7|L-JF7LJF---JF7LJLJLJL-JL-7|F-7FJL---7||JJFFL7FJ7F|J.|-LF--J-7FJJJ-LL7L-JLJL--JF7F7F7F7||F--7LJ||F--J||FJL---7LJLJLJ||L-J7.|FL.
.JJ-|F-J-LJ.LJJF7|L7-L----JL-------7F77LJL7||F----JLJ-F-7FJ|.F77|.|7|FJFLLFL7L.FF7|F--------J||LJLJLJLJF7L--7LJF--JLJF7F-7L7F-77JLJLLL|FFFL7
JJ.FLJF7|F7F7F7|LJFJF7F7F7F7F----77LJL7F7FJLJL--7F-7F7L7LJFJFJ|F7.||F7F77F777FF7||LJ7F--7|F77LJF-7|F7F-JL--7L7F|F----JLJJL7||FJ-7L|||..L7-..
.7-|J.|L-JLJLJ|L-7L-JLJLJLJLJF--7L----J|||F--7F7LJFJ||J|F-J|L7LJ|7-F|LJL7|L7-FF7||F-7|F7|FJ|F7.L7|FJLJF7F7LL7L-JL--7F7F--7|LJL77|FFJ-JFF|L77
FLFJ|-L------7L--JF7F7F--7F7FJF-JF-----JLJL-7LJ|F7L7|L7|L--7J|F-J77LL--7LJFJ-F|LJ|L7|LJ||L7|||F7||L--7|LJL7-L-7F---J||L7FJL--7L7F-7J.|FJJ7.7
||L-J7LJ-F---JF7F7|||LJF-J||||L--JF-----7F-7L-7LJL-J|FJ|F--JFJL--7F7FF7|F7L7FFL-7L7|L7L||FJ||LJLJL7JFJ|F--JF7-|L----JL-JL7|F7L7LJFJLF-F-7--J
F|L|-LJ.FJF7F-J||||LJF-JF7||L7F--7L----7|L7|F7L--7F7|L7|L7|FJF---J||FJLJ|L-JF7F7L7LJFJFJ|L7|L7F--7|FJFJL---JL7L-7F7F--7F7L-JL7|F7L--77F7J|.7
7.FF7LLF|FJLJF-J|LJF7L7FJ||L-JL-7L---7FJ|FJLJ|F--J|LJFJL7|FJFJ-F-7||L--7L7F7|LJL7L-7L7L7L7||FJ|F-J|L-J|F7F7F7L-7|||L7FJ|L7F7FJ|||F--JJJ|LF-7
L-LJL-|FLJF7FJF7|F-JL-JL7||F-7F-JF-7JLJF|L--7|L-7-|F-J|FJ|L7|F7|FJ||F7FJFJ||L--7L7J|FJ-L7LJ|L-JL-7L-77FJLJLJL7FJ|||FJL7L7||LJ7LJ|L-7F|LJ7F|-
F-F-.-F-7FJLJFJLJ|F7F---JLJL7LJF-JFL---7|F--JL--JFJL--7L7L-JLJLJ|FJ|||L7|FJ|F7-L7|FJ|F7LL-7|F7|F7|F-JFJF--7F7LJJLJLJ-FJFJ|L---7|L--J-|.L7LLJ
--7J-LL7LJF7FJF-7||LJF7F7F-7L--JF-----7||L-7F7F7FJF---JFJF------JL7LJ|FJ||FJ||F-J|L7||L---JLJL-J||L7.L7|F-J|L-------7L-JFL----JF7F7F7L|||LLJ
L|FJFFLL--JLJFL7|LJF7||||L7L--7LL----7||L--J|LJ|L7L--7FJFJF7F-7FF7L-7|L7||L7||L-7|7||L---------7LJFJF7LJL--JF-------JF---7F7-F7|||LJ|-LJ7--F
L-J-77.||J7LF--JL--JLJLJL7L--7|F7F---JLJF7F7L7FJ.|F--J|FJFJ||FJFJ|F7|L-J||FJ||F-J|FJ|F--7F-----JF7L-JL-7F---JLF-----7|F--J|L-J||LJF-J-7LF-7J
FJ--|.-FJ.-JL7F-----7F7F7L---J|||L---7F7||||FJL7FJL7F7||FJFJ|L7|FJ||L--7||L-J||F7LJFJ|F-JL-7F7F7||F-7F7|L--7F7|F---7||L-7FJF--J|F-JJ.L7.|--7
|FJ|.FJL--7F|LJ.F---J||||F---7LJL----J|||||||F-JL-7||LJ|L7L7|FJ|L7||LF7|||F--JLJL-7|FJL7F7F||LJLJ||-|||L--7LJLJL--7LJ|F-J|FJF7L||F7-F|7-J.|7
L|-F||7.F7F-77F7L--7FJLJ||F--JF-------JLJLJLJL-7F-JLJF-JFJFJ|L7|FJ||FJ||LJL-7F7F--J|L7FJ|L-J|F---JL7LJL7F7L-------JF7||F-JL-JL-JLJL77L77L|L7
L|FFLF77|FL7L-JL---J|F--J|L-7FJF-------7F7F7F7FJL---7|F7|FJ-L7LJ|FJ|L7|L7F--J|||F-7L-JL7L7F-JL7F7F7|F-7LJL-7F7JF7F7|||||F7F7F7F---7L77LL-77|
-JFL||L7F7.L---7F7F7|L---JF7LJ7L---7FF7LJLJ||LJF7F7FJ||LJL7F7|F-JL7L7||FJ||F7||||FJF---J7LJF--J|LJLJL7L---7LJL-JLJLJLJ|||LJLJ|L--7L7L77J.|-F
|.-FFL7LJL----7LJLJ||F----JL-------JFJL--7FJ|F-JLJ|L7LJF7FJ|||L7JFJFJ||L7|FJLJ|||L7|F--7F7.|F-7|F----JF7F7L-7F7F------J|L---7|F7FJ-L7L7----7
7..7LLL---7F-7L7JF7LJL--7F7F7F7F---7|F---JL7|L---7L7|F-JLJ||||FJFJFJ||L7|LJF--JLJFJLJF-J|L7LJFJ|L7F7F7|LJL-7|||L7F7F7LFJF7F7|LJ|L7F7L-JFF7.|
L7.J-|-JJ7||-L7L-JL-7F7FLJLJ||||F-7LJL----7LJF-7JL7||L7F-7FJLJ|.|FJF7L7||F-JF-77FJF7FJF7|FJF7|FJ7LJ||LJF7F7|LJL7LJLJL-JFJLJLJF7L7LJL-7F-7JF-
FFF7.-7JLFLJF7L----7|||F7F7LLJLJL7|F-7F---JF7L7|F-J|L7||FJL--7L7|L7|L7||||F7L7|FJFJLJFJLJ|FJ|LJF7F7LJF-JLJ|L-7FJF-7F7F7L-7FF-JL7L----JL7L-77
|7.-7.|.FL.FJL7F---JLJLJLJL7F7F7FJLJFJL---7|L7||L-7L-JLJL7F7.|FJ|FJL7LJ||LJ|FJ||FJF7.L-7FJL7L77|||L7.L---7|F-J|FJFJ|LJ|F7L7|F--JF--77F-JF-J-
.L7|.L-7.|FJF7LJF-7F--7F7F-J|LJ|L--7L-----J|FJ||F7L--7F--J||FJL7||F7L-7||F-JL7|||||L7JFJ|F7|FJFJ||FJF7F7FJLJF7|L7L7L7FJ|L-J||LF7L7FJFL-7||.L
.|||..FF--L-JL-7|FJ|F7LJ||F7L-7|F-7L7F7F-7-|L-J|||F--JL7F-J|L-7LJLJL7J||||LF7|LJL7L7|FJFJ|||L7L7||L7|LJ|L7FFJ|L-JFJFJL7L--7|L-JL-J|F-7FJL777
-JLF.J-|FF7JF-7LJL-J|L--JLJ|F7||L7L-J|||FJFJF--J||L--7FJL-7||FJF7F--JFJLJL7|||F--JL||L7|FJLJFJL|||FJL-7|FL7L7L--7L-JF7L7F-J|F-----J|FJ|F-JL|
|7JFF7F--JL7L7|LF--7L----7FJ||||FL-7FJ||L7|FJLF7||F7FJ|F7FJ|FJFJLJF-7L-7F-J||||F-7FJ|FJ||F7FJ|FJ||L7F-J|F-JFJF--JF7FJL7|L--JL-7-F7FJL7||F7F|
-L7.L-L---7L-JL7L-7|F----JL-JLJL---J|FJL7|||F7|||||||FJ||L7||FJF7-|FJF7||F7|||||FJL7|L7|||LJF7L7|L7|L7FJL--JFJF7F|LJF7|L7F-7F-JFJLJF7LJLJ|77
|.L|.FLJ.L|F7F7L-7||L-7F--7F7F7F---7|L7FJ||LJ|||||||||FJ|FJ||L7|L7||FJLJ||||LJ||L7FJL7|||L7FJ|FJL7||FJ|F7F-7L-JL-JF-J||FJ|FJL--JF--J|F7F7L-7
FJJ|-|L|-7LJLJL-7||L-7LJF7LJLJLJF--JL7||L||F-J|LJ||||LJFJL7LJFJ|.LJ|L7F-J||L7J|L7LJF-J||L7||FJ|F7||LJFJ|||FJF--7F-JF-J|L7|L-7F-7|F-7|||||F7|
L|FL-|-|JFJF----JLJF7L-7||F-77F7L7F-7||L7||L-7L-7||||F-JF7|F7L-JF--JFJ|F7|L7L7L-JF-JF7|L7|LJL-J|LJL7LL7|LJL7L7FJL-7L-7L7||F-JL7|LJFJ||||||LJ
LF-J7|L||.|L-----7FJ|F7|||L7L-JL-J|FJ||FJ|L-7L7FJLJ|||F7||LJL--7L7F7|||||L7L7L--7L-7|||.|L-7F--JF7FJF-JL--7L7|L7F-JF7L-J||L-7FJ|F7|-LJ||||||
.7JF-7-J-FF7F7F--J|FJ||LJL-JF---7FJL-J|L7L7FJFJL-7.|||||||F7F-7L7|||L7|||FJFJF7FJF-J|||FJF-J|F-7|||7L-7F-7L7||FJL--JL--7|L-7|L7LJ||FJ-LJLJ-7
F7.FF-FJLFJLJLJF7FJL-JL-7F-7|F--JL7F--JFJ.|L7L7F7L7LJLJLJ||LJFJFJLJ|FJ||LJF|FJ|L7L7FJ||L7L7FJ|FJ||L-7FJL7|FJLJL7F7F---7||F7|L7L-7LJ----||LL|
LJF-LJJ.FL--7F-JLJF7F-7FJ|FJ||F7F7|L--7L-7|FJFJ|L7L--7FF-JL-7L-JF--J|FJL7F-J|||FJFJ|FJL7L7|L7|L7||F7|L7FJLJ.F7FJ||L-7FJ|||||FJF-J.|.|.F-|7.L
.L-7|-FF-F--J|F--7||L7LJFJL7|LJ|||L7F-JF-JLJ|L7L7|F-7L7|F7F7L-77L7F7|L7FJ|F7L7||FJFJL--JJ||7LJFLJ||||FJL----J||FJ|F7||FJ||LJL7L7J-7.L|JF|77|
7.L-JF.L-L7F7||F7LJL7L-7L-7||F-J||FJ|F7L-----7L7|||L|FJLJ|||F7L77LJ|L7||FJ||FJ|||FJF7F---JL--7F--J|LJ|F7F7F-7|||-LJ||LJF|L77F|FJ|LF7|L.|7FL-
LJ7FFJ7J|.||LJLJL--7|FFJF-JLJL-7||L7LJL7F--7FJ7LJ|L7LJF--J||||FJF77L-JLJL7|||-||||.|||F--7F7FJL--7L7FJ|||||FJ||L-7FJL--7L7L7FJL--7||J.L|LFFJ
L|FFF.L7L-LJF------JL-JFJJF7F7FJ|L7L7FFJL77|L7F--JFJF-JF-7||||L-JL---7F--J||L7||||FJ||L7FJ||L-7F7|FJ|FJ||LJL7||F7|L-7F7|7L7|L7F--J||77F|7JLJ
.F|FJ-LLJL|.L---7F7F-7FJF-JLJLJFJ||FJFJF7L7L-JL--7L7|F7|F|||||F7F--7FJL--7||FJLJLJL7LJFJL7|L-7||LJL7||FJ|F7FJ|LJ|L-7|||L-7|L7||7F7||F7-F7L|J
F7J||-|F-JL7FLF7||||FJ|-L7F-7F7L-7||LL-JL7L---7F-JFJLJ||FJ|LJ|||L-7|L7F--J||L7F7JF7L-7L-7LJF7|||F-7|||L7LJ||FJF-JF-J||L7FJ|FJ|L-JLJLJL-JL7LF
|LFL7F-7-7LLF-JLJ|LJL7|F-JL7LJL7FJLJF----JF7F-J|F7L-7FJ|L7L7.|||F-JL7||F7FJL7LJ|FJL--JF7L7FJLJ|LJFJ|LJ-L7FJLJ7|F7L-7||.|L7||FJF7F-7F----7|.|
J-JLF7F-.-7-L7F7FJJF7LJL-7FJ-F-JL-7.L-7F7FJ|L-7LJL7FJL7|FJFJFJ||L-7-||LJ|L-7|F7|L--7F-JL7|L--7L7-L7|7F7FJL--7FJ||F-JLJFJFJ|||FJ|L7LJJ.L|LJ7-
|L-7J.|.F|J|LLJLJFFJ|F---J|F7|F7F7|F--J||L7L--J-F-JL-7||L7||L7||F7|FJL-7L-7|||LJF--JL--7||F--JFJF-JL-J|L7F7FJL7||L--7L|FJFJ||||L7L7.LLL7JF||
|..|||-.||-7J-F--7L7|L-7F7LJ|||||||L--7|L7L----7|F7F7|||FJ|F-J|||||L7F7|F-J||L7JL-7F7F7|LJL--7|LL7F7F7L7||||F7|||F7FJFJL7L-JLJ-JL-J7F7F|||77
J.7.LFJ7LLF-7-L-7|L|L--J||F-JLJ||||F--JL7|F----JLJ||LJLJL7|L7FJLJLJFJ||||F7|L-JF--J||||L7-F--JL7FJ||||FJLJ|LJ|LJ||LJ-|F-J|LJLJ.L|L.|LJ-L7|L|
FFF-.L.FJ-LJF---JL7L7F7FJ|L---7||||L-7F7||L-7F7F--JL---7FJ|FJL--7F7L-J|||||L7FFL--7|LJ|FJFJF-7FJ|FJLJ|L-7FJF-JF-JL-7LLJ7FF.L|LF-|-7|F||7LJF|
---.7.F|JL|FL----7L-J|||7L7F7FJ|||L7FJ||||F7LJ|L--7F-7FJL7|L7F-7LJL7F-J|||L7L7F---JL-7|L7L7|FJL7|L7F-JF7|L7L7LL-7F7L7JJF777FJ-J|L7L77F7-7LLL
||.FL.-|-77|F----JF-7||L7-LJ||-LJL7||FJ|||||F7|F--JL7|L7LLJ-LJ-L7F-J|F7|||FJFJ|F7F7F-JL7L7||L7FJ|FJL-7||L7|FJ77FJ|L7L7-FF7FLJ7J|JJ..F-7.J|JJ
.7F7J7J.|JF-L--7F7|FJ||FJF--JL-7F-J|LJFJ|||||||L7F7FJ|FJJ.LF----JL7FJ|||LJL7|FJ||||L--7L7|LJ7||L|L7-FJ|L7|||L|F|FJJL-J..J77--F.7LF-J|LJ-LL--
FJF|-|FFFJLF---J|||L7|||FJF-7F-J|F7L7F|FJ||||LJFJ||L7LJJ7F|L--7F7FJL7||L7JLLJ|FJLJL7F-JFLJ-F-JL7|FJFJFJFJ|LJ-F-J|J7|-J-|.|-.LF-LF7JFJ..F||..
J.LF.LL-|.LL-7F7|LJL|||||FJFJ|F-J||FJFJL7LJLJF-JFJ|FJJ|7F--F--J||L-7LJL7|JJL-||F7F-JL--7LJ.L-7FJ|L7L7L7L7L-7-L7FJ.|-.L-|F|.|J|L|||FJ777-LL77
LF7L||-FFF.LLLJ||FF7||||LJJL7|L7FJ|L7L7FJJL|JL7FJFJL7.77L.FL-7FJ|F7|LJFLJ7..FLJ|LJF7F--J-7-F-J|L|FJFJFJFJF7|JJLJF7..F----L-7FL7-|7L|LF-.LLJ7
.F|L|7-L-7J7J..LJ-|LJ|||7.LFJL7LJFJFJFJL77F|7LLJLL7FJ-L7FLJ.L||J||LJLF|7||7F-.L|F7||L-7JJF-L--J.LJLL-JJL-JLJ.F-L7LJ.|.|.||LJ7JJFL7F||JJFLLF|
F|7.J|7...|F7F-JLF|F7|LJ-J7L-7|J7|FJFJF-J7JFJ|L|JLLJF|.L-|LJ|||FJL7|-L-|-LFL-7-LJLJ|F-JJ.J-JLJ-FJ.L|-L-7L.J--7|J.|.L7-7-LLJF|7F77.-L|-FF-F7J
.||FL-J-77L-L|7.LF||||||7L7|-LJJ-|L7L7|J---JFL--.J||---..-|7LLJ|F7L-7JJ|7|||FFF7L|L|L7F--L7JLJ.F|7|LF7F-7L7F7LJ7F--|J.L.L|-LJ-J777LFL--JF-7.
7.L7J7FFJFLL7L-FLFJ||L7-F7FJ7-|L|L7|-LJJ7LJL|JJ.|J|JF.LF.|L7LJLLJL--JL7J77.LJ||F-7JL7|J.7J|.|.F|FF-FJJ|F|.JLJ|.F-FF|.7.-|L-JF--JFJFJJ|J-FJL-
J.|7-F7.LLJL|.77LL-JL-J7F-JLL7|.F7LJ7FL-J.FL|.|-.7.F|F-7.-7JJ|FL||-||.|7|-7-JJ|LLLJ|LJ7FJ.JFFL7JL--J-F77J-F77F-|-F.77.L.LF77FJ|LF-7.FJL-LJ|.
|.7L-|JL.LF777F|.|J|.JJ-|.-.|L.FFJJ||-L-|7L7JF7.7JFJLF.|J|||LF--JJ7.7.J-L||.|FL7||-L7L--7FLL-7|77F7J-|.J7.L7JLFL.LF7FF|.F7F|.FFJ|FJF||.7J7.J
|7|.FJ.J-.L|JF-JFJ.|.|LFJ-|FJFF7JJ.||LL7--J|FLJ7.LF-J|7L7L7J.LJF|7L-7-|JJ.FF-FL7-L7L|JLFL7.|F7JF-J-7FL7L7-|J77|LFF-|--J-F77JFL|.|J7.L7-JLJ7|
-F7-||7JFFL|L|.|F-7L7J7JFFF---L7JJ.7-.FFJ7F.|LFJ--J.F7JFJJ.F-7--77.F|J|FF7LJF--7J|L7.|.FJLF-7LFJ|-|F-77.LF----7LLJL||.LLJLJFF7J7|-77.-.LJF-J
LJ--|J7.LJFJ7|7FJ---L77LLF7J7.FJ7F7L77.J.FLF-FJJ.FL.F--FJ.F-FF7J|7FFJFF|-7|JLL--7J-J7|7LF7LLF---7F7--F7F-JL-.|7LF|7F|--.FJ.FJ|7F|-L77|L7FL-J
|JL-JJ-|.FJ-LL7LF7-F-JL--LJL|---|JLJJJ.|-JLLJ|J.FJ.FLLJJLJ-J|L-.LF-J-LJLLJ-7.L--L..LLJ-.LJJFJ---JL7J.LFJLF-J7.|-JJLJ.|J-JJ|L7J|.|.LL-7.|J.-J"""

    lines = input |> String.split "\n"

    field = lines |> List.map (String.toList) |> Array2D.fromList

    startPos = findStartPos field |> Maybe.withDefault (0, 0)
    loop = findLoop startPos field

    rowCount = Array2D.rows field  
    colCount = Array2D.columns field 
    emptyField = Array2D.repeat rowCount colCount ' '
    fieldWithoutS = replaceStart startPos field 
    loopField = justLoop loop fieldWithoutS emptyField

    plainLines = 
      List.range 0 rowCount 
      |> List.filterMap (\rowNo -> Array2D.getRow rowNo loopField)
      |> List.map (Array.toList)
      |> List.map (String.fromList)

    replacement = findReplacement startPos field 
    linesWithoutS = 
      lines 
      |> List.map (String.replace "S" (String.fromList [replacement]))

    insideTiles = 
      plainLines 
      |> List.indexedMap (\y line -> findInside line |> List.map (\x -> (x, y)))
      |> List.concat

    -- debugText = "insideTiles: " ++ (insideTiles |> List.length |> String.fromInt)
    debugText = ""

    model = { lines = linesWithoutS 
            , field = emptyField
            , startPos = startPos
            , loop = loop
            , steps = (loop |> List.length) // 2
            , insideTiles = insideTiles
            , insideCount = insideTiles |> List.length
            , debug = debugText }
  in 
    (model, Cmd.none)

-- UPDATE

type Msg = Tick 

updateModel : Model -> Model
updateModel model = model

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Tick ->
      (updateModel model, Cmd.none)

-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model = Sub.none

-- VIEW

toCharText : Int -> Int -> Char -> Html Msg 
toCharText yPos xPos ch = 
  let 
    xOffset = 2
    yOffset = 8
    xVal = xOffset + xPos * unitSize
    yVal = yOffset + yPos * unitSize
  in
    text_
      [ x (String.fromInt xVal)
      , y (String.fromInt yVal)
      , fontSize "9px"
      , fontFamily "monospace" ]
      [ text (String.fromChar ch) ]

lineToCharTexts : Int -> String -> List (Html Msg)
lineToCharTexts y line =  
    line |> String.toList |> List.indexedMap (toCharText y)

toColoredBox : String -> (Int, Int) -> Html Msg 
toColoredBox fillColor (xStart, yStart) = 
  let 
    xVal = unitSize * xStart
    yVal = unitSize * yStart
  in
    rect
      [ x (String.fromInt xVal)
      , y (String.fromInt yVal)
      , width (String.fromInt unitSize) 
      , height (String.fromInt unitSize)
      , opacity "0.8"
      , fill fillColor ]
      []

toOutlineBox : (Int, Int) -> Html Msg 
toOutlineBox (xStart, yStart) = 
  let 
    xVal = unitSize * xStart
    yVal = unitSize * yStart
  in
    rect
      [ x (String.fromInt xVal)
      , y (String.fromInt yVal)
      , width (String.fromInt unitSize) 
      , height (String.fromInt unitSize)
      , stroke "red"
      , strokeWidth "1"
      , fill "None"
      , opacity "1" ]
      []

toSvg : Model -> Html Msg 
toSvg model = 
  let 
    lines = model.lines
    loop = model.loop
    loopBoxes = loop |> List.map (toColoredBox "palegreen")
    insideBoxes = model.insideTiles |> List.map (toColoredBox "deepskyblue")
    charTexts = lines |> List.indexedMap lineToCharTexts |> List.concat
    startBox = model.startPos |> toOutlineBox
    numberOfLines = lines |> List.length
    numberOfChars = lines |> List.head |> Maybe.map (String.length) |> Maybe.withDefault 0
    svgWidth = (unitSize * numberOfChars) |> String.fromInt
    svgHeight = (unitSize * numberOfLines) |> String.fromInt
    elements = insideBoxes ++ loopBoxes ++ charTexts ++ [startBox]
  in 
    svg
      [ viewBox ("0 0 " ++ svgWidth ++ svgHeight)
      , width svgWidth
      , height svgHeight ]
      -- , Svg.Attributes.style "background-color:lightblue" ]
      elements

view : Model -> Document Msg
view model = 
  { title = "Advent of Code 2023 | Day 10: Pipe Maze"
  , body = [ viewBody model ] }

viewBody : Model -> Html Msg
viewBody model =
  let
    s = toSvg model
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
              [ Html.div [] [Html.text "Advent of Code 2023" ]
              , Html.div [] [Html.text "Day 10: Pipe Maze" ]] ]
      , Html.tr 
          []
          [ Html.td 
              [ Html.Attributes.align "center"
              , Html.Attributes.style "background-color" "white" 
              , Html.Attributes.style "font-family" "Courier New"
              , Html.Attributes.style "font-size" "20px"
              , Html.Attributes.style "padding" "20px"] 
              [ Html.div [ Html.Attributes.align "center" ] [ s ] 
              , Html.div [] [ Html.text <| String.fromInt model.steps ]
              , Html.div [] [ Html.text <| String.fromInt model.insideCount ]
              , Html.div [] [ Html.text model.debug ]
              ] ] ]
