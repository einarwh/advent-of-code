module Aoc15 exposing (..)

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

type DataSource = Input | SampleSmaller | SampleLarger | SampleSituation

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

sampleSmaller : String
sampleSmaller = """########
#..O.O.#
##@.O..#
#...O..#
#.#.O..#
#...O..#
#......#
########

<^^>>>vv<v>>v<<"""

sampleLarger : String 
sampleLarger = """##########
#..O..O.O#
#......O.#
#.OO..O.O#
#..O@..O.#
#O#..O...#
#O..O..O.#
#.OO.O.OO#
#....O...#
##########

<vv>^<v^>v>^vv^v>v<>v^v<v<^vv<<<^><<><>>v<vvv<>^v^>^<<<><<v<<<v^vv^v>^
vvv<<^>^v^^><<>>><>^<<><^vv^^<>vvv<>><^^v>^>vv<>v<<<<v<^v>^<^^>>>^<v<v
><>vv>v^v^<>><>>>><^^>vv>v<^^^>>v^v^<^^>v^^>v^<^v>v<>>v^v^<v>v^^<^^vv<
<<v<^>>^^^^>>>v^<>vvv^><v<<<>^^^vv^<vvv>^>v<^^^^v<>^>vvvv><>>v^<<^^^^^
^><^><>>><>^^<<^^v>>><^<v>^<vv>>v>>>^v><>^v><<<<v>>v<v<v>vvv>^<><<>^><
^>><>^v<><^vvv<^^<><v<<<<<><^v<<<><<<^^<v<^^^><^>>^<v^><<<^>>^v<v^v<v^
>^>>^v>vv>^<<^v<>><<><<v<<v><>v<^vv<<<>^^v^>^^>>><<^v>>v^v><^^>>^<>vv^
<><^^>^^^<><vvvvv^v<v<<>^v<v>v<<^><<><<><<<^^<<<^<<>><<><^^^>^^<>^>v<>
^^>vv<^v^v<vv>^<><v<^v>^^^>>>^^vvv^>vvv<>>>^<^>>>>>^<<^v>^vvv<>^<><<v>
v^^>>><<^^<>>^v^<v^vv<>v^<<>^<^v^v><^<<<><<^<v><v<>vv>>v><v^<vv<>v^<<^"""

sampleSituation : String 
sampleSituation = """#######
#...#.#
#.....#
#..OO@#
#..O..#
#.....#
#######

<vv<<^^<<^^"""

input : String
input = """##################################################
#.#.#......O....##.#O.##..O.........O........O.#.#
#......O..........O..O.O.....O.........#O.......O#
#.OO....#.O.O.OO#....#.OO.....O.##....O.......O.O#
#..........O...#O#.......#OO....O.O..O#........OO#
#O..O.....O..O..#.O.O...O..#.O.O.....O.#....OOO..#
#O#O..O..O.OO.#..O...........#.......#O.OO.O.O..##
#.O..OO.O.#.#.......O......O..........O.O..#.....#
#...O..O..O...OO....O....O....#.OO.#..O.....O..###
###.#.O..O.O.O..O.....O.O.#.....O.O......OO....#O#
#....O.....OO.O#..#..O.O...OO..................#O#
#....OO...O.O.OO.O#.O...O..#..O.#.#....O.O.O.OO.##
##.##....O#O.......O#..O.OOO....O.....O....O#..O.#
#...O..O....#O...O..O.OO..OOO.##O...........O.#O.#
#..O....O.#O...OO....OO.O..O.#OO..O....O.....O...#
#O#O..O.......OO...#.....O#OOO....#.O...O.......O#
#O.O.....OO#.#.O..........#..O#.....O..O#...O..###
#...#.....O#..O....##OO.O.#.#......O......OOO..O.#
#OO.O....O.OO..O.........OO.O.....#.O.O...O..O#O.#
#..O#..O#.OO.OOOO..O..O.......#O#..OO...OO.O....O#
#O.#O#....#..O.#..O#O.OO..O.O.#....#.OOO...O.....#
#.......#..OOOOO...O..OOO..O...O.OO....OO.OOOO...#
#...O..#..OO..OOO.O.#O.#..#.O..O...O#.....O.....O#
#.OOOO.....OOOOO...OO........O#....O.....O.#...#.#
#O#OO.#O..........O..OOO@.O.....O.....#O...O..OOO#
#.#............OO....O......O#O..#O..O.O.........#
#.O.O.....O....O.O.OO.O..O...#........OO.#O....O.#
#......OOOO......O#.#.O...#O.O.OO..O.O...O.O...OO#
#OO#.#O..O...#.O..O.....O...OO.O.....##.O.....O..#
#..#O.#OO.OOO..OOO.#..OO..#.O...OOO......#......##
#O.O#.O..O.......O..OO....#....O..O..O....O...#..#
#..OO.O...............##O..O..OO....O.OO...O...OO#
#.#O........O...O.O....#O#........O.....#..#O....#
#......#....O#......#.#.#...#........O..OOO..O..O#
#O.#OO.##..........O.#O.O.#O.#OO#....O..OO...OO..#
#...OO.......#...##..#......O.O.OO.OOOO.O.#.#....#
##.O..#..O#......OO.....#....O..#..O..O.#.OO..#..#
#....O.OOO.O..O#........O.O...OO...O...O.O....O..#
#OOOO.OO...O.O..#O.OO.O#......#OO..O.O.....O..O..#
#..#.O..O..#O......O...OO.......#O...#..#O#.O..O.#
#O...#..OOO.O#O.O.O.O.O..O...O.O#..O..O..O.#..OO.#
#.....O.O.O..O.O..OO.........#O.....O...O......O.#
#O...OOO....O.OO..#....O#O...O.O..O.O.......OOO.O#
#OO#..O##..O...O.#..O....##..OOO...O.O##.##..#.OO#
#OO.......O##.#..O.OO......O..O..O..O....O.O...O.#
#.....O...O.O.#..O..O.#..O.OOO#.......#.........##
#..O.O.OO....#...OOO.OO..O....O.#.O.O##....OOO...#
#OO#O..#.....O.....O.....O...O#O..O..O#......O#..#
#O....#.OOO.O#.OO..O...OOO.OOOO...OO#O.O.O.....OO#
##################################################

<><>^<^^vv>^^v^vvvv<>>^<>v>v>^<^v<><<v^^v^<><<>>>><>vv^^v<^><^<v<><v<^^<<^<^vv^^vv^v^v^<^v^>^vv^<v^>v^><>vvvv^v><>^v>>><^vvv<>v<>^^^vvv<>v>>^v>>^<v<^>^>>>>^<^^^>^>v><<^v><<>>v><v>^>vv<v>^^<^<v>^<v^^>^>^>^>vv<^^<v>>>><^v><vvv^><^<>v<v^vv<<>v<><v<><^vv^>>^v><v>v^<^>>vvv^<>vv<<<<<v^>^<^v<><^v^<>>v<<^<>v^>^>>^^>v>v>^<^^<>^v^<<v<>>v<^>^^v>^v^<<v>^<^v><^vvv>^^^<^<<vvv>>vv<><<<>v>>><v>v^v<v^><<^><vv^>v^v>>vv^><>v^v^<<>vv>><>v<<v>^>>vv<>v^v>^^v<^<>^^<^>^^^<^^<<v<^>vvv><>>^^>>>^>^vvvvv<v^^vv<^^><^v^<><<<>>>v<><v>v><vv><<v^>v>v>v<>vv^>>><v^>v><<<v>v^>^^v<<>v<v<>>v<>v<>>^^<v^<>^>>><<>v^>^vv<^<^^v^^^<vv^v^^^<>v>v>^^^^^>v>v^v><^v>^<<^<<v<>>>>^>><>>^v><^<<>><v^^v<^<>^^<<v>^>^vv<v><<v<v<>>^^><<^^v^>>^<<^^^^<v^^^>v>><<v^^v^>^v^v<^>>vv^^<<^v>><v^^<v<>v^^^<><<v<<^^v>>><vvv^<<<<^<>v>vv<<><^v^>>v^^v^^v<^<^<v>v^<vv>^<<<><><<^<^vv<^<<^^^^v^^>>><<^<^><<>v<>^v<v^>vvv^v^v>>><v>v<<>v>><v><<>><v>>v^>>>^^<^>^^^v<<><<^<<^<>v>^^^v>><>v>^>v<>^><vv^^<<>^v><<<>^^vv>>v^><<^v^<^>>^^v^^vv^v>v^>^^v>v<^<<v><<^>>^^><<<^^v<v
>^^v<v<^^^^v^^>v^v^^^^v<v^vvv<><^^<<<>>>v^v<>^<>^<vv<>>^^v>^v<^^v>v<>vv<^v>>><^^^vv>>v^><><v>^^>v>^vv<<vv^<^>><^v^<^>^><v<^^><v^<>^<^v<>v>v^<vv^<^<>><<>><^>><<v><vv^>vv<^><v^>>^v<v>v>^^<<<<>v^^<v<^<^<v^>^><<<><v^>><^>v^v<^v>><>^<<^^^^v^>^>vv>>v^<^<v><<><>^<v<>vvvvv>v<>vv^><^<^v^<v>^v<v>vv>v><>v<<><v^v^<>^^<><^<>v^>>^>vv<><<<<^v^<vv<v>^<<>v<^v^v><>>vv>>>>>^v>^v^>><v>^^v^<>v>v>>^><<<v^v<<^v<<<>>>>vv<>v<<^v><<<>v<v^v<><<^><<<vv>^>v^<^v>^<^>>v<v<><^<<^^vv>^>^vv^^<<<<><^<>^<>vv^^^v><vv><>>>vvv<>vv>^<>>^^<>>v^vv>>v<vv><v^^<v<vv<>>><<^^><>v^><v>^>^>v^^vv<>^><>>>>>vv>^^^^^>>^<<<><<^^>>v>^^>^^v^>v^v^<^<^><vv>><v<><^v^^^>>^<v<>><^^><^vv<^v<^^^<<<vv^^^>><<>>v^<>^v>v<<v^v<<<^v><v<^<vvv^^^>^v>^vv>>v^>^<v^^<vv><><>v><<v<vvv><><v^^<<<^><>v<v>v>^<^^>vv^vv^>^<^<v<><v><<^v^<<<^v^^^<<<<^>v>^^vv<v<>>><<>v<>^><v^><<>^<<^^<>>>vv<^>><^<v>^><^v<v<<^^<vvv><<<>^>>v<^><<^v<^v>v>^^v<v^v<<^>vv<<vv^<v^v>><<^v>>>>^v>vvv><>>v^<>><^<<^^>v><><^<v<v<>^v<<v^<>>^>^<<<^v>v<^>><v^>>>v^vv><^v<><v<<<v^<v^><<^>vvv>v^<>v<^v><v<
^>v>v<>^v>vv<>^<>v<>v<^v<^^<<><<<^<>v<vvv^><<<^v><^<^v>vv^^>v>v<^^<>>>v<<>^<>v<>v<^<>vv<<^<<v^^^><^<vv<^<<v^^<vv>^v^>v^^^^^^vvv^<<^<^<v>>>^<^<>^vv<^^v^v<v><^^vv><<<>>^<v^>^>^v<<<>><v<<>v<>vv<<v>^^<>v^<>^v<^v>><v<><<>><^^><<^v^>>>^^<v^v>v>^><^>^^^^^^<^^<<^v<vvvv<^>>v>v^<>v><^vv>v<<^^<>>>v^^><v<^vv<v<v>>^>><<^vv><v^>^^^^<^^^<v>>^^v<^^<v<v^<v^v<v>vv<<>^^<>^<<v><v>>^><v>><^^vv<^^v<>v>>vvv>>^v<vv<v<^<^^^v<v<<<v><<v<^<<>^<^^<^v^>^^<^><^<^><^^v>v<<^v<<^^^<^<^<v<<<><<v>^>v<<<^<>^>>v<<<^vvv^><v<v>^<<^v<<v><>^<v^v>>vv<<>vv<v<^>vv^^^>v<^^^v<<<vv^<<v>^><v<vv<^^>^<v<>^>><vv^><<>v^><v<v>>vvv^v>vv^^vv^^v<v^<<<v<v>^<>^^v^^><><^v^>v>>vvv<^<<>>>vv^><>v^v>>vvv^vv^v<<>vvv^^<^<<v>^^v>^^>^^v^><<^<<<<>^v<v^^^<><<><>v^<^>^>v^><v>v^v>^>>v^v<^v<<<<v>>^>v<^<v>>^vvv^<^^><<^>>v><^^^>>^vv<^<<^vvv^^><v<v>v^v<>>^>^>>v^^^vv>>^<vv><><>^<<>v<<<vv<v<<<^>^v>^v>^<<<v^^^v>>><v^<><v>^><vv<v><<^^<^>><><v>vv<>vv^^><>^>^v<<^<^^<^^v^><<vvv^>><<><>>v^^<^>v<><^^v^v^v>>v>^v><<<<<^<<<>>>^v^<v>v<v><^^><<>vvv^v<^<^v<<<^^>v>>^>>^<vvv>>
^>><<v<<>^v^><^^><>^><^<>^>>>vv^v>>^^<vv^^<v<><^^>^^<><<>><>^v><<<<><<v<<<<v<^<>>><>^<^<<v>v<><><v<>>^v^<<>^^<^^^^v^v^<^v<^v<^v^<<^>v<<v>vv>^<<^<vv^v<>^vvv<>>><<<vvv><^^v^^<vv^vv<<^^vv<v<><>^v>>><<^v<v^^<>>>^>^v<<v>><<v^v^^^vvv^v<v^v^v^v^<<vv^>><>v<^><^>v<^v<^^^^<<^v><>><^v<>>><><>v<^vv><<<<^<<>vv<>v>>>^<vv<<^<<<v<^<^^<^^<v<<vvv^^<<>>v>>>^>^>v<v<>^^^v>v<^<v<>>>>vv><>vv<^<>v>^^<v<<<><>><<<<>>^<>>>>^^>v<^v>v^><>v^^^>^v^vv<vvv<^<^>^v^>vvv^<<<>>vv<^^<^>v^v>^^<^v^v<>v>v<v><<<<<>^^<<>>vvv><vv<<<>v><vv>^>><<v>>^^v<v>>v^<^<^>^<^v<>>v<><vv>>v<<v^v>>>^>vvv>><^<vv>^v><<>v<<>v<<>vv^^v<vv^v>^^^>v<^v><v<v^v<vvv<<<<<^<^<>v>^v>^^>>vv^^v^<^vv^^>>v>v<v<vv^>v<>v><><><vvvv<v^<vv^v^v^<<<><<>v><v^>>v<>vv<<><<^v<v^>^>v><v<>^vv>>^^<^<v^><>v>>^><v^<>^^<>>>v<^v>v^vv>^^vv^v>vvvv<v^><>vv^>^vv>><<>^v^<><<>v>>^v<^v^<^^><>v<^v<v><<<><^v^^v><<vv<^v<<>v^<<<^>>>v>^>^vv>>^^^>v>v>><v^>>v>^<v<v>>^<v^v^v>^v>^<<v<^^<<>v>v<vv>v<<^<v^v<<>vv<v<>>v<<>v>^><vv<><<v<^>^^>^^vv>^<vv>^<<^><^^vvv^>>^v<^><v>^vv^^>v>>><^^^^^>>^v><^^<^>v
^<<v>^^v<>v^><^v^<^^^v<^><^^<<v><^^vv>vv^<><^vv<<<<<<v^>^<<><<^v^>vv>^><^>>v>v^<^^v<<><^>v>^>vv^v<><><v^<<<v<<v<>><^>^v<v>^^^<vvvv>^v^<>>v^^^<^v><><<>^v>><<vv^v<<v>^v<>^<<vvv<^v<^v<><^v^>>>^<vv^>>>>vv^<<<vv^^v<<v^>>v>>v>><vv<>>^vv>v><v>><<<vvvvv^^vv^^^v^^v<v^^v^><^<^vv>>v>v<^>v^<>^>v^>>v<<^vv><<vvv^>>v<v^>v<^v<v><>^>v^<><<vv<^^><^<v^^<<vv<v<>><v<^<><>>>>vvvvv<^<<v>>v^vv^>v^<>vv<>><>>>>^<v>v><^<>>vv>v<>v^v<<>><>^v<<^>^^^^<>^<<^><vv>v<>^v^<<^vv^^>v^^^<^<^>v>>vv<<^<vv<>>><<^vvv^^><<v<vvv><^>^vv>^^^^<^<^vvv^<>vv>vvv>^^^><vv^><>>v^^v><><^vvv>>>v<<^<^>^^v>^<>v<>^v^>v^^><^>^><>^v<>^<^><><>vv>v>^vvv>^^<^<<^v>^^v>v<v>v<><v^<><<<v>>^>v>>^<^v<<<v>^^>v>vvvv^>><<^^>v^>>^>vv<^<vv^v><<^^v<>>v><vvv><^<v><>v^^^<>><>^>vv^^v<>><^v>^^<><^>^v^^^<v>v>v>>>^v<>v<><^v^<<>v><^v><^>^^>^>^v<>vv<vv><^>^<^><^v><v<^^>>><>>><>>v<<^v><><^^^v<^^<vv><><vv<vv<^>>>>><<^>^>^>v<v>v^^^v^>v>v^^^vv>><^^>>v><^^vv<>^<>>^^v<vv^vv><<>><v<<^v^v>^><^<v^^^^><vvv<^>v<<>vv^v^<><<^<v>vv<<v<vv<^vv<>^^v>vv^v>v^^vv^^><^<vvv>v^^<>^^vvv>v>vv
<>v<>>^^<^>v^><v<vv^^v>>>^^^<^v^^^v^^<>v<>>^<vv<vvv^<<>^^><v><^v<vv^^^v<><>^<<<^<^<<<vv><v>^v^^<<<v>>^^^v>vv^^>><<><><>^^<>>^^<>^<^<^<v>^<v><>>^<v<<^vv^v<vv>v>><vv^>>>^v<<>>>^>>^<<^^v<^^vv>><vv<^>^v<<>v<v^^>^v^^v^>v^vv>>^>>v<>>v<>>^v^<vv<>v<v<v<<vv>v<v^^^<>>^vvvv^^^<<>><v^>^v^v<<<^<<<^>vvv>>^><^^>v^>vv^<<<^<^v>>^^<^<>><<^^<<vv<v^<v<v>^v<vv<v^v^>>><<vv<v<<<><>vv>>>^<<<>>vv^>^^<<<<<v<vv>^<vv^>^>v<<>^<^v<>^vvv<<>>^^^<^^^v>^^v^v^^^^>vv>^>vvvv^<v<><<v^<^vv<^vvv><^vv^>>>>>>vv^<v<<v<^>v<<vvv<<><v>v<>^>vv>>>^<>v<^><^>^<>vv>>>^v^>^v>^v^<v>v<<<<<<v^><>^^<>^v^<<^^^v>v<<><^<><<<^<<<<><>^><v^>v>^^<><^vv^<vv<^><<vv^v<^>^^><v<>v<<v>><>v<<v><<^v<^<^<v<vvv>^^vv>><>^>vv<>^><v^^v^^<><<^>^v<>^^v>v<^>^^^<<v<<^vv^v<vvv^><v>^<^>v^vv>>>>>>v^v><vv<<>v<v>>v^vv>v>^v>v><^<<>v<>v<<vv<^><v<<^>>vvvvv^^^><v^><^>^^^<vvv^v>v^v<^>>vv^><<^^>^v<v<v<<<<<v^v^<v>><>>^v^>>vv<v^v^^>vv^<v^^^<^v>v><<><<^v><<>^v>^^^>^<<^>v<>v^<vv>^>>^<v^^<><^vv^^<>v>v>v^^><v><^<v><^<>v^<<<^^<<>>v^^vvv^v<vv<>v>^>^^vv<<<^><vv^>^v<>v^<>v>v>vv^v<<v<^
^>^<>^^^^v^<v><<><>v^v^<>vv><v<>>^<^<<<v^vv<<^^<<>v<v>vvv<^<^^<>v>v^v<vv<^vv^<>^><v<<^v^^<>^>>v<>><<^>v<v<<^v>v^^>v^v<^<^<^^<^><^^>>v^<^<<^<v^^><>>v^<^<<<^<v><><>^>v<<v>vv<vv^<v>^<<<>>v<^<><<>^><v<v^<v^^v<<>v^^v<<^<v<>^>^v<^^v<^><v>vv><<^^v>vv<^^<^<<<^<>v><^^v><>>vv<^v><<v>v>^^^><>v^v^>v><^^<v<>^^^<>^v^vv>v>^^^^^v>^<v<<>v><>^vv><<>vvv>v^^^>vv>^>vv>v>^v^<<><<vv><<>^vv^^><vvv<v<>^>>^^><^<<^v><v^^<^<>v>>>^>>^<<v^v>v^>^^^^>v<^^><^v>v>^^><>^^<^v^<<<<v^<<^v<^>>v><^v>vv^<^>v^^^<v^<<v>v<vv^<vv>v^>vv<v<^v><v><^^><><<v^^<>>^>>><v<^^<^><>vv>><>>>^vv<v^v^><v<<^<>vvv>>><>^>v><^^^><v^v^<><<v>v^v^>>>v>^^^>^>^v^<v<>^<<<^<<<^<<>^>v<^<^v^vv>v<vvv^<>v>v<vv>^v^^v<<><<^^^^vvvvvv^v^<v<><<>v<>^>v<^<v>^<v^<>><<<v<vv>><<^>v><>^<v^<vv<>vvv<^><><<v<v^><^vvvv<>>^v<v^v^<^v^^>^^<v>^^>v^vvv^><<v>v<>^><^>v>>v^>vv<<v>^>v<^^>>^^>^<>^<<^vvv<<><^^>>>>^><^^><^^^>><><v^<v^>>>v^^<v^v>v<><><<<<v>>>>^<^v>^>^><<v^<>><><<vv>^<>v>v<>^^>>vv^>>v<^<<>vv>v^<v^^>>>>><>><>><v^><^v>v<><^v^v<>>><>^^>>^vv>^^^^^^<v^^><<^>^^>v<^^><>^>v><<<
>^<^v^<v>^<v<^v^v>v<v<><>v^<>v>^^<<v^<>v><<v<v<v<<<<^v<v>^>v<vv<<>>v><><v>^v^^>>>>^<v^v>>^>^^v<^^v<<vv<^v^>v<><v>v<v<vv><vvvv<<>v^v^v<>^v>>v<v>>v^v<^<v>>^v>>v<>^^<vv^v<><<^vv<v<<>^<v>^><<^>vv^v<<>vvvv<^vv^<v^^^^^^v<^>vv>vvv>v^v^<<>v^<<>^^^^<>v<^^<^>^^vv><v>^<><>^v<^vv>>vv<<^<^<<<>v^>v>v>vvvv^<>vvv><<><<>vv^<<vv>^<v^>vvv<v^^v>^><^<^>^^<v^<v^vv<^v>^>v^><<vvv^<<vv^^^^>>v<>>vv>vv>>^^^<^v<^><<v>^><^^v<^v<><^v<^<>v>>>>^><><>>>>v<<>><^^v^v^><vv^>v^<<><^>>><vvvv<^<^>^>^>v<>^>><v<>vv<vvv>v<v><v^<<>>>>><>v<^^<<<^^>>><v^<v<>vv<^^<<><v><^<vv>v<^^<>><^>v^><>^v>v><>v^>><><<^<^<>^^^<>^^v>v<^><><vvvv^^<^>^v^vv><^v><vv^^<^v<vv>>>v<<><^>>>>>>>^^<v<^<<^^^>^>v^^>v><>^vv^^<><>v>^>><^^^^>v>^<>>><v<^^^^<>>v<<v<^>v<^<^v><vv^>^^vv^>^>^>>>^v^^>><vv^^v<v<v>><<>vvv<^<^v>>^<v>>v^<v<v^^^<^<<vv>^^>vv<>><^<<<^^><^^>v>^v^^^<<<^>v<><>>>^<>^^<><<v>v><><<<<>^>>>><>>>>><v^<^^>v^^^^>v>>v>>^vv><<<^<^^^^v>v><^<^^><<vv<^^v^v<<v<<^<<^^>^^>>><>^<v^>><<^v><<><v^<>v^^v^v<v>vv^^<>>v<<^<>v<v^<^v<>>^<<^>><^^v^v^<<><><<v>v><v>v<v<<><
<v^^v>^v>^<^vvvv>>vv^<>v^^v<>>>^^<v<<><>^>^^<^>^v^><v<^v^^<^<v><<v>^>>v^^^v^><>v>><>>v>>vv>v^^><>v^<>^<^<<<v^vvv><^<^<v^v^^v<v^v>vv<^^v^v^v^>><>>>>^^^^^<v>>v>v^v<>>vv^>>^<^^vvv>><^v<<<v^^^^<^><v<v<>^<^<vv>><<^^v>><<<^^^v>v<^^<<vv<>><>v^^^>v<^^>>^<^<^<^><<>v<^^<vvv<vv<v^<>^>><>^>^>^^vv><>>^v><<^>v<>>vv>v^><<vvv<<<<><>^<<vv><^vv<v<>^<^<<v><>v>>^<>^><v<<>^<<vv^<^^v<<^>^^vvv<>v<<<^vv<>>vv^^v^^v^^><^<^^<^<>>>^<<^v<>><<v^<v<>><^v<v^>^><>><vvvv>v^><^>^v<><v>>^<v>><^<^>^v^>>v>^v^<<<>^^<v>v<v^v>v<^^v<<<^vv^v<^<>vv><^v<^>><><^<<>>v>>vv^^<><v<vv^<^<<<>>v^>v^>^<v<<v^<<^>^<^<<<vv><<>><v>>>v<v><^<><v^>>vv^<v^>vv^<v<<<v>>v<vv^><v^vvv>v^v^><^v>^<v>v^^<<v^^v<vv><v>v>><<^^<<v<vv>^>v^^>^>v^><v>><>><v^^v<^^vv^^^v<v>^<<v><^<<v<^v>>^^v^vv^^v^vvv<v^v^^<^v>^v<<v<>v>^<<^v^>>vv<<^^^^^^^v^^>v><<^v^vv^>v><<<<v>v>v^^<vvv<<^^^>>v<>>^><<^<>v^>>v><v^>^<v^v<>vv^>v^^<^>^<v>^^<^>v<<vv><^><<<><>vv^v<^>^<><<<>v^vv^<<>>^^v>^v^v^^>^<^<<^^v>>>^v>v><<^>>^^<v>^^v>v>^v^<>vv><^<<v>^><<<>>v^^v>>>^>^^v^^>^^>^vv>^^^v^<<<^<<><>^^^^>
><<<>>v^<>vvv^>>>>>v<v<vv><>v<<v^^>v<^>>><<^<v^^v^^>vv>>><<^<<v>>v^v<>^^>^v>>v^^<v<<<v>v><<vvv<^>>v<^^<v^><<<v^<v^v>>^vv<><^vv<>v><v>><v>v><<^>v<>>>^>v^vv^vvvv<>v>^^<>>^v>^<v><^v<^>><<>v^^>v<^>vvvvv<v<^<>>>^^>v>^v^^>><^^v<^v^<^>vv^>vv^>^>^^vvv<>>v^vv<v>v^^^^^>v<^^>><<><^>>>^<^^><<<^<^vv>>>vv^vvvv<v>vv>vv<>v^>><<>>^>>v^^v><^^><<>v<^v<<^v^>>^^v>^^vvv^^^>v<v^^<>^^>vv<^v<<>><<^v<^^>>>^^><><^^^v><vv>v>>>vv^<>>>^>>><>><v^^>>v^>^^v^v^vv<v>v<^><^<<<<>v^v^>^<^v^<vv<vv^v<<<^^><<^^^v<>^><^<>>vvv^<^^<>>><<^<^v^v^^>^<v>v>><>>>v<>><<>^^>^><vv>^<^<^>^v^>^^v<>v^<v^^v^<<>>>v<>^^^vv>^^vv^^<>^^^v<<vv>^^v<<v<>vvv^<<<<<v^^^v>>^>^v>v><v^<<v<<v^<v^<>^^>>^><>^<v^>^>><>^><<vv>v>v^<^vvv^><>v^v<<v><^v<>><<>v<v<vv<^<v<<v^>>^vv<><>><v^v^<^<^>^^<<>>>><><^>><v<^><vv<vv<^>^>^vv^<vv<<^>>><^<^<><^vv^^<<v^>v>>vv^>>^vv>^>v>>^<>>>v<><<^>^vv^^><>v>>^<<v^<<^vv<><<v^>v^^^^vv^<><><^v<v>^>vv^<vv>>vv>v>^>>>>v<v<^<v><<^<><><<v^^<>>><<v>v<>>>>^>^v>>><<<<><><v><><vv^<<<v>v>^>^v<>vvvv^<v^>^v^>>><>v<v^>^>^vv<v><>^<<>><vv>vv<^<v^>v><
vvv^^<^v^><<^<v<>><^<>><^^^^>>vv<vv<^><<^^^<vv><><>^v^>v^<v<v><v^^><v<<^<v><><v>^<<>^^v>vv><^vvv^^vv<^v<><<v^>vvv>>^<<>vv^^^v>v<v^<>>v^vv<<v>v<^^<>v^<>>>v>v>^<<v<<>^<>^<^<>vv^>v>v>^vvvv^>v^<<<<^^><^>v^>^^^>vv>v<v<>^^>vv<^><^<^<<v<vv<v^^><><^v<^^^^>><^><>><>v>>v>vvv>>v<><>>v<^>^vv>v>>v^^>^v<^><v^v<>vv^v^<vv<v<v^<v^^<v>^<^>^<^<^^<><^<<<<>><vv<^v^><<<v<<v<v^^^v<<<^<<vv>>v^v^>v^^><<<^>^<>>v>^<^v>^^<<v^vvvv<^^v>>^vv^<vv>^^^>v<><<^^^v^^>v>v<<<>^><<v^<v>v^v><>^^<>v^v>v>vv^>^vv<^>>vvv<<^v<^v<>><v>>^vv><v<<><<v>>^<v<v^<v^<>vv>v<v^<v>>v^^<>^>>>^<>^vv><<<<><^^<v<<<>^<v^<>^v<v><>^^>>^<vv^><v^^>>^^v>vv^>vv<<<^^^<>v>^>v^<v<^v^v^><>><v^><^v^>v^<>^v>v<^<^<<>>>>>v^^><><>^v>><^<^^v<<<^><^<^v<v^vv^<<>>^v>v^<^>^<^><v><<>v^vv>><vv<^v^^>^^v>^>^>vv>v>><^>^<>^<>>v<<<<v^^<^<^<vvv>^>>>v^^<v>v<^>^>vvv>v<>v^v<vvvv^<<<v^<><>>vv<<^>>>^>vv^^<v<<^>><v^^>^>v>v><v^<^v<^v<^>><<v<<vv>vv<<<>^^><^<<v^<v>vv^><<<v<^^v^>v<>^^^^>^v>>^>^v<v^^vvv<^<>>>><><vv^^><<<<>><>^<^^v>v^><<vv^<^v^^^v^^v>^>>><<v^<>^>^^<v<^^<^^<vv><>^<>^<>>^
v^<v<>^>>^v^<<v><v^<v^<<><<>>>^v<^^><>>v<>v>^<^><v><><<<v><^<^vv<v^v^<vv<v<<^<v<^^><v^v<>^>>>v<<<<<^v<><>^v>v^^><<>>vv^<^>v<v><^>v^v<^vv^<vv^>>^v<<<<<v^^v<>v^>>vvv<v^>v^<>vv<<v<><vv>><v^^^<^<^^>^v>vv^v^>><>vv^>^>^<vv><v^vv^^^^v>v^<v<^v>v<<>^<^>vvvv<>v^>^v<vv><v^<<>v><<<<><v^v>^^^v^<^vv>^<<<>^v^>^<>v^>vv<vv>v<<<^vvv<<v^v<v<>v<vv^<<<v>>>>^v>^>v<<^<<^^><^<<<<v<<<>>>^<>v^^>>>^^v<^^^^^^^><^<>^<v><<^v<>v>>>v<^>>^v<>^>>v><<><<^>>^vvv>^>^>v<^><<><^<>>vv^v^^<>v^v^v^<<<^>>v><<vv><>v<><^^v<>v^v^<^v^><>v><^<^vv^<^<^^^<<vvvv><^^^>vvv<^<>^>^^>><vv<>v<><<<<<>^v<><v>^>><^>>>v>>><>^vv<>vv^<v^>>^^>^<^<v>>^^^^^>^><<<<^<v><v>^vv^^>>v^<<^>v<<<v^v>^^vv>v<>^v^>><>v^<^>v^<v<vvv>vvv>>>^^^<^v>^^^<<^>^>v>^>^vv<<<>^>><v<<v^>v>v>v^^^>>><^<<^<^<^^vv^>>^>^vvv^v<>v^<<^v^>^v>v<v^^^vv^v^>vv><<^^^>v><<<><^vv^>v<v<<v^><<v<^><<>^^>>^^vv<v><>v<v<v><^>>^^>v<v>^v<^<><^>v^v<v^>vv<<^<<<^>>vv^^>^>^v<v^^v^vv^<^>v><<^v^^>>>><^v<<v>><v<v^^<<<v<v>>^>v^^v<^v<><<<v^<><<^v>vvv^>>^<><v^><<><>v<v>>v<<<<^^^><^<v^v^^<^^v^>^>v^<<>^v<^v>^<v
<<^<^>vv<>v>>>^<^^>vv^>v^vv^<^<v^><vv>><v>><v><<>^^<v><v<<vv<^>^^v<v<<^<<v^^>^><^<>^<^v<^^<vv^^v^<v^>>^<<>v^v>^v<>^<^^>^<<><>>>^^<<^>^><>^^v<^^>^v^v<vv^^v<v^v>vv<v>><>>^vv^<^<<^>><vv>vv>vv^<^^>^^v<^^>vv^^v<v>vv<^v^<^>>^<vv^<>vv^<>>v^vv<<v>vv>>><<^>v<<^^<>v<v^v^v><>^<v^^><>>^^>>^>v>>>^>^>v<v>v^v>^<>v^<^>><^v>v<<<<^<<^^v>v^<v^>v^v>^>v^^>^<v>vvv><<<^<>^<>v>v>v<>>vvv^vv><v>^v><^>^^<v^>>^v<<v^v<>>v>^<><<^^<^^^^v^>^v<<^^>^>^>^^<<vvvv^v<>v>v^vv><<>><^^^v^^<v^v<>vv^><^^v><v<<v^^vvvv<>><<vv<v<<>>^vvvv<<<v^vv>v><<v<<v>vv<^<>^<<^vvv>^^^v><<<<>>>^v>^v^><^>v^^v^<^><>vv<vv<v^>v^^^^^v^<v<<>vvvv<^^^>v>><<^^v^v>^>><>>vv>^<<>v^<^^<>v^>^>><>v<>^^^^><><<v<vv<^v^<vv<v<v<v^>v<v>v^v<^<<<^<v<<^<<><><<^>>^^v^<^^v^<<><>v^^v<v^^v<^^<<v<>>^<v<^>v^><v^>^<v<><<v>^<><<^>^^><<<>>><<v>>^^>^><>^<v<<>v>><>^>^><>>v<>vvv<>^vv>><>vv^<<<<<^<<<>vvv><^<<<v<^^>v<>^<>v<<>^<v<<<^v>^v>v><v<>^v><v><v^vv^<^<v^vv><<v><vv>v^^>v>>>v<vv^v<^<><v<<>v<><<<<<<vv^^vv^vv^>>^v^v^vv>>>>><^v>>v<^<^^v<<>^<>v<<>^>>>vv<>v^^^>^v<v<v<<>>^v><vv>^^^<<
<<v^<v<vvv^^^<<>>><v<>>^>v<<vv<^>>>v>v>^v^^<>^v><<>>^v<<vvv<>>>>>^v>^><^>v>>^v^<>^>^>v^vv<^>v>^^^^v<^vv<>>vv<v>vv<><<v<<>v^^<<>vv>^^<<<v^v>v<>^>^v><v>>v<v^<vvv^<^<^<<<><^vv><<vv>^^vvv^^<vv<>vv^v>v<^^v^v^<<>>>^v^<^<vv^<>>>^<^<v<>>>^<<>^v>^^^<v>v^^v><v^<>v^^>v>^^vvv^><<v^>vv<<^^^^^v<^>v^<>v<<<><>^^<><>v<<^vvvv^v>>v<v><^>^^><><v>^^^^<vv><v^v<v>v>v>^>v^^><<>v^<>>v<^v>^v<^^<^v>>vvv^<<><<<^><>>^v>>^<v<^v><<^<v<vv<^^>v><><><<v<^v^>v<<<<^>v>^^^<>^^^>>v>><>^<>v<<v>^<<<^v<><v><v^<vv<>^>vv<>v^^v<^<^>><>>v<>v^>^v>>^>v><<>^<^^^><>^vvv^vvv<>^><^v>^>^^><v>vv>vv^v><>vvvv<^^^^^^<v>vvv<^>^^^^^^v>^^^<v<v<><><v>^>vv>v^<^v^<vv>><>>>v>>><v^><^v<vv>^v^<>v^v>v>^^^<>>vv^<>^v<^<^>vv>^^vv<<vv^^vv>><<v<><v^v^^>><><^vv<<vv<<<v>^>v<v^>>^>>^^^^<<v>v<^^^v<^>v<v<vv>v<v>^^>v>^vv^^^>v<<<^^>v>^>>><<^vv>>^^>^^^<>v^>^>^<^<>vvv<v<>><<^v^<^<>vv>vv<>vv^vv^><<<^>v>^<v^>^>><<<<<>v^><^>^>><>^v^<vv^>v<v^>>v<v<^v<vvv<v^vvv^^v><^<^<<v<^^>><v<^v<^^>v>^><<<><<>v^>><><<v>>^><v^^vvvv^v>^>v>^v>v>^^v^<v>^v<^<<>^^<^^><v>v>>>><<>v>><vv^^v>
vv<^v^>v>>>^^>><<vvv<v<^^<^^^^v>>^<^v>vv>v<^^<>>^v^>^v^<<<^<>>v>v<>v^v<<v^v^<^<>vvvvv>vv^<^v<^^^v<^^<vvvv<v<v>^v^^<<v^>^<<v<^<v^^>>v^^^>^vv^^>^<<^v^<>^^><<^<vv^v<>><^v<<>v<<>^<v><<v^<<v>v^>^^^v<>^^vv<<<v>>^v>v>vv<>v^^^>^^><>^vv>>^<<^v>>>^><v^<>>v>><>v^>v<^>v^^v<<>v<v^<^><v^<vv>^^vv<<><<v>^<><>>>vv<vv<<v<<v>v>^<>^^^>v^<^v^<<<<>>^>vv>v><>><v><><<<<<<vv^<vv><^^>^>>><>^>>^^<^^^>^><v^<>^<^vv^<<^><<vv<><v<^v^<<^^^>^vv^<>^<v><^^>>^>^^<<<^v>>^^^vv^^<v<>^>v<^vvv>vv>^<v^<^<v^^^^^<>v^v<v^>vvvvv><^^><v^<v>v>>>^><^v<>^^v>vv<<<<^^<<vv<<v>v^<v<>>^^<>^>>>>^><<v<^>>^<^^v>v^<><^v>^<^^v<^>^^>>v^>>>>>>^<<<<^><<><^<^><>><^^^vv^vv>><>^v>v^^^^<>^^v><>><><vvv<v>>>v>v><^^v<v>>^>^v<>^<^^><vv>>>>>>v<^v><^>^<><^v<<^<>><^<v>>><<><<>v>v^v<<><^^>>vv^v>>^v>><^v^vv>>^>^^>v<<<vv<>v<<^<>^^>>><^^>>>^^>^v<>v<<v<^v<^>v><vvvv^>^><><<^^><v><v>>^<><>>vv^<^>>^>>v<<vv>><^>^v><v>^<>vv^<vvv^vv<vv<^><vv^v>vv>v>>^v>v^<<v>>v>>><v><^vv><><v^v^<<v>>>^^>v><^<<^v<^v<>>v^^v><><>^<<v^<^v^^<v<><^^^^v<v^^vv^v><v<<v^>^<v<<>^>v<^v^v<>>^<>^v>>
v<<v<<<<v><<><^v<^^vv<v>><v^>>><<v^^<^^>>>v><<<>^>v<<^<^>v<<>>^v^<<<<v<v^>>^>>v>>><>vv><<^v<>><^><^><vv^^^>vv>^^<^v><vvv>v^>^<^v>>>>vvvv>>><>^^<^<<^v<^v<<>vvvvv><v<<v><<^<>>v^<<<v^vv<v<v^>^^vv>vv^vv>>>^><vvv^^^<<>v>>v>><<^^>v^^v<<v>vv><^v^>>v>^<<v>v><v^^>v^<^^^<<^<v><vv<v>^>>v<v><><><>>^^^^^>>vv^vvv^v>^^^<>^>^v>^vv^^><>>^<^<<^>^<<>>^v^v<<>v<v>^<^vv<>^v>>>v<><>^v><v^v<^><<<^v^>^<vv<^<v^>>^>^^^vvvv^v^^^<><v^v<>^v>><<^v^v<^<^v>>><>v<>^>>^>vv^<^<^v^>v^^vv^^<^^<>v>v<<^v^^^>^<>>^vv^v<<<v>>>><<>v>v><^<<v^<v^<<^>^v^>vvv<>^><v>>vv>^^<^v^>^v^>>v<v<v^v>v><^><<<v<^v>>>>>v^^<v^^<v<<>^><<>v>>^vv^>^<v<^v^<<<<v^><>v<<v^<><>><v>v^<<^>^>>>v^vv>>v<<^<v<vv><vvv^^><v^<^^^>v^v<v>v>>v^><>>^<>>^^>><^<^>>v^^^<>>v<>v<>>v^v<<^<<<^>^<><<^<^^vv>v^<<^^v^<^v<>^<^vv><>>>v<>^^<<^v^vv<<v^><<vv><^><<<^v>v<vv>v^>>>>>v><^>><vv<^><^><>>^<v<^>>^<^<<><v^<<><<^<v><v>v>>><<^>>^>^^^<v^vvv<>v>>vv>>^v<>>^vv^^^v^>>^<><v>v<><^>v<v^>^^v^^vv<vv^v>v^><v^^^v<^v^><>^<^^>>^<<><<v>>^^<<>><><>v<^<<^^<vv>>v^v<^^^>><^>^<<<^>^<<vv>><^>v^<>v<v
vvv>>^^>v>v<>>^^<>><>^v^<>v<>^v^<^>>^><^^>v><>^><<^v<<<<<v>v^<<v<><<v<v<<><^>v>v>^>v^^^vvv><^^<^vvv<<<^v>v^>>^^><vvvv><^<^>v^>>>vv>v>>>v^<><^vv>v<v<><v>><<^v><<<<<>v>^>vvv^v<>^<^v^<<<v>^vv^^^^^^>^><^><><^vv^<<^<>>^^>>^v^><vvv^^v>^><<^v>^<^>>v<^^>>v><vv<^<><<<>vvv<v>^^<^<v<^><>>^>v<^^<v^<<v>><^>v>>>v^^<>^>v^>>^<v<<<<v<<><<<<>v^><v><^v<^v^>><vv^v><>v><><>v<><v^>^<><>vv>v^>^>>^v^>vv^^>^<^<>>^>>v<^<vv>>v>>^v<<<v<^<^v<>^v^v>^^v<v^<>v^v>v<v>><vv>v>v<><^vvv<vvv^v>v<<^<<>>>v>>><vv>^v^<<>^>v>^>^^<^<<>^v^^>^^v<^>^<><>^^<>v^>v<v^vv^v^v^<v<<><><>v><v^vvv^v<>>>^<^^<>v<^^>>v^>v^^<^>>>>><^^<>>>v^<^>^<^<<^<>^v<v^^<>v^<v>^v<<<>^v^^^<><vv^^><^v<>>>^<>^<<>v><<<^vv>>vv^vv<^v^>v>v^>v>><<<<v^^^^vv<v>^v<<^<>>v>>v<>vv^v>>>vv>^vvv^v^v^v^<<>^^>vv>^>v>^v>>>^v^>v><>v<>><>>><>vv<<>>^^v>vv>v^<>>><^vv>v<>^v<v^<v^^^><>^v^><>^>>>^v^^vv>>><>v<>>^<^^^v<>v>^v^^>><^v^>><vvv>><v<<^^^>>>><^^v>v^v><v>v^<v<^>>^v^<<^<v^><vv<^^>vv^v<<><v><^^^<<^^v>^^v<vvvvv^>><><>v^^<<v>>^^>>^>>^<v^<<vv^>^<>>^v^v^<^<>^>^>^^vv^^vvv>>>^^>v^><<^<<
vv<^v^>^<^>^^><vvv>^vvv<v^v<v<vvv^><v^<>^>^<^<<<<<<<>>v^^>v^<<^><>^>^>><<<><^<>^>v^^<<vv>vv<><vvv^>v<^^^^>v<<v<<v^<>v<<>><^^<^^^>>><>v<>v^<>vv><^>>>>v<v^<v<>v<>^<<v<^^<<^v><^^>^v<v><<v^v>vv^^v<<>>^^^<><<v<^^^^v<vv^<v^>v^>^<^vv<>>>v>>><^<^<v>^<<<v>v^>>v^v<<^vv<^<v^>v<<>><vvv^<^><^^<>><<v>^><<>^v<>><><vv<<^<><>v>>v^>>^><v^^><>><>v^<<<><>^>vv^^^<^v><v<v>v^^^^v><>>^><>>v^^v>v<<><<^^<^>>v^^<v>v^^v><v><<<^>vv<<>^<v<v^v^>v^<v<<v<>>>^vvv<>^<><^^><^v<>^>v<vv>>>><^>>><>><^<<>vv><^<>^v^<^v<^>v^^>v^<v><<v>><^vv<^vv<>^>v^<v>><<v><><^>v>^<<vv^v<<^^<<vv>v<v<v^>^>>^>v>>v>>>>><>v<^>><^^>>^vvv><v^^v^<v^<^<>v^^vv<^><<><^vv><v<<<><^^<>><v^<^<^v><^v<>v>vv^<^v<<^^^><<vv>v<^v><<>vv^vv<>v^^^^^>>^>v<<<>v^^<<<^<<v<^>>>vv>^>^<v>>^^<v^^<<vv><<<vvv>v><^<^<>v^<>^>^^>v<<><>><>><^<<v<><v<v<v>^^<<^>vv<>^>v^<<><^<<><<^<v><><>v>v^^>>v<>^v<<^>>^vv><>v^^>^vv<>v<v>vv^v^>^^><<^><^>v>>^^<^<v<v<^><<v>vv<^v<^>>v><><<v^^<<^^<>^^>>v<<>^v>v^^^<v>vv><v^>>><^vvvv><^^vvv<v<>vv^^<^<<><<>^v<^<^v^v^>><<vv>^<^<v<<>vv<<^<<>v<<<<<<vv>v^v<
<^vv>^v>^<<>vv<<>>vv<v^<^v>^<>^^><^^^<><v<<<>>^>v<^v>>^>><^<<><^<<^^v<v><^^^^<>v><^>>>vv^>>v<v^<<v^<>v^>^^><>vv<^>^>>>v^v<>v><^>v<<<<^v^>^v><vv^>^^>>v^<v^v>>^>>>^>>vvv>>v<^><>><^^>>v>v>v^^<v<><>^<v<^vv<<^^><>v^><>^>>v>v<>^<vv>v<>^><>^>vv>^v><v><>><<><<v>^<<^>v^<v<vv><v<<v^^><v>vv<v^>v<>>^^<<^<^^^v>v<v><v<<^<^>v^<>>><<^>^<<^<^v<v^^^v^^v<>vv<>^v^^^^^>^><>^^^^>>^>v^^>^v<>vvvv<><v>vv<<><^<>^<<<<^^^><^<^>>>^<^v^vv^^^v^v^^<<<^vv<^<^>^<><><<>>vv^^>><>><<^^<v<<><v>v^><>^^<^v>v^^<v<<<>^><^<<>>^^<^^<>v^vvvv>^<^v>^<v^>^v<vv^v>^^vv>vv^>^>v>^v^<>^v<><>^<>vv^<^>v^^^<vv^>^vv><v<v>v<>^>vv>v<^<v<>v^>^<>><v^><<<^v^v>>v^^>>>^^><v>^>^<>v<v^<>^v<<>v^>^>>v><v><>>>^<><><v>^^^v^^v<<v^><><v>>v>^^<<<<^^^^v>v>v^^>v>^^v^^^v><vv^vvv<^v^<v>>vv<^^v>^^v^<>vvv^^>>v>^<^<^v>^<<vv^<<<>^>><^<<^><>v^v>v^<>><<<<^^<>>^>^>>v<>v<^<v>>>v<<<<>>v^<^v>v>^^v<<v<<^>><<<^v^^<<<<v<<^v<^>>^v^^vvv^v>>^^>v<>v^>>>v<<>^><><<<v^v>>^vv>^v^^v^><v^<^>v^^^vv<^<>vvv><v>^v>><<^>v<v><v<^>>v^<^^vv>v<>^>><v<^v>>><vv^<vv^^^>>>v<^>v<^^<>^vvv><^<>^>>>^
><<<^v>v>^>^^>^>^^v>>^v>v<v>>^^v<^<<>^^<<v^^>>v><>vv>v^>^^>v<v^^v^^v^^>v>^<vv^^<<^><<><^v^>vv<^v>^vv^<<^<<<^v^^<><^><<<<>>^><^>^vvv>><v^<^<v>^vv>><v>v>><^^v^>^^<^<^<>^v>^v>^v><<>^>v^^vv^^^v^v^v<^vv^^vvv^^^v>^>>^>vvv>>^^<^>^v<>>^^>>v>^^<v><>v>>v<^<vv^<vv>^>v<^^v>^<>>^^v><>^^v^v^<^<<^^<vv^^^<v><<<>^v>v^<<<v<v<>^v>vv<v<^<v>^^>>><<^^><^v>^vv<<^>>^>^vvv><>v<^v>vvv<v<^<>^^^>^^>^^>v^><^<^v<><<<v^v>>^<^<v<^<v<<<>>v><v>v>><>^>><^<<>v^<<^v><^<^<>><^<<^v><^^><v^>^^v<<v<^<^^^<>^>>^^v<<^<<v^>>v<v<^^^v><v<<^>^v>v><<^<<^<>^>v>^^<vvv<^><vv<<<^v><^^vv<<>^v^<^>v^v>v<<^v>><><v<v><<><><<^<<>^>vvv<><<^<v<^^^^><<^v><>v<>v>v>v><^>v^v<^<v^^<>^v^^<v><vv><>vvv>^^<><><>^><^<>><^^^>>>^^>v<v^<<<<>>v<v^<<^^^^vvv^^<^v^><v>><>v<v<>>^^v<v>^<<^^<>>>^>>vv><^>v>^<>^v>v^<^>v^><<<^<<^><><^^v<^<<>v<>>>>v><^<v^^<^<>^vvvv^<^^^^><^^<^><v>>^v>v^^v^<^vv^>^<><><v<>>^v<v<<v>vv<<^>v>v><v^><^v^^<>^<^<<<>>>v>>^v^><>v^<>vvv<<v<v^v^^^<><>v>^v>^<v>^v>^vv^<<v<>^^<^vv>v<^^v^^<^^>^<v<v<<<v>vv^><v^^>v>vv<<>>v^^<^<v^<v^<<vv>^^v^>v<>^^<<v<^v<"""

read : DataSource -> String
read dataSource = 
  case dataSource of 
    Input -> input
    SampleSmaller -> sampleSmaller
    SampleLarger -> sampleLarger
    SampleSituation -> sampleSituation

initModel : DataSource -> Model 
initModel dataSource = 
  let 
    data = read dataSource
    (rows, moves) = 
      case String.split "\n\n" data of 
        a :: b :: _ -> (a |> String.split "\n", b |> String.toList)
        _ -> ([], [])
    numberOfRows = rows |> List.length 
    numberOfCols = rows |> List.head |> Maybe.withDefault "?" |> String.length 
    warehouse = rows |> List.map (String.toList) |> Array2D.fromList
    robot = findRobot warehouse 
  in 
    { warehouse = warehouse
    , large = dataSource == Input 
    , wide = False
    , robot = robot 
    , dataSource = dataSource
    , moves = moves
    , paused = True
    , finished = False  
    , tickInterval = defaultTickInterval 
    , message = "?" }

init : () -> (Model, Cmd Msg)
init _ =
  (initModel SampleLarger, Cmd.none)

-- UPDATE

type Msg = 
  Tick 
  | Step 
  | TogglePlay 
  | Faster 
  | Slower 
  | Clear 
  | UseInput
  | UseSmaller
  | UseLarger
  | UseSituation

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

-- let tryFindSpaceSimple (warehouse : char[,]) (robotPos : Pos) (move : Move) : (Pos*Pos) list =
--     let rec loop (x, y) (swaps : (Pos*Pos) list) = 
--         let (dx, dy) = moveToOffset move 
--         let pos = (x + dx, y + dy)
--         let nextSwaps = (pos, (x, y)) :: swaps 
--         match Warehouse.get warehouse pos with 
--         | '#' -> 
--             [] 
--         | '.' ->
--             nextSwaps
--         | '[' 
--         | ']' 
--         | 'O' ->
--             loop pos nextSwaps
--         | _ -> failwith "?" 
--     loop robotPos []

tryFindSpaceSimpleLoop : Array2D Char -> Char -> Pos -> List (Pos, Pos) -> List (Pos, Pos)
tryFindSpaceSimpleLoop warehouse move (x, y) swaps = 
  let 
    (dx, dy) = moveToOffset move 
    nextPos = moveStep move (x, y)
    nextSwaps = ((x, y), nextPos) :: swaps 
  in 
    case Array2D.get y x warehouse of 
      Just '#' -> []
      Just '.' -> nextSwaps 
      Just 'O' -> 
        tryFindSpaceSimpleLoop warehouse move nextPos nextSwaps 
      _ -> [] 

tryFindSpaceSimple : Array2D Char -> Pos -> Char -> List (Pos, Pos) 
tryFindSpaceSimple warehouse robot move = 
  tryFindSpaceSimpleLoop warehouse move robot []

tryFindSpace : Bool -> Array2D Char -> Pos -> Char -> List (Pos, Pos) 
tryFindSpace wide warehouse robot move = 
  tryFindSpaceSimple warehouse robot move

-- let rec moveStuff warehouse swaps = 
--     match swaps with 
--     | [] -> ()
--     | (pos1, pos2) :: rest -> 
--         let cell1 = Warehouse.get warehouse pos1 
--         let cell2 = Warehouse.get warehouse pos2
--         Warehouse.set warehouse pos1 cell2 
--         Warehouse.set warehouse pos2 cell1 
--         moveStuff warehouse rest 

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
              wh = warehouse |> Array2D.set y1 x1 cell2 |> Array2D.set y2 x1 cell1 
            in 
              moveStuff wh rest  
          _ -> 
            moveStuff warehouse rest

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

updateClear : Model -> Model
updateClear model = 
  initModel model.dataSource

updateStep : Model -> Model
updateStep model = 
  let 
    wide = model.wide 
    warehouse = model.warehouse 
    robot = model.robot
  in 
    case model.moves of 
      [] -> { model | message = "No moves!" } 
      move :: movesLeft -> 
        let 
          (wh, rb) = tryMoveRobot wide warehouse robot move 
          msg = "Move: " ++ String.fromChar move 
        in 
          { model | warehouse = wh, robot = rb, moves = movesLeft, message = msg  }

updateTogglePlay : Model -> Model
updateTogglePlay model = 
  { model | paused = not model.paused }

updateToggleWide : Model -> Model
updateToggleWide model = 
    initModel SampleLarger 

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
    UseInput -> 
      (initModel Input, Cmd.none)
    UseSmaller -> 
      (initModel SampleSmaller, Cmd.none)
    UseLarger -> 
      (initModel SampleLarger, Cmd.none)
    UseSituation -> 
      (initModel SampleSituation, Cmd.none)

-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
  let 
    tickSub = if model.paused then Sub.none else Time.every model.tickInterval (\_ -> Tick)
  in 
    tickSub

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

view : Model -> Html Msg
view model =
  let
    textFontSize = if model.large then "12px" else "28px"
    warehouse = model.warehouse
    rows = toWarehouseRows warehouse
    -- Insert robot symbol.
    elements = rows |> List.concatMap (toRowElements)
    message = model.message
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
              , Html.div [] [Html.text "Day 15: Warehouse Woes" ] ] ]
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
              [ Html.Attributes.align "center" ]
              [ 
                Html.input 
                [ Html.Attributes.type_ "radio", onClick UseInput, Html.Attributes.checked (model.dataSource == Input) ] 
                []
              , Html.label [] [ Html.text "Input" ]
              , 
                Html.input 
                [ Html.Attributes.type_ "radio", onClick UseSmaller, Html.Attributes.checked (model.dataSource == SampleSmaller) ] 
                []
              , Html.label [] [ Html.text "Smaller" ]
              , Html.input 
                [ Html.Attributes.type_ "radio", onClick UseLarger, Html.Attributes.checked (model.dataSource == SampleLarger) ] 
                []
              , Html.label [] [ Html.text "Larger" ]
              , Html.input 
                [ Html.Attributes.type_ "radio", onClick UseSituation, Html.Attributes.checked (model.dataSource == SampleSituation) ] 
                []
              , Html.label [] [ Html.text "Consider" ]
            ] ]
      , Html.tr 
          []
          [ Html.td 
              [ Html.Attributes.align "center"
              , Html.Attributes.style "padding" "10px" ]
              [ Html.button 
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
                Html.div [] [ Html.text model.message ]
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
