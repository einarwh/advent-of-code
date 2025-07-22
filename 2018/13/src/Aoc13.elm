module Aoc13 exposing (..)

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
defaultTickInterval = 100

-- MAIN

main =
  Browser.document
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions }

-- MODEL

type DataSource = Input | Sample | Sample2

type alias Pos = (Int, Int)

type Dir = N | W | S | E

type Junction = Left | Forward | Right

type alias Cart = 
  { dir : Dir 
  , pos : Pos 
  , switches : Junction
  }

type alias Model = 
  { mine : Array2D Char
  , carts : List Cart 
  , dataSource : DataSource
  , steps : Int 
  , removeCrashedCarts : Bool
  , paused : Bool 
  , finished : Bool 
  , tickInterval : Float 
  , message : String
  , counter : Int 
  , debug : String }

sample : String
sample = """/->-\\        
|   |  /----\\
| /-+--+-\\  |
| | |  | v  |
\\-+-/  \\-+--/
  \\------/   """

sample2 : String 
sample2 = """/>-<\\  
|   |  
| /<+-\\
| | | v
\\>+</ |
  |   ^
  \\<->/"""

input : String
input = """                    /--------------------------------------------------\\                                        /----------------\\                    
          /---------+--------------------------------------------------+-------------\\                          |                |/-------------\\     
          |   /-----+---------------------------------\\                |          /--+--------------------------+-------\\        ||             |     
          |   |     |                                 |        /-------+----------+--+--------------------------+-------+---\\    ||             |     
          |   |     |              /------------------+--------+-------+\\         |  |                          |       |   |    ||             |     
          |   |     |    /---------+------------------+--------+--\\    ||         |  |                    /-----+-------+---+----++-------------+---\\ 
          |   |     |    |         |                  |        |  |    ||         |  |                    |     |       |   |    ||             |   | 
        /-+---+-----+----+---\\     |                  |        |  |    ||         |/-+--------------------+-----+-------+---+----++-------------+\\  | 
        | |   |     |    |   |     |   /--------------+--------+--+----++---------++-+--------------------+-----+------\\|   |    ||             ||  | 
        | |/--+-----+----+---+-----+---+--------------+--------+--+----++\\        || |                    |     |      ||   |    ||             ||  | 
        | ||  |     |    |   |     |   |              |        |  |   /+++--------++-+--------------------+\\    |      ||   |    ||             ||  | 
        | ||  |   /-+----+---+-----+---+--------------+--\\     |  |   ||||        || |  /-----------------++\\   |      ||   |    ||             ||  | 
        | || /+---+-+--\\ |   |     |   |              |  |     |  |   ||||        || |  |                 |||   |      ||   |    ||             ||  | 
      /-+-++-++---+-+--+-+---+-----+---+--------------+--+-----+--+---++++--------++-+--+----------\\      |||   |      ||   |    ||             ||  | 
      | | || ||   | |  | |   |     |/--+--------------+\\ |     |  |   ||||        || |  |          |      |||   |      ||   |    |\\-------------/|  | 
      | | || ||   | |  | |   |/----++--+--------------++-+-----+--+---++++--------++-+--+----------+---\\  |||   |      ||   |    |               |  | 
      | | || ||  /+-+--+-+---++----++--+--------------++-+-----+--+---++++--------++-+--+----------+---+--+++\\  |      ||   |    |               |  | 
      | | || ||  || |  | |   ||    ||  |       /------++-+-----+--+---++++--------++-+--+----------+---+--++++--+---\\  ||   |    |               |  | 
      | | || ||  || |  | |   ||    ||  |       |      || |     |  |   ||||        || |  |          |   |  ||||  |   |  ||   |    |               |  | 
      | | || ||  || |  | |   ||    || /+-------+------++-+-----+--+---++++---->---++-+--+----------+---+--++++--+---+--++---+-\\  |               |  | 
      | | || ||  || |  | |   ||    || ||       |      || |     |  |   ||||        || |  |          |   |  ||||  |   |  ||   | |  |               |  | 
      | | || ||  || |  | |   ||    || ||       |      || |     |  |   ||||        || |  |          |   |  ||||  |   |  ||   | |  |               |  | 
      | | ||/++--++-+--+-+---++\\   || ||       |      || |     |  |   ||||        || |  |          |   |  ||||  |   |  ||   | |  |               |  | 
     /+-+-+++++\\ || |  | |   |||   || ||       |      || |/----+--+---++++--------++-+--+----------+---+--++++--+---+--++---+-+-\\|               |  | 
 /---++-+-++++++-++-+--+-+---+++---++-++----->-+------++-++----+--+---++++--------++\\|  |    /-----+---+--++++--+---+--++---+-+-++---------------+\\ | 
 |   || | |||||| || \\--+-+---+++---++-++-------+------++-++----+--+---+/||        |\\++--+----+-----+---+--++++--+---+--++---+-+-++---------------/| | 
 |   || | |||||| ||    | |   |||   || ||     /-+------++-++----+--+---+-++--------+-++--+----+-----+---+--++++--+---+--++--\\|/+-++------------\\   | | 
 |   || | |||||| ||    | |   |||   || ||     | |  /---++-++----+-\\|  /+-++--------+-++--+----+-----+-\\ |  ||||  |   |  ||  |||| ||            |   | | 
 |   || | |||||| ||    | |   |||   || ||     | |  |   || ||   /+-++--++-++--\\     | ||/-+----+-----+-+-+--++++--+---+--++--++++-++--\\         |   | | 
 |   |\\-+-++++++-++----+-+---+++---++-++-----+-+--+---++-++---++-++--++-++--+-----+-+++-+----+-----/ | |  ||||  |   |  ||  |||| ||  |         |   | | 
 |   |  | |||||| ||    | |   |||   || ||     | |  |   || ||   || ||  || ||  |     | ||| |    |       | |  ||||  |   | /++--++++-++--+--------\\|   | | 
 |   |  | |||||| ||    | |   |||   || ||  /--+-+--+---++-++---++-++--++-++--+-----+-+++-+----+--\\    | |  ||||  |   | |||  |||| ||  |        ||   | | 
 |   |  | |\\++++-++----+-+---+++---++-++--+--+-+--+---++-++---++-++--++-+/  |     | ||| |  /-+--+----+-+--++++--+---+-+++--++++-++--+----\\   ||   | | 
 |   |  | \\-++++-++----+-+---+++---++-++--+--+-+--+---++-++---++-++--++-+---+-----+-+/| |  | |  |/---+-+--++++--+---+-+++--++++-++-\\|    |   ||   | | 
 |   |  |   |||| \\+----+-+---+++---++-++--+--+-+--+---++-++---++-++--++-+---+-----+-+-+-+--+-+--++---+-+--+++/  |   | |||  |||| || ||    |   ||   | | 
 |   |  |   ||||  |    | |   |||   || ||  |  | |  |   || ||   || ||  || |   |     | | | |  | |  ||   | |  |||   |   | |||  |||| || ||    |   ||   | | 
 |   |  |   ||||  |    | |   |||  /++-++--+--+-+--+---++-++---++-++--++-+---+----\\| | | |  | |  ||   | |  |||   |   | |||  |||| || ||    |   ||   | | 
 |   |  \\---++++--+----+-+---/||  ||| ||  |  | |  |   || ||  /++-++--++-+\\  |    || | | |  | |  ||   | |  |||   |   | |||/-++++-++-++----+---++---+-+\\
 |   |      ||||  |    | |    ||  ||| ||  |  | |  |   || ||  ||| ||  || ||  |    || | | |  | |  ||   | | /+++---+---+\\|||| |||| || ||    |   ||   | ||
 |   |      ||||  |   /+-+----++--+++-++--+--+-+--+---++-++\\ ||| ||  || ||  | /--++-+-+-+-\\|/+--++---+-+-++++---+---++++++-++++-++-++----+---++--\\| ||
 |   \\------+++/  |   || |    ||  ||| ||/-+--+-+--+---++-+++-+++-++--++-++--+-+--++-+-+-+-++++--++---+-+-++++-\\ |   |||||| |||| || ||    |   ||  || ||
 |          |||   |   || |    ||  ||| ||| |  | |  |   || |||/+++-++--++-++--+-+--++-+-+-+-++++->++---+-+-++++-+-+---++++++-++++-++-++--\\ |   ||  || ||
 |          |||   |   || |    ||  ||| ||| \\--+-+--+---++-+++++++-++--++-++--+-+--++-+-+-+-++++--/|   | | |||| | |   |||||| |||| || ||  | |   ||  || ||
 |          |||  /+---++-+----++--+++-+++----+-+--+---++-+++++++-++--++-++--+-+--++-+-+-+-++++--\\|   | | |||| | |   |||||| |||| || ||  | |   ||  || ||
 |          |||  ||   || |    ||  ||| |||    | |  \\---++-+++++++-/|  || ||  | |  || | | | |\\++--++---+-+-++++-+-+---++++++-++++-++-++--+-/   ||  || ||
 |          |||  ||   || |    ||  ||v |||    | |      || |||||||  |  || ||  | |  || | | |/+-++--++---+-+-++++-+-+---++++++-++++-++-++-\\|     ||  || ||
 |          |||/-++---++-+----++--+++-+++----+-+------++-+++++++--+--++-++--+-+--++-+-+-+++-++--++---+-+-++++-+-+--\\|||||| ||||/++-++-++-\\   ||  || ||
 |          |||| ||   || |   /++--+++-+++----+-+------++-+++++++-\\|  \\+-++--+-+--++-+-+-+++-++--++---/ | |||| | |  ||||||| ||||||| || || |   ||  || ||
 |          |||| ||   || |   |||  ||| \\++----+-+------++-+++++++-++---+-++--+-+--++-+-+-+++-++--++-----+-++++-+-+--+++++++-+++/||| || || |   ||  || ||
 |          |||| ||   || |   |||  |||  ||    | |      || ||||||| ||   | ||  | |  || | | ||| ||  ||     | |||| | |  ||||||| ||| ||| || || |   ||  || ||
 |          |||| || /-++-+---+++--+++--++----+-+------++-+++++++-++---+-++--+-+--++-+-+-+++-++--++-----+-++++-+-+-\\||||||| ||| ||| || || |   ||  || ||
 |          |||| || | || |   |||  |||  ||    |/+------++-+++++++-++---+-++--+\\|  || | | ||| ||/-++-----+-++++-+-+-++++++++-+++-+++\\|| || |   ||  || ||
 |  /-------++++-++-+-++-+---+++--+++--++----+++------++-+++++++-++-\\ | ||  |||  || | | ||| ||| ||     | |||| | | |||||||| ||| |||||| || |   ||  || ||
 |  |       |||| || | || |   |||  |||  ||    |||      || ||||||| || | | ||  |||  || | | ||| ||| ||     | |||| | | |||||||| ||| |||||| || |   ||  || ||
 |  |       |||| || | || |   |||  |||  ||    |||      || ||||||| || | | ||  |||  || | | ||| ||| ||     | |||| | |/++++++++-+++-++++++-++-+-\\ ||  || ||
 |  |       |||| || | || |   |||  |||  ||    |||      || ||||||| || | ^ ||  |||  || | | ||| ||| ||     | |||| | |||||||||| ||| |||||| || | | ||  || ||
 |  |       |||| || | ||/+---+++--+++--++----+++\\     || ||||||| || | | ||  |||  || | | |^| ||| |\\-----+-++++-+-++++++++++-+++-++++/| || | | ||  || ||
 |  |       |||| || | ||||   |||  ||\\--++----++++-----+/ ||||||| || | | ||  |||  || | | ||| ||| |      | |||| | |||||||||| ||| |||| | || | | ||  || ||
 |  |  /----++++-++-+-++++---+++--++---++----++++-----+--+++++++-++-+-+-++--+++\\ || | | ||| ||| |      | |||| | |||||||||\\-+++-++++-+-++-+-+-++--++-+/
 |  |  |    |||| || | ||||   |||  ||   ||    ||||     |  ||||||| || | | ||  |||| || | \\-+++-+++-+------+-++++-+-+++++++++--+++-++++-/ || | | ||  || | 
 |  |  |    |||| || | ||||   |||  ||   ||    ||||     |  ||||||| || |/+-++--++++-++-+---+++-+++-+------+-++++-+-+++++++++--+++\\||||   || | | ||  || | 
 |  |  |    |||| |\\-+-++++---+++--++---++----++++-----+--/|||||| || ||| ||  |||| || |   ||| ||| |      | |||| | |||||||||  |^||||||   || | | ||  || | 
 |  |  |    |||| |  | ||||   |||/-++---++----++++-----+---++++++-++-+++-++--++++-++-+---+++-+++-+----\\ | |||| | |||||||||  ||||||||   || | | ||  || | 
 |  |  |    |||| |  | ||||   |||| ||   ||    ||||     |   |||||| || ||| ||  |||| || |   ||| \\++-+----+-+-++++-+-+++++++++--++++++++---++-+-+-++--/| | 
 |  |  |  /-++++-+--+-++++---++++-++---++----++++-----+---++++++-++-+++-++-\\|||| || |   |||  || |    | | |||| | |||||||||  ||||||||   || | | ||   | | 
 |  |  |  | |||| |  | ||||   |||| ||   ||    ||||     |   |||||| || ||| || ||||| || |   |||  || |    | | |||| | \\++++++++--++++++/|   || | | ||   | | 
 |  |  |  | |||| |  | ||||   |||| ||   ||    ||||     |  /++++++-++-+++-++-+++++-++-+---+++--++\\|    | | |||| |  ||||||||  |||||| |   || | | ||   | | 
 |  |  |  | |||| |  | ||||   |||| ||   ||    ||||     |  |||||||/++-+++-++-+++++-++-+---+++--++++----+-+\\|||| |  ||||||||  |||||| |   || | | ||   | | 
 |  |  | /+-++++-+--+-++++---++++-++---++----++++-----+--++++++++++-+++-++-+++++-++-+-\\ |||  ||||    | |||||| |  ||||||||  |||||| |   || | | ||   | | 
 |  |  | || |||| |  | ||||   |||| ||   \\+----++++-----+--++++++++++-+++-++-+++++-++-+-+-+++--++++----+-++++++-+--++++++/|  |||||| |   || | | ||   | | 
 |  |  | || |\\++-+--+-+/||   |||| ||    |    ||||     |  |||||||||| ||\\-++-+++++-++-+-+-+++--++++----+-++++/| |  |||||| |  |||||| |   || | | ||   | | 
 |  |  | || | || |  | | ||   |||| ||    |    ||||     |  |||||\\++++-++--++-+/||| || | |/+++--++++----+-++++-+-+--++++++-+--++++++-+-\\ || | | ||   | | 
 |/-+--+-++-+-++-+--+-+\\||   |||| ||    |    ||||     |  ||||| |||| ||  || | ||| || | |||||  ||||    | |||\\-+-+--++++++-+--++++++-+-+-++-+-+-++---+-/ 
 || |  | || | || |  | ||||   |||| ||    |    ||||     |  ||||| |||| ||  || | ||| || | |||||  ||||    | |||/-+-+--++++++-+--++++++-+-+-++-+\\| ||   |   
 || |  | || | || |  | ||||   |||| ||    | /--++++-----+--+++++-++++-++--++-+-+++-++-+-+++++--++++----+-++++-+-+--++++++-+--++++++-+-+-++-+++-++---+--\\
 || |  | || | || |  | \\+++---++++-++----+-+--++++-----+--++/|| |||| ||  || | |\\+-++-+-++++/  ||||    | |||| | |  |||||| |  |||||| | | || ||| ||   |  |
 || |  | || | || |  |  |||   |||| ||    | |  ||||   /-+--++-++-++++-++--++-+-+-+-++-+-++++---++++----+-++++-+-+--++++++-+-\\|||||| | | || ||| ||   |  |
 || |  | || \\-++-+--+--+++---++/| ||    | |  ||||   | |  || || |||| ||  || | | | || | ||||  /++++----+-++++-+-+--++++++-+-+++++++-+-+-++\\||| ||   |  |
 || |  | ||   || |  |  |||   || | |\\----+-+--++++---+-+--++-++-++++-++--/| | | | ||/+-++++--+++++----+-++++-+-+--++++++-+-+++++++\\| | |||||| ||   |  |
 || |  | ||   || |  |  |||   || | |     | |  ||||   | |  || || |||| ||   | | | | |||| ||||  |||||    | |||| | |  |||||| | ||||||||| | |||||| ||   |  |
 || |  | ||   || |  \\--+++---++-+-+-----+-+--++++---+-+--++-++-++++-++---+-+-+-+-++++-++++--+++++----+-++++-+-+--+/|||| | ||||||||| | |||||| ||   |  |
 || |  | ||  /++-+-----+++\\  || | \\-----+-+--++++---+-+--++-++-++++-++---+-+-+-+-/||| ||||  |||||    | |||| | |  | |||| | ||||||||| | |||||| ||   |  |
 || |  | ||  |||/+-----++++--++-+----\\  | |  ||||   | |  || || |||| ||   | | | |  ||| ||||  |||||    | |||| | |  | |||\\-+-+++++++++-+-++++++-/|   |  |
 || |  | ||  ||\\++-----++++--++-+----+--+-+--++++---+-+--++-++-++++-++---+-+-+-+--+++-++++--+++++----+-++++-+-+--+-/||  | ||||||||| | ||||||  |   |  |
 || |  ^ ||  || ||     |||| /++-+----+--+-+\\ ||||   | |  || || |||| ||   | | | |  ||| ||||  ||\\++----+-++++-+-+--+--++--+-++++++++/ | ||||||  |   |  |
 || |  | ||  || ||     |||| ||| |    |  | || ||||   \\-+--++-++-++++-++---+-+-+-+--+++-++++--++-++----+-++++-+-+--+--++--+-/|||||||  | ||||||  |   |  |
 || |  | ||/-++-++-----++++\\||| |    |  | || ||||     |  || || |||| |\\---+-+-+-+--+++-++++--++-++----+-++++-+-+--+--++--+--+++/|||  | ||||||  |   |  |
 || |  | ||| |\\-++-----++++++++-+----+<-+-++-++++-----/  || || |||| |    | | | |  ||| ||||  || ||    | ||\\+-+-+--+--+/  |  ||| |||  | v|||||  |   |  |
 \\+-+--+-+++-+--++-----++++++++-+----+--+-++-++++--------++-++-++++-+----+-+-+-+--++/ ||||  || ||/---+-++-+-+-+--+--+---+--+++-+++--+-++++++\\ |   |  |
  | |  | ||| |  ||     |||||||| |    |  | || ||||        || || |||| |    | | | |  |\\--++++--++-+++---+-++-+-+-+--+--+---+--+++-++/  | ||||||| |   |  |
  | | /+-+++-+--++-----++++++++-+----+--+-++-++++----\\   || || \\+++-+----+-+-+-+--+---++++--++-+++---+-++-+-+-+--+--+---+--+/| ||   | ||||||| |   |  |
  | | || |||/+--++-----++++++++-+----+--+-++\\||||    |   || \\+--+++-+----+-+-+-+--+---++++--++-+++---+-++-+-+-+--+--+---+--+-+-++---+-+/||||| |   |  |
/-+-+-++-+++++--++-----++++++++-+----+--+-+++++++----+---++-\\|  \\++-+----+-+-+-+--+---++++--++-+++---+-+/ | | |  |  |   |  | | ||   | | ||||| |   |  |
| | | || |||||  ||     |||||||| |    |  | |||||||    |   || ||   || |    | | | |  \\---++++--++-+++---+-+--+-+-+--+--+---/  | | ||   | | ||||| |   |  |
| | | || |||||  ||     |||||||| |    |  | |||||||    |   || ||   ||/+----+-+-+-+------++++-\\|\\-+++---+-+--+-+-+--+--+------+-+-++---+-+-+++++-+---/  |
| | | || |||||  ||     |||||||| |    |  | |||||||/---+---++-++---++++----+-+-+-+------++++-++--+++---+-+--+-+-+--+--+------+-+-++->-+-+-+++++-+\\     |
| | | || |||||  ||     |||||||| |    |  | ||||||||   |   || ||   ||||    |/+-+-+------++++-++--+++---+-+--+-+-+--+--+------+-+-++---+-+\\||||| ||     |
| | | || |||||/-++-----++++++++-+----+--+-++++++++---+---++-++---++++-\\  ||| |/+------++++-++--+++---+-+--+-+-+--+--+------+-+-++---+-+++++++-++----\\|
| | | || |||||| ||     |||||||| |    |  | ||||||||   |   || ||   |||| |  ||| |||      |||| ||  |||   | |  | | |  |  |      | | ||   | ||||||| ||    ||
| | \\-++-++++++-++-----++++++++-+----+--+-++++++++---+---++-++---+++/ |  ||| |||      |||| ||  |||   | |  | | |  |  |      | | ||   | ||||||| ||    ||
| |   || |||||| ||     |||||||\\-+----+--+-++++++++---+---++-++---+++--+--+++-+++------++++-++--+++---+-/  | | |  |  |      | | ||   | ||||||| ||    ||
| |   || |||||| ||     |||||||  |    |  | \\+++++++---+---++-++---+++--+--+++-+++------++++-++--+++---+----+-+-+--+--+------+-+-++---+-+++++++-++----+/
| |   || |||||| ||     |||||||  |    |  |  |||||||   |   || ||   |||  |  ||| |||      |||| ||  |||   |    | | |  |  |      | | ||   | ||||||| ||    | 
| |   || |||||| ||     |||||||  |    |  |  |||||||   |   || ||   |||  |  ||| ||| /----++++-++--+++---+----+-+-+--+--+------+-+-++--\\| ||||||| ||    | 
| |   || |||||| ||     |||||||  |    |  |  |||\\+++---+---++-++---+++--+--+++-/|| |    |||| ||  |||   |    | | |  |  |      | | ||  || ||||||| ||    | 
| |   || |||||| ||     |||||||  |    |  |  ||| |||   |   |\\-++---+++--+--+++--++-+----++++-++--+++---+----+-+-+--+--+------+-+-+/  || ||||||| ||    | 
| |   || |||||| ||     |||||||  |    |  |  ||| |||   |   |  ||   |||  |  |||  || |    |||| ||  |||  /+----+-+-+--+--+------+-+-+--\\|| ||||||| ||    | 
| |   || |||||| ||     |||||||  |    |  |  ||| |||   |   \\--++---+++--+--+++--++-+----++++-++--/||  ||    | | |  |  |  /---+-+-+--+++-+++++++-++\\   | 
| |   \\+-++++++-++-----+++++++--+----+--+--+++-+++---/      ||   |||  |/-+++--++-+----++++-++---++-\\||    | | |  |  |  |   | | \\--+++-+++/||| |||   | 
| |    | |||||| ||     |||||||  |    |  |  ||| |||          ||   |||  || |||  || |    v||| ||   || |||   /+-+-+--+--+--+---+-+\\   ||| ||| ||| |||   | 
| |    | |||||| ||     |||||||  |    |  \\--+++-+++----------++---+++--++-+++--++-+----++++-++---++-+++---++-+-/  |  |  |   | ||   ||| ||| ||| |||   | 
| \\----+-++++++-++-----/|\\++++--+----+-----+++-+++----------++---+/|  || |||  || |    ||||/++---++-+++---++-+---\\|  |  |   | ||   ||| ||| ||| |||   | 
|      | |||||| \\+------+-++++--+----/     ||| |||          ||   | |  || |||  || |    |||||||   || |||   || |   ||  |  |   | ||   ||| ||| ||| |||   | 
|      | ||||||  |      | ||||  |          ||| |||          ||   | |  || |||  || |    |||||||   || |||   || |   ||  |  |   | ||   ||| ||| ||| |||   | 
|      | ||||||  |    /-+-++++--+------\\   ||| ||v          ||   | |  || |||  || |    |||||||   |\\-+++---++-+---++--+--+---+-++---+++-+++-++/ |||   | 
|      | ||||||  |    | | ||||  |      |   ||| |||          ||   | |  || |||  || |    ||\\++++---+--+++---++-/   ||  |  |   | ||   ||| ||| ||  |||   | 
|      | ||||||  |    | | ||||  |      |   ||| |||          ||   | |  || |||  \\+-+----++-++++---+--+++---++-----++--+--+---+-++---+++-+++-++--+++---/ 
|      \\-++++++--+----+-+-++++--+------+---+++-+++----------++---+-+--++-+++---/ |    || ||||   |  |||   ||     ||  |  |   | ||   ||| ||| ||  |||     
|        ||||||  |    | | ||||  |      |   ||\\-+++----------++---+-+--++-+++-----+----++-++++---+--+++---++-----++--+--+---/ \\+---+++-+++-++--/||     
|     /--++++++--+----+-+-++++--+------+---++--+++\\         |\\---+-+--++-/||     |    || ||||   |  |||   \\+-----++--+--+------/   ||| ||| ||   ||     
|     |  ||||||  |    | | ||||  |      |   ||  ||||         |    | |  ||  ||     |    || ||||   |  |||    |     ||  |  |          ||| ||| ||   ||     
|     |  \\+++++--+----+-+-++++--+------+---++--++++---------+----+-+--++--++-----+----/| ||||   |  |||    \\-----++--+--+----------+++-+++-/|   ||     
|     |/--+++++--+----+-+-++++--+------+---++-\\\\+++---------+----+-+--++--++-----+-----+-++++---+--+++----------++--/  |          ||| |||  |   ||     
|     ||  |||||  |    | | ||||  \\------+---++-+-+++---------+----+-+--++--++-----+-----+-++++---+--++/          ||     |          ||| |||  |   ||     
|     ||  |||||  |    | | ||||         |   || | |||         |    | |  ||  ||     |     | ||||   |  ||           ||     |          ||| |||  |   ||     
|     ||  |||||  |    | | ||||         |   || | |||         |    | |  ||  ||     |     | ||||   |  ||/----------++-----+------\\   ||| |||  |   ||     
|     ||  |||||  |    | | ||||         |   || | |||         |    | \\--++--++-----+-----+-++/|   |  |||          ||     |      |   ||| |||  |   ||     
|/----++--+++++--+----+-+-++++---------+---++-+-+++---------+----+----++--++-----+\\    | \\+-+---+--+++----------++-----+------+---+++-/||  |   ||     
||    ||  |||||  |    \\-+-++++---------/   || | |||         |    |    ||  ||     ||    |  | |   |  |||          ||     |      |   |||  ||  |   ||     
||    ||  |||||  |      | ||||             || | |||         |    |    ||  ||     ||    |  | |   |  |||          ||     \\------+---+++--++--+---+/     
||    ||  |||||  |      | ||\\+-------------/| | |||         |    |    ||  || /---++----+--+-+---+--+++---\\      ||            |   |||  ||  |   |      
||    |\\--+++++--+------+-++-+--------------+-/ |||         |    |    ||  || |   ||    |  | |   |  |||   |      ||            |   |||  ||  |   |      
||    |   |||||  |      | || |  /-----------+\\  |\\+---------+----+----++--++-+---++----+--+-+---+--+++---+------++------------+---+++--++--+---/      
||    |   |\\+++--+------+-+/ |  |   /-------++--+-+--\\      |    |    ||  || |   ||    |  | |   |  |||   |      ||            |   |||  ||  |          
||    |  /+-+++--+------+-+--+--+---+-------++--+-+--+------+----+----++--++-+---++----+--+-+---+--+++\\  |      ||            |   |||  ||  |          
||    |  || |||  |      | |  |  |   |       ||  | |  |      |    |    ||  || |   ||    |  | |   |  ||\\+--+------++------------/   |||  |^  |          
||    |  || |||  |      | |  \\--+---+-------++--+-+--+------+----/    ||  || |   ||    |  | |   |  || |  |      ||                |||  ||  |          
||    |  || |||  |      | |     |   |       ||  | |  |      |         ||  || |   ||    |  | |   |  || |  |      ||                |||  ||  |          
||    \\--++-+++--+------+-+-----+---+-------++--+-/  |      |         ||  || |   ||    \\--+-+---+--++-+--+------++----------------++/  ||  |          
||       || |||  |      \\-+-----+---+-------++--/    |      |         ||  || |   ||       | |   |  || |  |      |\\----------------++---++->/          
||       || ||\\--+--------+-----+---+-------++-------+------+-<-------/|  || |   ||       | \\---+--++-+--+------+-----------------++---+/             
||       |\\-++---+--------+-----+---+-------++-------+------+----------+--+/ |   ||       |     |  || |  |      |                 ||   |              
\\+-------+--++>--+--------+-----+---+-------++-------+------/          |  |  \\---++-------+-----+--++-+--/      |                 ||   |              
 |       |  ||   \\--------+-----+---+-------++-------+-----------------+--+------++-------+-----/  || |         |                 ||   |              
 |       |  \\+------------+-----+---+-------/|       |                 |  \\------++-------+--------++-+---------+-----------------++---/              
 |       |   \\------------/     \\---+--------/       |                 |         \\+-------+--------++-+---------+-----------------+/                  
 |       |                          |                |                 \\----------+-------+--------/| |         |                 |                   
 \\-------+--------------------------+----------------+----------------------------/       |         \\-+---------+-----------------/                   
         \\--------------------------+----------------+------------------------------------+-----------/         |                                     
                                    \\----------------/                                    \\---------------------/                                     """

initMine : DataSource -> Array2D Char
initMine dataSource = 
  let 
    data = 
      case dataSource of 
        Sample -> sample 
        Sample2 -> sample2
        Input -> input 
  in 
    data |> String.split "\n" |> List.map (String.toList) |> Array2D.fromList

toIndexedList : Array2D Char -> List (Pos, Char)
toIndexedList mine = 
  let 
    yRange = List.range 0 (Array2D.rows mine - 1)
    xRange = List.range 0 (Array2D.columns mine - 1)
  in 
    yRange |> List.concatMap (\y -> xRange |> List.map (\x -> ((x, y), Array2D.get y x mine |> Maybe.withDefault ' ')))

isCart : Char -> Bool 
isCart ch = 
  let 
    symbols = ['^', '<', 'v', '>']
  in 
    symbols |> List.member ch

isBlank : Char -> Bool 
isBlank ch = 
  ch == ' '

isCrash : Char -> Bool 
isCrash ch = 
  ch == 'X'

charToDir : Char -> Dir 
charToDir ch = 
  case ch of 
    '^' -> N
    '<' -> W
    'v' -> S
    '>' -> E
    _ -> N

dirToChar : Dir -> Char 
dirToChar dir  = 
  case dir of 
    N -> '^'
    W -> '<'
    S -> 'v'
    E -> '>'

dirToMineSymbol : Dir -> Char 
dirToMineSymbol dir  = 
  case dir of 
    N -> '|'
    W -> '-'
    S -> '|'
    E -> '-'

findCarts : Array2D Char -> List Cart
findCarts mine = 
  let 
    indexed = toIndexedList mine 
  in 
    indexed |> List.filterMap (\(pos, ch) -> if isCart ch then Just { dir = charToDir ch, pos = pos, switches = Left } else Nothing)

withoutCarts : List Cart -> Array2D Char -> Array2D Char 
withoutCarts carts mine = 
  case carts of 
    [] -> mine 
    c :: rest -> 
      let 
        (x, y) = c.pos 
        symbol = dirToMineSymbol c.dir 
      in 
        withoutCarts rest (Array2D.set y x symbol mine)

initModel : DataSource -> Bool -> Model 
initModel dataSource removeCrashedCarts = 
  let 
    mine = initMine dataSource
    carts = findCarts mine
  in 
    { mine = withoutCarts carts mine
    , carts = carts
    , dataSource = dataSource
    , removeCrashedCarts = removeCrashedCarts
    , steps = 0 
    , paused = True
    , finished = False 
    , tickInterval = defaultTickInterval
    , counter = 0
    , message = ""
    , debug = "" }

init : () -> (Model, Cmd Msg)
init _ =
  (initModel Input False, Cmd.none)

-- UPDATE

type Msg = 
  Tick 
  | Step 
  | TogglePlay 
  | ToggleRemovedCrashedCarts
  | Faster 
  | Slower 
  | Clear 
  | UseSample 
  | UseSample2
  | UseInput 

getAllPositions : Array2D Char -> List Pos
getAllPositions mine = 
  let
    ys = List.range 0 (Array2D.rows mine - 1)
    xs = List.range 0 (Array2D.columns mine - 1)
  in 
    ys |> List.concatMap (\y -> xs |> List.map (\x -> (x, y)))

getNestedPositions : Array2D Char -> List (List Pos)
getNestedPositions mine = 
  let
    ys = List.range 0 (Array2D.rows mine - 1)
    xs = List.range 0 (Array2D.columns mine - 1)
  in 
    ys |> List.map (\y -> xs |> List.map (\x -> (x, y)))

getNextPos : Dir -> Pos -> Pos 
getNextPos dir (x, y) = 
  case dir of 
    N -> (x, y - 1)
    W -> (x - 1, y)
    S -> (x, y + 1)
    E -> (x + 1, y)

turnLeft : Dir -> Dir 
turnLeft dir = 
  case dir of 
    N -> W
    W -> S
    S -> E 
    E -> N

turnRight : Dir -> Dir 
turnRight dir = 
  case dir of 
    N -> E
    W -> N
    S -> W 
    E -> S

straightAhead : Dir -> Dir 
straightAhead dir = dir

turn : Junction -> (Dir -> Dir)
turn switches = 
  case switches of 
    Left -> turnLeft
    Forward -> straightAhead
    Right -> turnRight

getNextDir : Char -> Cart -> Dir 
getNextDir nextSymbol cart =
  let 
    dir = cart.dir 
    switches = cart.switches
  in 
    case nextSymbol of 
      '/' -> 
        case dir of 
          N -> turnRight dir
          W -> turnLeft dir 
          S -> turnRight dir 
          E -> turnLeft dir 
      '\\' ->
        case dir of 
          N -> turnLeft dir
          W -> turnRight dir 
          S -> turnLeft dir 
          E -> turnRight dir 
      '+' ->
        turn switches dir 
      _ -> 
        dir 

getNextSwitches : Char -> Junction -> Junction 
getNextSwitches nextSymbol switches = 
  case nextSymbol of 
    '+' -> 
      case switches of 
        Left -> Forward
        Forward -> Right 
        Right -> Left 
    _ -> 
      switches

moveCart : Array2D Char -> Cart -> Cart
moveCart mine cart = 
  let 
    (x, y) = getNextPos cart.dir cart.pos 
    nextSymbol = Array2D.get y x mine |> Maybe.withDefault ' '
    nextDir = getNextDir nextSymbol cart 
    nextSwitches = getNextSwitches nextSymbol cart.switches
  in 
    { dir = nextDir, pos = (x, y), switches = nextSwitches }

crashesWithAnotherCart : List Cart -> Cart -> Bool 
crashesWithAnotherCart carts cart = 
  let 
    count = carts |> List.filter (\c -> c.pos == cart.pos) |> List.length 
  in 
    count > 1

findCrashes : List Cart -> List Cart 
findCrashes carts =  
  carts |> List.filter (crashesWithAnotherCart carts) 

updateClear : Model -> Model
updateClear model = 
  initModel model.dataSource model.removeCrashedCarts

posToString : (Int, Int) -> String 
posToString (x, y) = 
  String.fromInt x ++ "," ++ String.fromInt y

updateStep : Model -> Model
updateStep model = 
  let 
    carts = model.carts 
    crashes = findCrashes carts  
    remaining = carts |> List.filter (\c -> List.member c crashes |> not)
    movedRemaining = remaining |> List.map (moveCart model.mine)
    steps = model.steps
  in 
    if model.removeCrashedCarts then 
        if List.length remaining > 1 then 
          { model | carts = movedRemaining, steps = steps + 1 }
        else 
          let 
            msg = 
              case remaining of 
                [] -> "No carts left!" 
                c :: _ -> 
                  let 
                    (x, y) = c.pos 
                  in 
                    "Last cart at " ++ String.fromInt x ++ "," ++ String.fromInt y
          in 
            { model | carts = movedRemaining, finished = True, message = msg }
    else 
      case crashes of 
        [] -> 
          { model | carts = movedRemaining, steps = steps + 1 }
        _ -> 
          let
            crashPositions = crashes |> List.map (\c -> c.pos) |> Set.fromList |> Set.toList |> List.sort 
            s = crashPositions |> List.map posToString |> String.join " "
            msg = "First crash at " ++ s
          in 
            { model | carts = carts, finished = True, message = msg }

updateTogglePlay : Model -> Model
updateTogglePlay model = 
  if model.finished then 
    let 
      m = initModel model.dataSource model.removeCrashedCarts
    in 
      {m | paused = False }
  else 
    { model | paused = not model.paused }

updateToggleRemoveCrashedCarts : Model -> Model
updateToggleRemoveCrashedCarts model = 
  let
    removeCrashedCarts = not model.removeCrashedCarts
  in
    initModel model.dataSource removeCrashedCarts

updateUseSample : Model -> Model
updateUseSample model = 
  initModel Sample model.removeCrashedCarts

updateUseSample2 : Model -> Model
updateUseSample2 model = 
  initModel Sample2 model.removeCrashedCarts

updateUseInput : Model -> Model
updateUseInput model = 
  initModel Input model.removeCrashedCarts

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
    ToggleRemovedCrashedCarts -> 
      (updateToggleRemoveCrashedCarts model, Cmd.none)
    UseSample -> 
      (updateUseSample model, Cmd.none)
    UseSample2 -> 
      (updateUseSample2 model, Cmd.none)
    UseInput -> 
      (updateUseInput model, Cmd.none)

-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
  let 
    tickSub = if model.paused || model.finished then Sub.none else Time.every model.tickInterval (\_ -> Tick)
  in 
    tickSub

-- VIEW

toCharElement : Array2D Char -> List Cart -> Pos -> Html Msg 
toCharElement mine carts (x, y) = 
  let 
    cartsAtPos = carts |> List.filter (\c -> c.pos == (x, y))
    symbol = 
      case cartsAtPos of 
        [] ->
          Array2D.get y x mine |> Maybe.withDefault ' '
        [c] -> 
          case c.dir of 
            N -> '^'
            W -> '<'
            S -> 'v'
            e -> '>'
        _ -> 'X'
  in  
    if isCart symbol then 
      Html.span [ Html.Attributes.style "color" "#2E8B57", Html.Attributes.style "background-color" "#CCCCCC" ] [ Html.text (String.fromChar symbol) ]
    else if isBlank symbol then 
      Html.span [ Html.Attributes.style "color" "#FFFFFF" ] [ Html.text "." ]
    else if isCrash symbol then 
      Html.span [ Html.Attributes.style "color" "#D2042D", Html.Attributes.style "background-color" "#CCCCCC" ] [ Html.text "X" ]
    else 
      Html.text (String.fromChar symbol)

view : Model -> Document Msg
view model = 
  { title = "Advent of Code 2018 | Day 13: Mine Cart Madness"
  , body = [ viewBody model ] }

viewBody : Model -> Html Msg
viewBody model =
  let
    mine = model.mine
    carts = model.carts 
    nestedPositions = getNestedPositions mine
    nestedElements = nestedPositions |> List.map (\positions -> positions |> List.map (toCharElement mine carts))
    elements = nestedElements |> List.foldr (\a b -> List.append a (Html.br [] [] :: b)) []      
    textFontSize = 
      case model.dataSource of 
        Sample -> "32px"
        Sample2 -> "32px"
        Input -> "9px"
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
              [ Html.div [] [Html.text "Advent of Code 2018" ]
              , Html.div [] [Html.text "Day 13: Mine Cart Madness" ] ] ]
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
                [ Html.Attributes.href "https://adventofcode.com/2018/day/13" ] 
                [ Html.text "https://adventofcode.com/2018/day/13" ]
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
              , Html.input 
                [ Html.Attributes.type_ "radio", onClick UseSample2, Html.Attributes.checked (model.dataSource == Sample2) ] 
                []
              , Html.label [] [ Html.text "Sample2" ]
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
              [ Html.Attributes.align "center" ]
              [ Html.input 
                [ Html.Attributes.type_ "checkbox", onClick ToggleRemovedCrashedCarts, Html.Attributes.checked model.removeCrashedCarts ] 
                []
              , Html.label [] [ Html.text " Remove crashed carts" ]
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
                Html.div [] [ Html.text ("Carts: " ++ String.fromInt (model.carts |> List.length)) ]
              , Html.div [] [ Html.text ("Steps: " ++ String.fromInt model.steps) ]
              , Html.div [] [ Html.text model.message ]
              ] ]
              ]
