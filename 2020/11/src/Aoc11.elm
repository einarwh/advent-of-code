module Aoc11 exposing (..)

import Browser exposing (Document)
import Html exposing (Html)
import Html.Attributes
import Html.Events exposing (onClick)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Task
import Time
import Array exposing (Array)

defaultTickInterval : Float
defaultTickInterval = 250

id : a -> a
id x = x

-- MAIN

main =
  Browser.document
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions }

-- MODEL

type DataSource = Sample | Input
type Strategy = Adjacent | Direction
type Status = Empty | Occupied
type Space = Seat Status | Floor
type alias Row = Array Space
type alias Layout = Array Row
type alias Point = (Int, Int)
type alias Move = Point -> Point
type alias Look = Move -> Layout -> Point -> Space
type alias Evolve = Layout -> Int -> Int -> Status -> Status

type alias Model =
  { layout : Layout
  , dataSource : DataSource
  , strategy : Strategy
  , evolver : Layout -> Layout
  , paused : Bool
  , finished : Bool
  , tickInterval : Float }

toSpace : Char -> Space
toSpace ch =
  case ch of
    'L' -> Seat Empty
    '#' -> Seat Occupied
    _ -> Floor

toSpaceRow : String -> Row
toSpaceRow s =
  s |> String.toList |> List.map toSpace |> Array.fromList

initModel : Strategy -> DataSource -> Model
initModel strategy dataSource =
  let
    samp = [ "L.LL.LL.LL"
           , "LLLLLLL.LL"
           , "L.L.L..L.."
           , "LLLL.LL.LL"
           , "L.LL.LL.LL"
           , "L.LLLLL.LL"
           , "..L.L....."
           , "LLLLLLLLLL"
           , "L.LLLLLL.L"
           , "L.LLLLL.LL" ]
    inpt = [ "LLLLL.LLL.LLLLLLLLLLLLLLL.LLLLLLLLL.LLLLLLLLLLLLL.LLLLLLL.LLLLLLL.LLLLL.LLLLLLLLLLLLLLLLLLL.LLLLL"
           , "LLLLLLLLL.LLLLLLLLLLLLLLL.LLLLL.LLLLLLLLLLLLLLLLLLLLLLLLL.LLLLLLL.LLLLL.LLLLLL.L.LLLL.LLL.L.LLLLL"
           , "LLLLLLLLL.LLLLLL.L.LLLLLL.LLLLLLLLL.LLLL...LLLLLL.LLLLLLL.LLLLLLL.LLLLLLLLLLLLLLLLLLL.LLLLL.LLLLL"
           , "LLLLLLLLLLLLLLLLLLLLLLLLL.LLLLLLLLLLLLLLLLLLLLLLL.LLLLLLL.LLLLLLLLLLLLL.L.LLLLLL.LLLLLLLLLLLLLLLL"
           , "LLLLLLLLLLLLLLL..LLLLLL.LLLLLLLLLLLLLLLLL..LLLLLL.LLLLLLL.LLLLLLL.LLLLL.LLLLLLLL.LLLL.LLLLLLLLLLL"
           , "LLLLLLLLLLLLLLLL.LLLLLLLL.LLLLLLLLL.LLLLL.LLLLLLL.LL.LLLL.LLLLLLL.LLLLL.LLLLLLLLLLLLL.LLLLLLLLLLL"
           , "LLLLLLLLLLLLLLLL.LLLLLLLL.LLLLLLLLLLLLLLL.LLLLLLL.LLLLLLLLLLLLLLL..LLLLLLLLLLLLL.LLLL.LLLLLLLLLLL"
           , "LLLLLLLLL.LLLLLLL.L.LLLLL.LLLLLLLLL.LLLLL.LLLLLLL.LLLLLLLLLLLLLLL.LLLLL.LLLLLLLLLLLLL.LLLLLLLLLLL"
           , "LLLLLLLLLLLLLLLL.LLLLLLLLLLLLLLLLLL.LLLLLLLLLLLLLLLLLLLLL.LLLLLLL.LLLLL.LLLLLLLL.LLLL.LLLLL.LLLLL"
           , "L.LL...L..L.L...L...L..L...L.....L....LL..L.LLLL..LL.LL..LLL.L..LL....L..L....L...LL..LL.......L."
           , "LLLLLLLLLLLLLLLL.LLLLLLLL.LLLLLLLLLLLLLLL.LLLLLLL.LLLLLLL.LLLLLLLLLLLLL.LLLLLLLL.LLLL..LLLL.LLLLL"
           , "LLLLLLLLL.LLLLLL.LLL.LLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLL.LLLLL.LLLLLLLLLLLLLLLLLLL.LLLLLLL.LLL"
           , "LLLLLLLLL.LLLLLL.LLLLLLLL.LLLLLLLLL.LLL.LLLLLLLLL.LLLLLLLLLLLLLLLLLLLLL.LLLLLLLL.LLLL.LLLLLL.LLLL"
           , "LLLLLLLLLLLLLLLL.LLLLLLLLLLLLLLLLLL.LLLLL.LLLLLLL.L.LLLLL.L.LLL.L.LLLLL.LLLLLLLLLLLLL.LLLLLLLLLLL"
           , "LL..LLL......LLL.L...L.....L.....L.L.L..L.L.L.LLL.L.L.LLL....L...LL..L..L.L.LL......L.....LL.L..."
           , "LLLLLLLLL.LLLLLL.LLLLLLL..LLLLLLLLLLLLL.LLLLLLLLL.LLLLLLL.LLLLLLL.LLLLL.LLLLL.LL.LLLL.LLLLL.LLLLL"
           , "LLLLLLL.L.LLLLLL.LLLLLLLL.LLLLLLLLL.LLL.L.LLLLLLLLLLLLLLL.LLLLLLL.LLLLL.LL.LLLLLLLLLLLLLLLLLLLLLL"
           , "LLLLLLLLLLLLLLLL.LLLLLL.LLLLLLLLLLL.LLLL.LLLLLLLL.LLLLLLLLLL.L..LLLLLLLLLLLLLLLL.LLLL.LLLLL.LLLLL"
           , "LLLLLLL.LLLLLLLLLLLLLLLLL.LLLLLLLLLLLLL.L.LLLLLLL.LLLLLLLLLLLLLLL.LLLLL.LLLLLLLL.LLLL.LLLLL.L.LLL"
           , "LLL.LLL.L.LL....L.....LL..L........L.LL.L...LLL.L...L.......L.L.L.L....LL...L.LLL........LL.....L"
           , "LLLLLLLLL.LL.LLLLL.LLLLLLLLLLLLLLLL.LLLLL.LLLLLLL..LLLLL..LLLLLLL.L.LLL.LLLLLLLL.LL.L.LLLLLLLLLLL"
           , "LLLLLLLLLLLLLLLLLL.LLLLLL.LLLL.LLLL.LLLLL.LLLLLLL.LLLL.LL.LLLLL.LLLLLLL..LLLLLLL.LLLLLLLLLL.LLLLL"
           , "LLLLLLLLL.LLLLLLLLLLLLLLL.LL.LLLL.LLLLLLL.LLLLLLLLLLLLLLL.LLLLLLL.LLLLLLLL.LLL.LLLLLL.LLLLL.LLLLL"
           , "L.LLLLLLL.LLLLLL.LL.LLLLL.LLLL.LLLLLLLLLL.LLLLLLLL.LLLLLLLLLLLLLLLLLLLL.LLLLLLLLLLLLL.LLLLL.LLLLL"
           , "LLLLLLLLL.LLLLLLLLLLLLLLLLLLLLLLLLL.LLLLL.LLLLLLL.LLLLLLL.LLLLLLL.LLLLL.LLL...L.LLLLL.LLLLL.LLLLL"
           , "LLLLLLLLL.LLLLLLLLLLLLL.L.LLL.LLLLL.LLLLLLLLLLLLL.LLLLLLLLLLLLLLL.LLLLL.LLLLLLLL.LLLL.LLLLL.LLLLL"
           , "LLLLLLLLL.LLLLLL.LLLLLLLL.LLLLLLLLL.LLLLLLLLLLLLL.LLLLLLL.LLLLLLL.LL.LLLLLLLLLLL.LLLL.LLLLL.LLLLL"
           , "LLLLLLLLL.LLLLLLLLLLLLLLLLLLLLLLLLLLL.L.LLLLLLLL..LLLLLLL.LLL.L.L.LLLLL.LL.LLLLL.LLLLLLLL.L.LLLLL"
           , "LLLLL.LLL.LLLLLLLL.LLLLLL.LLLLLLLLL.LLL.L.LLLLLLL.LLLLLLL.LLLLLLLLLLLLL.LLLLLLLL.LLLL.LLLLLLLLLLL"
           , ".L..LL...L....L..LL..LLLL......L.L..L.L.LL....L.L..L..L....L....LL..L.L..L.L.L..L..L.LL.L...L...."
           , "LLLLLLLLLLLLLLL.LLLLL.LLL.LLLLLLLLLLLLLLL.LLLLLLLLLLLLLLL.LLLLLLL.LLLLL..LLLLLLL.LLLL.LLL.L.LLL.L"
           , "LLLLLLLLLLL.LLLL.LLLLLLLL.LLLLLLLLLLLLL.LLLLLL.LLLLL.LLLLLLLLLLLLLLLLLL.LLLLLLLL.LLL..LLLLL.LLLLL"
           , "LLLLLLLLLLLLLLLL.LLLLLLL.LL.LLLLLLL.LLLLL.LLLL.LLLLLLLLLL.LLLLLLLLL.LLL.LLLLLLLLLLLLL.LLLLL.LLLLL"
           , "LLLLLLLLL.LLLLLL.LLLLLL.L.LLLLLLLLL.LLLLL.LLLLLLL.LLLLLLL.LLLLLLLLLLLLL..LLLLLLLLLLLLLLLLLLLLLLLL"
           , "LLLLLLLLL.LLLLLL.LLLLLLLL.LLLLLLLLL.LLLLLLLLLLLLL..LLLLLL.LLLLLLL.LLLLL.LLLLLLLL.LLLL.LLLLL.LLLLL"
           , "..L..L.L..L..LL..LLL.L..L..LL.L...LL.L.L..LL.L.LL.L....LLL..LLL..LLL.L.L...LL.LL.LL...L..L...L.LL"
           , "LLLLLLLLL.LLLLLLLLLLLLLLL.LLLLLLLLL.LLLLLLLL.LLLL.LLL.LLL.LLLLLLLLLLLLL.LLLLL.LL..LLL.LLLLL.L.LLL"
           , "LLLLLLLLLLL.LLLL.LLLLLLLLLLLLLLLLLL.LLLLL.LLLLLLL.LLLLLLLLLLLLLLLLL.LLL.LLL.LLLL.LLLLLLLL.L.LLLLL"
           , "LLLLLLLLL.LLLLLLLL.LLLLLL.LLLLLLLLL.LLLLL.LLLLL.LLLLLLLLL.L.LLLLLLLLLLL.LLLLLLLL.LLL.LL.LLL.LLLLL"
           , "LLLLLLLLL.LLLLLL.LLLLLLLL.LLLLLLLLLLLLLLL.LLLLLLL.LLLLLLLLLLLLLLL.LLLLL.LLLLLLLLLLL.LLLLLLL.LLLL."
           , ".LLLLLLLL.LLLLLL.LLLLLLLLLLLLLLLLLL.LLLL.LLLLLLLLLLLLLLLL.LLLLLLLLLLLLL.LLLLLLLLLLLLLLLLLLL.LLLLL"
           , "....L...L.....L..L...LL.L....L......L.....L..LL...L.......L....L.....L.L..L...L..........LL.LL..."
           , "LLLLL.LLL.LLLLLL.LLL.LLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLL.LLLLLL..LLLLL.LLLLLLLL.LLLLLLLLLLLLLLLL"
           , "LLLLLLLLL.LLLLLL.LLLLLLLL.LLLLLLLLLLLLLLL.LLLLLLL.LLLL.LL.LLLLLLL..LLLLLLLLLLLLL.LLLLL.LLLL.LLLLL"
           , "LLLLLLLLL.L.LLLLLLLLLLLLL.LLLLLLLLL.L..LLLLLLLLLL.LLLLLLLLLLLLL.L.LLLLLLLLLLLLLL.LLLL.LLLLL.LLLLL"
           , "LLLL.LL.L.LLLLLL.LLLLLLLL.LLLL.LLLL..LLLL.LLLLLLL..LLLLLLLLLLLLLL.LLLLLLLLLLLLLL.LLLL.LLLLLLLLLLL"
           , "LLLLLL.LLLLLLLLLLLLLL.LLL.LLLLLLLLLLLLLLL.LLLLLLLLLLLLLLL.LLLLLLL.LLLLL.LLLLLLLLLLLLLLLLLLLLLLLLL"
           , "L.LLLLLLLLLLLLLL.LLLLL.LL.LLLLLLLLLLLLLLLLLLLLLLL.LLLLLLL.LLLL.LL.LLLLLLLLLLLLLLLLLLL.LLLLL.LLLLL"
           , "LLLLLLLL..LLLLLL.LLLLLLLLLLLLLLLL.L.LLLLL.LLLLLLLLLLLLLLL.LLLLLLL..LLLLLLLLLLLLL.LLLL.LLLLL.LLLLL"
           , "L...LLL.L..LL..L.LLL.....L......L...LLL..L....LLL..L...L...........L..L..L...LL.LL.L...LL.LLLLLL."
           , ".LLLLLLLLLLLLLLL.LLLLLLLL.LLLLLLLLL.LLLLLLLL.LLLLLLLLLLLLLLLLLLLLLLLLLL.LLLLLLLL.LLLL.LLLLL.LLLLL"
           , "LLLLLLLLL.LLLLLL.LLLLLLLL.LLLLLLLLLLLLL....LLLLLL.LLLLLLLLLLLLLLLLLLLLLLLLLL.LLL.LLLL.LLLLLLLLLLL"
           , "LLLLLLLLLLLLLLLLLLLLLLLLL.LLLLLLLLLLLLLLL.LLLLLLL.LLLLLLL.L.LLLLL.LLLLL.LLLLL.LLLLLLLLLLLLL.LLLLL"
           , "LLL.LLLLLLLLLLLLLLLLLLLLLLLLLLLLLLL.LLLLL.LLLLLLL.LLLLLLLLLLL.LLL.LLLLL.LLLLLLLL.LLLL.LLLLL.LLLLL"
           , "LLLLLLLL..LLLLLL.LLLLLLLLLLLLLLLLL.LLLLLL.L.LLLL..LLLLLLL.LLLLLLL.LLLLL.LLL.LLLL.LLLL.LLLLLLLLLLL"
           , "LLLLLLLLLLLLLLLLLLLLLLLLL.LLLLLLLLL.LLLLL.LLLL.LL.LLLLLLLLLLLL.LLLLLLLL.LLLLLLLL.LLLL.LLL.LLLLLLL"
           , "LL.LLLLLL.L.LLLLLLLLLLL.L.LLLLLLLLL.LLLLLLLLLLLLL.L.LLLLL.LLLLLLLLLLLLL.LLLLLLLL.LLLL.L.LLLLLLLLL"
           , "....LLL.LL.....LLLLL...........LLLL.L..L.L..L.L.L...L.LL...LL.LLL.LL....LLLL....L.L.......L...L.L"
           , "LLLLLLLLLLLLLLLL.LLLLLLLLLLLLLLLLLL.LLLLL.LLLLLLL.LLLLLLL.LLLLLLLLLLLLL.LLLLLLLLLLLLL.LL.LL.LLLLL"
           , "LLLLLLLLLLLLLLLL.LLLLLLLL.LLLLLLLLLLLLLLL.LLLLLLL.LLLLLLLLLLLLLLL.LLLLL.LLLLLLLLLLLLLLLLLLLL.LLLL"
           , "LLLLLLLLLLLLLLLL.LLLLLLLLL.LLLLLLLL.LLLLL.LLLLLLL.LLLLLLLLLLLLLLLLLLLLL.LLLLLLLLLLL.L.LLLLL.LLLLL"
           , "LLLLL.LLL.LLLLLLLLLLLL.LLL.LLLLLL.L.LL.LL.LLLL.LL.LLLLLLL.LLLLLLLLL.LLLLLLLLLLLLLLLLL.LLLLLLLLLL."
           , "..L.L.LL........L.L...LL.LLL...L....LL......L.......LL.......L.L...L..LL.L..LL.L.L.L..L.........."
           , "LLLLLLLLL.LLLLLL.LLL.LLLL.LLLLLLLLL.LLLLL.LLLLLLLLLLLLLLLLLLLLLLL..LLLL.L.LLLLLL.LLLL.LLLLLLLLLLL"
           , "LLLLLLLLLLLLLLLL.LLLLLLLLLLLL.LLLLLLLLLLL.LLLLLLLLLLLLLLL.LLLLLLL.LL.LLLLLLLLLL..LLLL.L.LLL.LLLLL"
           , "L.LLLLLLL.LLLLLL.LLLLLLLL.LLLLLLLLL.LLLLL.LL.LLLL.LLLLLLLLLLLLLLLLLLLLL.LLLLLLLL.LLLLLLLLLL.LLLLL"
           , "LLLLLLLLLLLLLL.L.LLLLLLLL.LLLLLLLLL.LLLLL.LLLLL.L.LLLLLLL.LLLLLLLLLLLLL.LLLLLLLL.LLLL.LLLLL.LLLLL"
           , "LLLLLLLLLLLLLLLLLLLLLLL.L.LLLLLLLLLL.LLLL.LLLLLLL.LLLLLLL.L.LLLLL.LLLLL..LLLLLL..LLLL.LLLLL.LLLLL"
           , "LLLLLLLLLLLLLLLLLLLLLLLLLL.LLLLLLLL.LLLLL.LL.LLLLLLLLLLLLLLLLLLLLLLLLLL.LLLL.LLL.LLLL.LLLLL.LLLLL"
           , ".LLLLLLLL..LLLLLLLLLLLLLL....LLLL.L.LLLLL.LLLLLLLLLLLLLLL.LLLLLLL.LLLLLLLLLLLLLL.LLLL.LL.LL.LLLLL"
           , "LL.LLLLLL.LLL.LLLLLLL..LLLLLLLLLLLL.LLLLL.LLLLLLL.LLLLLLL.LLLLLLL..LLLL.LLLLLLLLLLLLL.LLLLL.LLLLL"
           , "LLLLLLLLL.LLLLLLLLLLLLLLL.LLLLLLLLL.LLLLL.L.LLLLL.LLLLLLL.LLLLLLL.LLLLL.LLLLLLLLLLLLL.LLLLL.LLLLL"
           , ".......L...L..L.L...L..LL..LL..L.....L..LL..L.L.L.....L.....L..L..LL.L..L..L.LL...LL..L.....L.L.."
           , "LLLLLLLLLLLLLLLL.LLLLLLLL.LLLLLLLLL.LLLLL.LLLLLLLLLLLLLLL.LLLLLLL.LLLLLLLL.LLLLL.LLLLLLLLLL.LLLLL"
           , "LLLLLLLLL.LLLLLL.LLLLLLLLLLLLLLLLLL.LLLLL.LLLLLLL.LLLLLLL.LLLL.LL.LLLLL.LLLLLLLLLLLLLLLLLLL.LLLLL"
           , "LLLLLLLLL.LLLLLLLLLLLLLLLLLLLLLLLLL.LLLLL.LLLLLLL.LLLLL.LLLLLLLLL.LLLLLLLLLLLLLL.LLLL.LLLLL..LLLL"
           , "LLLLLLLLLLLLLLLL.LLLLLLLLLLLLLLLLLL.LLLLLLLLLLLLL.LLLLLLLLLLLLLLL.LLLLLLLLLLLLLL.L.LL.LLLLL.L.LLL"
           , "LLLLLLLLL.LLLLLL.LLLLLLLLLLLLL.LLLL.LLLLL.LLLL.LLLLLLLLLLLLLLLLLL.L.LLL.LLLLLLLL.LLLL.LLLLLLLLLLL"
           , "L.LLLLLLLLLLL.LL.LLLLLLLLLLLLLLLLLL.LLLLLLL.LLLLLLLLLLLLLLLL.LLLL.LLLLL.L.LLLLLL.LLLLLLLLLL.LLLLL"
           , "LLLLLLLLL.LLLLLL.LLLL.LLL.LLLLLLLLL.LLLLL.LLLLLLL.LLLLLLL.LLLLLLL.LL.LLLLLLLLLLLLLLLLLLLLLL.LLLLL"
           , "LLLLLLLLL.LLLLLL.LLLL.LLLLLLLLLLLLL.LLLLL.LLLL.LL..LLL.LL.LL.LLLL.LLL.LLLLLLLLLL.LLLL.LLL.LLLLLLL"
           , "LLLL.LLL..LLLLLLLLLLLLLLL.LLLLLLLLL.LLLLL.LLLLLLL.LLLLLLL..LLLLLL.LLLLL.LLLLLLLL.LLLL.LLLLL..LL.L"
           , ".L......L....LLLL.......L.L...LL...L.L..L..L..LL..L.....L......LL..LL...L...L....L.L..L...LL.L.L."
           , "L.LLLLLLLLLLLLLL.LLLLLLLLLLLLLLL.LLLLLLLL.LLLL.LL.LLLLLLL.LLLLLLL.LLLLLLLLLLLLLL.LLLLLLLLLL.LLLLL"
           , "LLLLLLLLL.LLLLLL.LLLLLLLL.LLLLLLLLLLLLLLLLLLLLLLL.LLLLLLL.LLLLLLL.LLLLL.LLLLLLLL.LL.L.LL.LLLLLLLL"
           , "LLLL.LLLLLLLLLLL.LLLLLLLL.LL.LLLLL..LLLLL.LLLLLLL.L.LLLLL.LLLLLLL.LLLLL.LL.LLLLL.LLLLLLLLLL..LLLL"
           , "LLLLLLLLL.LLLLLL.LLLLLLLL.LLLLLL.LL.LLLLL.LLLLLLLLLLLLLLL.LLLLLLL...LLL.LLLLLLLL.LLLL.LLLLLLLLLLL"
           , "LLLLLLLLL.LLLLL.LLLLLLLLL.LLLLLLLLL.LLLLL.L.LLLLL.LLLLLLLLLLLLLLL.LLLLLLLLLLLL.L.LLLL.LLLLL.LLLLL"
           , "LLLL.LLLL.LLLLLL.LLLLLLLL.LLLLLLLLLLLLLLLLLLLLLLL.LLLLLLL.L.LLLLLLLLLLLLLLLLLLLL.LLLL..LLLL.LLLLL"
           , "LLLLLLLLL.LLLLLL.LLLLLLLL.LLL.LLLLL.LLLLLLLLLLLLL.LLLLLLLLLLLLLLL.LLLLL.LLLLLLLLLLLLLLLLLLL.LL.LL"
           , "LLLLLLLLL.LLLLLLLLLLLLLLL.LLLLLLLLLLLLLLLLLL.LLLL.LLLLLLL.LLLLLLL.LLLLL.LLLLLLLL.LLLL.LLLLLL.LLLL"
           , "LLLLLLLLLLLLLLLL.LLLLLLL..LLLLLLLLLLLLLLL.LLLLLLLLLLLLLLLL.LLLL.L..LLLL.L.LLLLL..LLLL.LLLLL.LLLLL" ]
    strs =
      case dataSource of
        Input -> inpt
        Sample -> samp
    layout = strs |> List.map toSpaceRow |> Array.fromList
  in
    { strategy = strategy
    , dataSource = dataSource
    , layout = layout
    , evolver = getEvolver strategy
    , paused = True
    , finished = False
    , tickInterval = defaultTickInterval }

init : () -> (Model, Cmd Msg)
init _ =
  (initModel Adjacent Input, Cmd.none)

-- UPDATE

type Msg = Tick | Clear | TogglePlay | Step | Faster | Slower | UseSample | UseInput | UseAdjacent | UseDirection

rowCount : Layout -> Int
rowCount = Array.length

colCount : Layout -> Int
colCount layout =
  layout
  |> Array.get 0
  |> Maybe.withDefault Array.empty
  |> Array.length

check : Layout -> Point -> Maybe Space
check layout (rowNo, colNo) =
  if rowNo >= 0 && rowNo < rowCount layout then
    let
      row = layout |> Array.get rowNo |> Maybe.withDefault Array.empty
    in
      if colNo >= 0 && colNo < colCount layout then
        row |> Array.get colNo
      else
        Nothing
  else
    Nothing

lookAt : Look
lookAt dir layout (rowNo, colNo) =
  check layout (dir (rowNo, colNo)) |> Maybe.withDefault Floor

lookDir : Look
lookDir dir layout xy =
  let
    next = dir xy
  in
    case check layout next of
      Nothing -> Floor
      Just space ->
        case space of
          Seat status -> Seat status
          Floor -> lookDir dir layout next

north : Move
north (x, y) = (x, y-1)

west : Move
west (x, y) = (x-1, y)

south : Move
south (x, y) = (x, y+1)

east : Move
east (x, y) = (x+1, y)

findPositions : Look -> Layout -> Int -> Int -> List Space
findPositions look layout rowNo colNo =
  [ look (north >> west) layout (rowNo, colNo)
  , look north layout (rowNo, colNo)
  , look (north >> east) layout (rowNo, colNo)
  , look west layout (rowNo, colNo)
  , look east layout (rowNo, colNo)
  , look (south >> west) layout (rowNo, colNo)
  , look south layout (rowNo, colNo)
  , look (south >> east) layout (rowNo, colNo) ]

spaceToStatus : Space -> Maybe Status
spaceToStatus space =
  case space of
    Seat status -> Just status
    Floor -> Nothing

findSeats : Look -> Layout -> Int -> Int -> List Status
findSeats look layout rowNo colNo =
  findPositions look layout rowNo colNo |> List.filterMap spaceToStatus

countSeat : Status -> Int
countSeat status =
  case status of
    Empty -> 0
    Occupied -> 1

countOccupiedSeats : List Status -> Int
countOccupiedSeats seats =
    seats
    |> List.map countSeat
    |> List.sum

countOccupiedSpaces : List Space -> Int
countOccupiedSpaces spaces =
  let
    count space =
      case space of
        Seat seat -> countSeat seat
        Floor -> 0
  in
    spaces
    |> List.map count
    |> List.sum

countOccupiedRow : Row -> Int
countOccupiedRow row =
  row
  |> Array.toList
  |> countOccupiedSpaces

countOccupiedTotal : Layout -> Int
countOccupiedTotal layout =
  layout
  |> Array.toList
  |> List.map countOccupiedRow
  |> List.sum

countOccupiedSeatsSeen : Look -> Layout -> Int -> Int -> Int
countOccupiedSeatsSeen look layout rowNo colNo =
  findSeats look layout rowNo colNo |> countOccupiedSeats

evolveSeatDirectNeighbors : Evolve
evolveSeatDirectNeighbors layout rowNo colNo seat =
  let
    occupied = countOccupiedSeatsSeen lookAt layout rowNo colNo
  in
    case seat of
      Empty -> if occupied == 0 then Occupied else Empty
      Occupied -> if occupied >= 4 then Empty else Occupied

evolveSeatInDirection : Evolve
evolveSeatInDirection layout rowNo colNo seat =
  let
    occupied = countOccupiedSeatsSeen lookDir layout rowNo colNo
  in
    case seat of
      Empty -> if occupied == 0 then Occupied else Empty
      Occupied -> if occupied >= 5 then Empty else Occupied

evolveSpace : Evolve -> Layout -> Int -> Int -> Space -> Space
evolveSpace evolveSeat layout rowNo colNo space =
  case space of
    Seat status -> Seat (evolveSeat layout rowNo colNo status)
    Floor -> Floor

evolveRow : Evolve -> Layout -> Int -> Row -> Row
evolveRow evolveSeat layout rowNo row =
  row |> Array.indexedMap (evolveSpace evolveSeat layout rowNo)

evolveLayout : Evolve -> Layout -> Layout
evolveLayout evolveSeat layout =
  layout |> Array.indexedMap (evolveRow evolveSeat layout)

getEvolver : Strategy -> Layout -> Layout
getEvolver strategy =
  case strategy of
    Adjacent -> evolveLayout evolveSeatDirectNeighbors
    Direction -> evolveLayout evolveSeatInDirection

updateClear : Model -> Model
updateClear model =
  initModel model.strategy model.dataSource

updateStep model =
  if model.finished then model 
  else 
    let
      evolvedLayout = model.evolver model.layout
      finished = evolvedLayout == model.layout
      paused = model.paused || finished
    in
      { model | layout = evolvedLayout, finished = finished, paused = paused }

updateTogglePlay model =
  if model.finished then 
    let 
      m = initModel model.strategy model.dataSource
    in 
      {m | paused = False }
  else 
    { model | paused = not model.paused }

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Tick ->
      (updateStep model, Cmd.none)
    Step ->
      (updateStep model, Cmd.none)
    Faster -> 
      ({model | tickInterval = model.tickInterval / 2 }, Cmd.none)
    Slower -> 
      ({model | tickInterval = model.tickInterval * 2 }, Cmd.none)
    Clear ->
      (updateClear model, Cmd.none)
    TogglePlay ->
      (updateTogglePlay model, Cmd.none)
    UseSample ->
      (initModel model.strategy Sample, Cmd.none)
    UseInput ->
      (initModel model.strategy Input, Cmd.none)
    UseAdjacent ->
      (initModel Adjacent model.dataSource, Cmd.none)
    UseDirection ->
      (initModel Direction model.dataSource, Cmd.none)

-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
  let
    tickSub = if model.paused then Sub.none else Time.every model.tickInterval (\_ -> Tick)
  in
    tickSub

-- VIEW

type alias Pos =
  { x : Int
  , y : Int }

type alias Circle =
  { x : Int
  , y : Int
  , r : Int
  , color : String }

toSeatColor : Status -> String
toSeatColor status =
  case status of
    Empty -> "#228B22"
    Occupied -> "#D70040"

toMaybeCircle : Int -> Pos -> Space -> Maybe Circle
toMaybeCircle radius pos space =
  case space of
    Seat seat ->
      let circle = { x = pos.x, y = pos.y, r = radius, color = toSeatColor seat }
      in
        Just circle
    Floor -> Nothing

toCircleList : Int -> Int -> List Space -> List Circle
toCircleList radius rowNo spaces =
  spaces
  |> List.indexedMap (\ix -> \space -> toMaybeCircle radius {x=ix*2*radius+radius,y=rowNo*2*radius+radius} space)
  |> List.filterMap id

toCircles : Int -> Layout -> List Circle
toCircles radius layout =
  layout
  |> Array.map Array.toList
  |> Array.toList
  |> List.indexedMap (toCircleList radius) |> List.concat

toCircleElement : Circle -> Svg msg
toCircleElement c =
  let
    xStr = String.fromInt c.x
    yStr = String.fromInt c.y
    rStr = String.fromInt c.r
    colorStr = c.color
  in
    circle [ cx xStr, cy yStr, r rStr, fill colorStr ] []

view : Model -> Document Msg
view model =
  { title = "Advent of Code 2020 | Day 11: Seating System"
  , body = [ viewBody model ] }

viewBody : Model -> Html Msg
viewBody model =
  let
    radius = 
      case model.dataSource of 
        Sample -> 10 
        Input -> 5
    circles = model.layout |> toCircles radius
    elements = circles |> List.map toCircleElement
    occupied = countOccupiedTotal model.layout
    countStr = occupied |> String.fromInt
    widthUnit = 2 * radius
    svgWidth = (rowCount model.layout * widthUnit) |> String.fromInt
    svgHeight = svgWidth
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
              , Html.div [] [Html.text "Day 11: Seating System" ] ] ]
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
                [ Html.Attributes.href "https://adventofcode.com/2020/day/11" ]
                [ text "https://adventofcode.com/2020/day/11" ]
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
                [ Html.text "Reset"]
              , Html.button 
                [ Html.Attributes.style "width" "80px", onClick Slower ] 
                [ text "Slower" ]
              , Html.button 
                [ Html.Attributes.style "width" "80px", onClick TogglePlay ] 
                [ if model.paused then text "Solve" else text "Pause" ] 
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
              , Html.Attributes.style "font-family" "Courier New"
              , Html.Attributes.style "font-size" "16px" ]
              [
                Html.input
                [ Html.Attributes.type_ "radio", onClick UseAdjacent, Html.Attributes.checked (model.strategy == Adjacent) ]
                []
              , Html.label [] [ Html.text "Adjacent" ]
              , Html.input
                [ Html.Attributes.type_ "radio", onClick UseDirection, Html.Attributes.checked (model.strategy == Direction) ]
                []
              , Html.label [] [ Html.text "Direction" ]
            ] ]
      , Html.tr
          []
          [ Html.td
              [ Html.Attributes.align "center"
              , Html.Attributes.style "font-family" "Courier New"
              , Html.Attributes.style "font-size" "32px"
              , Html.Attributes.style "padding" "10px"]
              [ text countStr ] ]
      , Html.tr
          []
          [ Html.td
              [ Html.Attributes.align "center"
              , Html.Attributes.style "background-color" "white"
              , Html.Attributes.style "padding" "10px"]
              [ svg [ width svgWidth, height svgHeight ] elements ] ] ]
