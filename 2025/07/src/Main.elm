module Main exposing (..)

{- Advent of Code 2025. Day 07: Laboratories. -}

import Browser 
import Html exposing (Html)
import Html.Attributes
import Html.Events exposing (onClick)
import Set exposing (Set)
import Dict exposing (Dict)
import Time

defaultTickInterval : Float
defaultTickInterval = 50

chamberWidth : Int 
chamberWidth = 7 

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

type alias State = 
  { count : Int 
  , seen : Set Pos }

type alias SplitState = 
  { beams : Set Int 
  , depth : Int  
  , lines : List (List Char)
  , seen : Set (Int, Int)
  , count : Int }

type alias CallInfo = 
  { pos : (Int, Int) 
  , lines : List (List Char) }

type StackItem = 
  Left CallInfo | Right CallInfo | Combine | Down CallInfo | Bubble

type alias TimelineState = 
  { mem : Dict (Int, Int) Int 
  , pos : (Int, Int) 
  , lines : List (List Char)
  , seen : Set (Int, Int)
  , count : Int 
  , stack : List StackItem }

type alias Model = 
  { dataSource : DataSource
  , lines : List (List Char)
  , splitState : SplitState 
  , timelineState : TimelineState 
  , quantum : Bool 
  , paused : Bool 
  , finished : Bool 
  , tickInterval : Float 
  , debug : String }

type alias ViewModel = 
  { seen : Set (Int, Int) 
  , count : Int }

sample : String
sample = """.......S.......
...............
.......^.......
...............
......^.^......
...............
.....^.^.^.....
...............
....^.^...^....
...............
...^.^...^.^...
...............
..^...^.....^..
...............
.^.^.^.^.^...^.
..............."""

input : String
input = """......................................................................S......................................................................
.............................................................................................................................................
......................................................................^......................................................................
.............................................................................................................................................
.....................................................................^.^.....................................................................
.............................................................................................................................................
....................................................................^...^....................................................................
.............................................................................................................................................
...................................................................^.^.^.^...................................................................
.............................................................................................................................................
..................................................................^.^.^...^..................................................................
.............................................................................................................................................
.................................................................^.^.^.^.^.^.................................................................
.............................................................................................................................................
................................................................^.^.^.^.^...^................................................................
.............................................................................................................................................
...............................................................^.^.^...^.^.^.^...............................................................
.............................................................................................................................................
..............................................................^.^.^.^.^.^...^.^..............................................................
.............................................................................................................................................
.............................................................^.^.......^...^.^.^.............................................................
.............................................................................................................................................
............................................................^.^.^.^.^...^.^.^.^.^............................................................
.............................................................................................................................................
...........................................................^.^.^.^.^.^.^.^...^.^.^...........................................................
.............................................................................................................................................
..........................................................^.....^.^.^.^.......^.^.^..........................................................
.............................................................................................................................................
.........................................................^...^.^.^.^.^...^.^.^.....^.........................................................
.............................................................................................................................................
........................................................^.....^.^.....^.^.^.^.....^.^........................................................
.............................................................................................................................................
.......................................................^...^.^.^.......^...^...^.^.^.^.......................................................
.............................................................................................................................................
......................................................^.^.^.^.^.^.^.^.^.^.^.^.^...^.^.^......................................................
.............................................................................................................................................
.....................................................^.^.^.^...^...^...........^.^.^.^.^.....................................................
.............................................................................................................................................
....................................................^.^...^.^.....^.^...^.^.^.^.^...^...^....................................................
.............................................................................................................................................
...................................................^.^...^...^...^...^.^.^.^.^...^.^.^.^.^...................................................
.............................................................................................................................................
..................................................^.....^.^.....^.^.^.^.^.^...^.^.....^.^.^..................................................
.............................................................................................................................................
.................................................^.^.^...^.^.^.....^.^.^.^.^.....^.^.^...^.^.................................................
.............................................................................................................................................
................................................^...^...^...^.^.^.........^.^.^.^.^.^.......^................................................
.............................................................................................................................................
...............................................^.^.^.^...^.^.^.......^.^.........^.^.^.^.....^...............................................
.............................................................................................................................................
..............................................^.^.^...^.^.^.^.^.^...^.^.....^.......^.^.^...^.^..............................................
.............................................................................................................................................
.............................................^.^.^.^.^.^...^.^.^.....^.^.^.^.^.^.^.....^.^.^.^.^.............................................
.............................................................................................................................................
............................................^.^.^...^.^.^.^.^.^.^...^.^.^.^...^.^.^.^.^.^...^.^.^............................................
.............................................................................................................................................
...........................................^.^.^.^.^.^.^.^.^.^...^.^.............^.^.^.........^.^...........................................
.............................................................................................................................................
..........................................^.^.^.....^.^.^.^...^.^.^.^.^.^...^.^.^.^.^.^...^.^.^...^..........................................
.............................................................................................................................................
.........................................^.^...^.^.^.^.^.^.......^.^...^.^.^.^...^...^.^.^...^.^.^.^.........................................
.............................................................................................................................................
........................................^.^.^.^.^.^...^...^.^.^.....^.^.^...^.^.^.^.^.^...^.^.^...^.^........................................
.............................................................................................................................................
.......................................^.^.^.^.^.^.^.^.^.^.^.^.^.^...^.^.^.^.......^...^.^...^.^.^...^.......................................
.............................................................................................................................................
......................................^.^.^.^...^.^...^.^.^.^.^.^.^.^.^.....^.^...^.^.....^.^.^...^...^......................................
.............................................................................................................................................
.....................................^.^.^.^.^.^.^.^...^.^.^...^.....^.^...^.^.^.^.....^.^...^...^.^.^.^.....................................
.............................................................................................................................................
....................................^...^.^.^.....^.^.^.^.^...^...^...^.^.^.^.^.^.^...^.^.^.^.^...^...^.^....................................
.............................................................................................................................................
...................................^...^...^...^.^.^.....^.^...^.^.^.^.^.....^.^.^.^.^.^.....^...^.^.^...^...................................
.............................................................................................................................................
..................................^.^.^.^.^...^.....^.....^.......^.^.....^.^...^...^.^.^...^.....^.^.^.^.^..................................
.............................................................................................................................................
.................................^.^.....^.^.^.^.......^.......^.^.^.^.^.^...^.^...^.^.^...^...^.^...^.....^.................................
.............................................................................................................................................
................................^.^.^.^.^...^.^.^...^.^.^.^.^.^.^.^...^.^.^.^.....^.^.^.....^.^.^.^...^.....^................................
.............................................................................................................................................
...............................^.^...^.^.^...^...^.^.....^.....^.^.^.^...^.....^...^...^...^.^.^.^.^.....^.^.^...............................
.............................................................................................................................................
..............................^.^.....^.^.^.^...^.^.^.^.^...^.^.^.^.^.^.^.^.^.^.^.^.......^.^.^.....^...^...^.^..............................
.............................................................................................................................................
.............................^.^.^.^.........^.....^.^.^...^...^.^.^.^.^...^...^.^.^.^.^.^.^...^.^.^.^...^.^...^.............................
.............................................................................................................................................
............................^.^.^.^.^.^.^.^...^.^.....^.^...^.^.^.^.^.....^.^.^.^...^.^.^.^.^...^...^...^...^.^.^............................
.............................................................................................................................................
...........................^.^...^.^.^.^.^.^...^.^.^.....^...^.^.^.^.^...^...^.^...^.^.......^.^.^...^.^.^.^.^.^.^...........................
.............................................................................................................................................
..........................^.^...^.^.^.^.^.^.^.........^.^...^.^.^.^.^.^.^.......^.^.^...^.^...^...^.^.^.^.^...^.^.^..........................
.............................................................................................................................................
.........................^.....^.^.^.^.^.^...^.^.^.^.....^.^.^...^.^.....^.^.^.^.^.^...^...^.^.....^.^.....^.^.^.^.^.........................
.............................................................................................................................................
........................^...^...^.^.^.^.^.^.....^.^.^.^.^.^.^.^.^.^.....^.^...^...^...^.^.^.^.^.^.......^.^.^.^.^...^........................
.............................................................................................................................................
.......................^.^.^...^.^.......^.^.^.^.^.^...^.^.......^.^.^...^.^...^.^.^.^.^...^.^.^.^...^.^.^.^.^.^.^.^.^.......................
.............................................................................................................................................
......................^.^.^.^...^.^.^.^.^.^.^...^.......^.^...^...^...^...^...^...^.....^...^.^.^.^...^.^.^...^.^.^.^.^......................
.............................................................................................................................................
.....................^.^.^...^.^...^.^...^.....^...^.^.^.^.....^.^...^.^.^.^.....^...^.......^.^.^...^.....^.^.^.....^.^.....................
.............................................................................................................................................
....................^.^.^.^.^.^.^.^...^.^.^.^.^.^.^.....^...^.^.^.^.^...^.^.^.^.^.^.^...^.^.......^...^.^.^.^.^.^.^.....^....................
.............................................................................................................................................
...................^.^.....^.....^...^.^...^...^...^.^.^...^.......^.^...^...^.^.^.^.^.^.^.^...^.^.^.^.^...^.^...^...^.^.^...................
.............................................................................................................................................
..................^...^.^.^.^.^.^.^.^.^.^.^.^.....^...^...^...^.^.^.^.^.^.^.^.^...^.....^...^...^.^.^.^.^.....^...^.^.^.^.^..................
.............................................................................................................................................
.................^.^.^.^.....^...^.^.^.^.^.^.^.^.^.^...^.^.^.^.^.^.^.....^.^.^...^.^.^...^.......^.....^.^...^...^.......^.^.................
.............................................................................................................................................
................^.^...^.....^.^...^.^...^.^.^...^.^.^.^...^.^.^.^.....^.^...^.^...^.^...^.^.....^.^.^.....^.^.^.^.^.^...^...^................
.............................................................................................................................................
...............^.^.....^.^.^.^.^.^.^.^...^.^...^.^...^.^.^...^.^.....^.^...^.^.^.^.^.........^.^...^.^.^...^.^.^.....^...^.^.^...............
.............................................................................................................................................
..............^.^.^.^.^.^.^.^.^.^.^...^.^.^.^.^.^.^.^.....^.^.^...^.^.^.^.^.^.^.^...^.^.^.^.^.^.^.^.........^.^.^.^.^...^.^.^.^..............
.............................................................................................................................................
.............^.^.^.^.^.^...^.^.^.^...^.^.^.^.^.........^.^...^.^.^.....^.^...^.^.^...^.^.^...^.^...^...^...^...^.^.^.^.^.^...^.^.............
.............................................................................................................................................
............^.^.....^.^...^.^.^.^.......^.^.^.^.^.....^.^.^.^.^.^.^.^.^.^.^...^.^.^.^.^...^.^.^...^.^...^.^.^...^.^.^.^.^.^.^...^............
.............................................................................................................................................
...........^.^.^...^.^.^.^.^...^...^.^.^.....^.^...^.^.^...^.^.^...^.^.^.^...^...^.^.....^...^.^.....^.^.^.^.^.^.^.^...^.......^.^...........
.............................................................................................................................................
..........^.^.^.^.^.^...^...^.^.^.^.^.^.....^.^.^.^.^...^...^.^.^...^.^.^.^.^...^.^.^.^.....^.^.^.^.^.^.^.^.^.^.^.....^.^.^.......^..........
.............................................................................................................................................
.........^.^.^.^.^...^.^...^...^.....^.^...^.^...^.^.^.^.^.^.........^.^.^.^.^...^.^.^...^.^.^.^.^...^.....^.^.^.^.^.^.^.^.^.^.^...^.........
.............................................................................................................................................
........^...^.....^.^.......^...^.^.^.^.....^.^.^.^.......^.^...^.^.^.^.^.^.^.^.^.^.^.....^...^.^.....^...^.^.^.^.^...^...^.....^.^.^........
.............................................................................................................................................
.......^.^.^.^.^.^...^.^.^...^.^...^...^.......^.^.^.^.^.^...^.^.^.^.^...^.^.^.^.^.^.^.^.^.^.^...^.^.^.^.^.^.^.^.^.....^.^.^.^...^.^.^.......
.............................................................................................................................................
......^.^.^.^.......^.^.^.^.....^.^.^...^.^.^.^.^...^...^...^.^.^.........^.....^.^.^.^.^.^.....^.......^.^.......^.....^.^.^.^.^.....^......
.............................................................................................................................................
.....^.^.^.^.....^.^.^...^...^.......^.^...^.....^.^.^.^.^...^...^.^.^.^...^.^.^.^.^.^.^.^.^...^.......^...^.........^.^.^.^...^.^...^.^.....
.............................................................................................................................................
....^.^.^.^.^.^.^.^.^.^...^.^...^.^.^...^.^.^.^...^.^...^.^.^.^...^...^.^.^.^.^.^.^...^.^.^...^.......^.^.^.^.^.^.^.^.^.^.^.^...^.^.^...^....
.............................................................................................................................................
...^...^.^.^.^...^.^...^.^.....^.^.^.^.^.^.^.^.^.....^.^.^...^...^.^.^.^.^...^...^.^.^.^...^.^.^.^.^...^...^.^.^.^.^...^.^.^.^.....^.....^...
.............................................................................................................................................
..^.^...^...^.^.^.^...^.^.^...^.^.^.^.^.....^...^...^.^.^.....^.^.....^.^.^...^.....^.^...^.^.^.^...^.......^.^.....^.^.......^.^.......^.^..
.............................................................................................................................................
.^...^.^.^.^.^.^.....^.^.^.........^.^...^.^.^.^...^.^.^.^...^.^.........^.^...^.....^...^.^.^.^.^.^.^...^...^.^.^.........^.^.^.^.^.^.^.^.^.
............................................................................................................................................."""

read : DataSource -> String
read dataSource = 
  case dataSource of 
    Input -> input
    Sample -> sample

defaultTimelineState : TimelineState 
defaultTimelineState = { mem = Dict.empty, pos = (0, 0), lines = [], seen = Set.empty, count = 0, stack = [] }

initTimelineState : List (List Char) -> TimelineState
initTimelineState lines = 
  case lines of 
    [] -> defaultTimelineState
    h :: t ->  
      case indexOf 0 'S' h of 
        Nothing -> defaultTimelineState
        Just ix -> 
          let 
            item = Down { pos = (ix, 0), lines = lines }
          in 
            { mem = Dict.empty 
            , pos = (ix, 0)
            , lines = lines
            , seen = Set.empty 
            , count = 0
            , stack = [ item, Bubble ] }

defaultSplitState : SplitState
defaultSplitState = { beams = Set.empty, depth = 0, lines = [], seen = Set.empty, count = 0 } 

initSplitState : List (List Char) -> SplitState 
initSplitState lines = 
  case lines of 
    [] -> defaultSplitState
    h :: t ->  
      case indexOf 0 'S' h of 
        Nothing -> defaultSplitState
        Just ix -> 
          let 
            beams = Set.empty |> Set.insert ix
          in 
            { beams = beams 
            , depth = 0
            , lines = lines
            , seen = Set.empty 
            , count = 0 }

initModel : Bool -> DataSource -> Model 
initModel quantum dataSource = 
  let 
    data = read dataSource
    lines = data |> String.split "\n" |> List.map String.toList
    splitState = initSplitState lines 
    timelineState = initTimelineState lines
  in 
    { dataSource = dataSource
    , lines = lines 
    , splitState = splitState 
    , timelineState = timelineState
    , quantum = quantum
    , paused = True
    , finished = False  
    , tickInterval = defaultTickInterval 
    , debug = "?" }

init : () -> (Model, Cmd Msg)
init _ =
  (initModel False Input, Cmd.none)

-- UPDATE

type Msg = 
  Tick 
  | Step 
  | TogglePlay 
  | ToggleQuantum
  | Faster 
  | Slower 
  | Clear 
  | UseInput
  | UseSample

getNestedPositions : Int -> Int -> List (List Pos)
getNestedPositions rows columns = 
  let
    ys = List.range 0 (rows - 1) 
    xs = List.range 0 (columns - 1)
  in 
    ys |> List.map (\y -> xs |> List.map (\x -> (x, y)))

updateClear : Model -> Model
updateClear model = 
  initModel model.quantum model.dataSource

splitStep : SplitState -> SplitState 
splitStep state = 
  case state.lines of
    [] -> state 
    h :: t -> 
      let 
        beams = state.beams 
        depth = state.depth 
        indexes = 
          h |> List.indexedMap (\i -> \c -> (i, c)) 
            |> List.filterMap (\(i, c) -> if c == '^' then Just i else Nothing)
        collisions = beams |> Set.filter (\b -> indexes |> List.member b)
        uncollisions = Set.diff beams collisions
        splitBeams = Set.union (collisions |> Set.map (\b -> b - 1)) (collisions |> Set.map (\b -> b + 1))
        nextBeams = uncollisions |> Set.union splitBeams 
        count = state.count + Set.size collisions 
        beamsPositions = beams |> Set.map (\b -> (b, depth))
        splitBeamsPositions = splitBeams |> Set.map (\b -> (b, depth))
        seen = beamsPositions |> Set.union splitBeamsPositions |> Set.union state.seen
      in 
        { beams = nextBeams, depth = depth + 1, lines = t, seen = seen, count = count }

updateSplitStep : Model -> Model
updateSplitStep model = 
  case model.splitState.lines of 
    [] -> { model | finished = True, paused = True, debug = "done split" }
    _ -> 
      let 
        state = splitStep model.splitState 
      in 
        { model | splitState = state, debug = "..." }

timelineStep : TimelineState -> TimelineState 
timelineStep state = 
  let 
    mem = state.mem 
    (beam, depth) = state.pos 
    lines = state.lines 
  in 
    case Dict.get (beam, depth) mem of 
      Just c -> 
        { state | count = state.count + c }
      Nothing -> 
        case lines of 
          [] -> 
            let 
              nextMem = state.mem |> Dict.insert (beam, depth) 1 
            in
              { state | count = state.count + 1, mem = nextMem }
          h :: t -> 
            let 
              indexes = 
                h |> List.indexedMap (\i -> \c -> (i, c)) 
                  |> List.filterMap (\(i, c) -> if c == '^' then Just i else Nothing)
            in 
              if indexes |> List.member beam then 
                let 
                  seen = state.seen |> Set.insert (beam, depth) |> Set.insert (beam - 1, depth) 
                  itemL = Left { pos = (beam - 1, depth + 1), lines = t }
                  itemR = Right { pos = (beam + 1, depth + 1), lines = t }
                  stack = itemL :: itemR :: Combine :: state.stack 
                in 
                  { state | seen = seen, stack = stack }
              else 
                let 
                  seen = state.seen |> Set.insert (beam, depth) 
                  item = Down { pos = (beam, depth + 1), lines = t }
                  stack = item :: Bubble :: state.stack 
                in 
                  { state | seen = seen, pos = (beam, depth + 1), stack = stack }

updateTimelineState : TimelineState -> StackItem -> List StackItem -> TimelineState
updateTimelineState state item rest = 
  case item of 
    Left info -> 
      let 
        nextState = { state | pos = info.pos, lines = info.lines, stack = rest }
      in 
        timelineStep nextState 
    Right info -> 
      let 
        nextState = { state | pos = info.pos, lines = info.lines, stack = rest }
      in 
        timelineStep nextState 
    Down info -> 
      let 
        nextState = { state | pos = info.pos, lines = info.lines, stack = rest }
      in 
        timelineStep nextState 
    Combine -> 
      let 
        (beam, depth) = state.pos 
        (b, d) = (beam - 1, depth - 1)
        pos = (b, d)
        countL = Dict.get (b - 1, d + 1) state.mem |> Maybe.withDefault 0 
        countR = Dict.get (b + 1, d + 1) state.mem |> Maybe.withDefault 0 
        nextMem = state.mem |> Dict.insert pos (countL + countR)
        seen = state.seen |> Set.insert (beam, depth - 1) 
      in 
        { state | mem = nextMem, pos = pos, stack = rest, seen = seen }
    Bubble -> 
      let 
        (beam, depth) = state.pos 
        pos = (beam, depth - 1)
        countD = Dict.get (beam, depth) state.mem |> Maybe.withDefault 0 
        nextMem = state.mem |> Dict.insert pos countD 
        seen = state.seen |> Set.insert pos 
      in 
        { state | mem = nextMem, pos = pos, stack = rest, seen = seen }

updateTimelineStep : Model -> Model
updateTimelineStep model = 
  case model.timelineState.stack of 
    [] -> { model | finished = True, paused = True }
    item :: rest -> 
      let
        state = updateTimelineState model.timelineState item rest 
      in 
        { model | timelineState = state }

updateStep : Model -> Model
updateStep model = 
  if model.quantum then updateTimelineStep model 
  else updateSplitStep model 

prevState : List State -> State 
prevState acc = 
  acc |> List.head |> Maybe.withDefault { count = 0, seen = Set.empty }

timelineCount : List State -> Dict Pos Int -> Int -> Int -> List (List Char) -> (Dict Pos Int, List State, Int) 
timelineCount acc mem beam depth lines = 
  let 
    prev = prevState acc 
  in 
    case Dict.get (beam, depth) mem of 
      Just cached ->
        let 
          st = { count = prev.count + cached, seen = prev.seen }
        in 
          (mem, st :: acc, cached) 
      Nothing -> 
        case lines of 
          [] -> 
            let 
              st = { count = prev.count + 1, seen = prev.seen }
            in 
              (mem, st :: acc, 1) 
          h :: t -> 
            let 
              indexes = 
                h |> List.indexedMap (\i -> \c -> (i, c)) 
                  |> List.filterMap (\(i, c) -> if c == '^' then Just i else Nothing)
            in 
              if indexes |> List.member beam then 
                let 
                  seen0 = prev.seen |> Set.insert (beam, depth) |> Set.insert (beam - 1, depth)
                  state0 = { count = prev.count, seen = seen0 }
                  acc0 = state0 :: acc
                  (memL, accL, countL) = timelineCount acc0 mem (beam - 1) (depth + 1) t 
                  prevL = prevState accL 
                  seen1 = prevL.seen |> Set.insert (beam + 1, depth)
                  state1 = { count = prevL.count, seen = seen1 }
                  acc1 = state1 :: accL
                  (memR, accR, countR) = timelineCount acc1 memL (beam + 1) (depth + 1) t 
                  prevR = prevState accR 
                  seen2 = prevR.seen 
                  state2 = { count = prevR.count, seen = seen2 }
                  acc2 = state2 :: accR
                  totalCount = countL + countR 
                  nextMem = memR |> Dict.insert (beam, depth) totalCount 
                in 
                  (nextMem, accR, totalCount) 
              else 
                let
                  seen0 = prev.seen |> Set.insert (beam, depth) 
                  state0 = { count = prev.count, seen = seen0 }
                  acc0 = state0 :: acc
                  (m, a, c) = timelineCount acc0 mem beam (depth + 1) t
                  -- afterState = { count = prev.count, seen = Set.empty }
                in 
                  (m, a, c)

compact : List State -> List State 
compact steps = 
  case steps of 
    a :: b :: rest -> 
      if Set.size b.seen == Set.size a.seen then 
        b :: compact rest 
      else 
        a :: compact (b :: rest)
    _ -> steps

timeline : Int -> List (List Char) -> List State 
timeline beam lines = 
  let 
    initState = { count = 0, seen = Set.empty |> Set.insert (beam, 0) }
    acc = [ initState ]
    mem = Dict.empty 
    depth = 0
    (_, result, _) = timelineCount acc mem beam depth lines 
  in 
    result |> List.reverse |> compact

splitCount : List State -> Set Int -> Int -> List (List Char) -> List State
splitCount acc beams depth lines = 
  case lines of 
    [] -> acc |> List.reverse 
    h :: t -> 
      let 
        indexes = 
          h |> List.indexedMap (\i -> \c -> (i, c)) 
            |> List.filterMap (\(i, c) -> if c == '^' then Just i else Nothing)
        collisions = beams |> Set.filter (\b -> indexes |> List.member b)
        uncollisions = Set.diff beams collisions
        splitBeams = Set.union (collisions |> Set.map (\b -> b - 1)) (collisions |> Set.map (\b -> b + 1))
        nextBeams = uncollisions |> Set.union splitBeams 
        seen = nextBeams |> Set.union collisions |> Set.map (\b -> (b, depth))
        count = acc |> List.head |> Maybe.map (\st -> st.count) |> Maybe.withDefault 0 
        state = { count = count + Set.size collisions, seen = seen }
      in 
        splitCount (state :: acc) nextBeams (depth + 1) t 

indexOf : Int -> a -> List a -> Maybe Int 
indexOf ix it lst =
  case lst of 
    [] -> Nothing 
    h :: t -> 
      if h == it then Just ix else indexOf (ix + 1) it t 

solve : Bool -> List (List Char) -> List State 
solve quantum lines = 
  case lines of 
    [] -> [] 
    h :: t -> 
      case indexOf 0 'S' h of 
        Nothing -> []
        Just ix -> 
          if quantum then 
            timeline ix lines 
          else 
            let 
              initState = { count = 0, seen = Set.empty |> Set.insert (ix, 0) }
              beams = Set.empty |> Set.insert ix 
            in 
              splitCount [ initState ] beams 0 lines 

updateTogglePlay : Model -> Model
updateTogglePlay model = 
  if model.finished then 
    let 
      m = (initModel model.quantum model.dataSource)
    in 
      updateTogglePlay { m | debug = "re-init" }
  else if model.paused then 
    { model | paused = False, debug = "un-paused" }
  else 
    { model | paused = True, debug = "paused" }

updateToggleQuantum : Model -> Model
updateToggleQuantum model = 
  initModel (not model.quantum) model.dataSource

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
    ToggleQuantum -> 
      (updateToggleQuantum model, Cmd.none)
    UseInput -> 
      (initModel model.quantum Input, Cmd.none)
    UseSample -> 
      (initModel model.quantum Sample, Cmd.none)

-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
  let 
    tickSub = if model.paused then Sub.none else Time.every model.tickInterval (\_ -> Tick)
  in 
    tickSub

-- VIEW

toCharElement : Set Pos -> Pos -> String -> Html Msg 
toCharElement seen pos symbol = 
  let
    cssClass =
      if Set.member pos seen then 
        if symbol == "^" || symbol == "S" then "draw-red adaptive" else "draw-green adaptive"
      else 
        "draw-empty adaptive"
    sym = 
      if symbol == "." && Set.member pos seen then 
        "|"
      else
        symbol  
  in
    Html.span [ Html.Attributes.class cssClass ]  [ Html.text sym ]

selectSymbol : Set Pos -> Pos -> String 
selectSymbol rolls pos = 
  if Set.member pos rolls then "@" else "."

view : Model -> Html Msg
view model =
  let
    vm = 
      if model.quantum then 
        { seen = model.timelineState.seen 
        , count = model.timelineState.count }
      else 
        { seen = model.splitState.seen 
        , count = model.splitState.count }

    nestedElements = 
      model.lines |> List.indexedMap (\y -> \line -> line |> List.indexedMap (\x -> \ch -> toCharElement vm.seen (x, y) (String.fromChar ch)))
    elements = nestedElements |> List.foldr (\a b -> List.append a (Html.br [] [] :: b)) []    
    textFontSize =
      case model.dataSource of 
        Sample -> "24px"
        Input -> "8px"  in 
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
              [ Html.div [] [Html.text "Advent of Code 2025" ]
              , Html.div [] [Html.text "Day 7: Laboratories" ] ] ]
      , Html.tr 
          []
          [ Html.td 
              [ Html.Attributes.align "center"
              , Html.Attributes.style "padding-bottom" "10px" ]
              [ Html.a 
                [ Html.Attributes.href "https://adventofcode.com/2025/day/7" ] 
                [ Html.text "https://adventofcode.com/2025/day/7" ]
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
                [ Html.text "Reset" ]
              , Html.button 
                [ Html.Attributes.style "width" "80px", onClick Slower ] 
                [ Html.text "Slower" ]
              , Html.button 
                [ Html.Attributes.style "width" "80px", onClick TogglePlay ] 
                [ if model.paused then Html.text "Play" else Html.text "Pause" ] 
              , Html.button 
                [ Html.Attributes.style "width" "80px", onClick Faster ] 
                [ Html.text "Faster" ]
              , Html.button 
                [ Html.Attributes.style "width" "80px", onClick Step ] 
                [ Html.text "Step" ]
            ] ]
      , Html.tr 
          []
          [ Html.td 
              [ Html.Attributes.align "center" ]
              [ Html.input 
                [ Html.Attributes.type_ "checkbox", onClick ToggleQuantum, Html.Attributes.checked model.quantum ] 
                []
              , Html.label [] [ Html.text " Quantum Manifold" ]
            ] ]
      , Html.tr 
          []
          [ Html.td 
              [ Html.Attributes.align "center"
              , Html.Attributes.style "font-family" "Courier New"
              , Html.Attributes.style "font-size" "16px"
              , Html.Attributes.style "padding-top" "10px" ] 
              [ 
                Html.div [] [ Html.text (String.fromInt vm.count) ]
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
                  Html.Attributes.align "center" 
                , Html.Attributes.style "max-width" "100%"
                ] elements
              ] ] 
      , Html.tr 
          []
          [ Html.td 
              [ Html.Attributes.align "center"
              , Html.Attributes.style "font-family" "Courier New"
              , Html.Attributes.style "font-size" "16px"
              , Html.Attributes.style "padding" "0px" ] 
              [ 
                -- Html.div [] [ Html.text (model.moves |> List.length |> String.fromInt ) ]
              -- , Html.div [] [ Html.text (String.fromInt model.position) ]
                -- Html.div [] [ Html.text model.debug ]
              -- , Html.div [] [ Html.text (String.fromInt (Set.size vm.seen))  ]
              ] ] 
              ]
